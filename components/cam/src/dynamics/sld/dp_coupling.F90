
!-------------------------------------------------------------------------------
! dynamics - physics coupling module
!-------------------------------------------------------------------------------
module dp_coupling

   use shr_kind_mod,      only: r8 => shr_kind_r8
   use ppgrid,            only: pcols, pver
   use pmgrid
   
   use phys_grid
   use physics_types,     only: physics_state, physics_tend
   use constituents,      only: pcnst
   use physconst,         only: cpair, gravit, rair, zvir, rairv
   use geopotential,      only: geopotential_t
   use check_energy,      only: check_energy_timestep_init
#if (defined SPMD)
   use spmd_dyn,          only: buf1, buf1win, buf2, buf2win, &
                                spmdbuf_siz, local_dp_map, &
                                block_buf_nrecs, chunk_buf_nrecs
   use mpishorthand,      only: mpicom
#endif
   use cam_abortutils,    only: endrun
   use perf_mod

   implicit none

!===============================================================================
CONTAINS
!===============================================================================

!===============================================================================
  subroutine d_p_coupling(ps, t3, u3, v3, q3, &
                          omga, phis, phys_state, phys_tend,  pbuf2d)
!------------------------------------------------------------------------------
! Coupler for converting dynamics output variables into physics input variables
!------------------------------------------------------------------------------
    use physconst,     only: cappa
    use constituents,  only: cnst_get_type_byind, qmin
    use physics_types, only: set_state_pdry
    use physics_buffer, only : physics_buffer_desc, pbuf_get_chunk
!------------------------------Arguments--------------------------------
    real(r8), intent(in) :: ps  (plon, beglat:endlat)            ! surface pressure
    real(r8), intent(in) :: t3  (plon, plev, beglat:endlat)      ! temperature
    real(r8), intent(in) :: u3  (plon, plev, beglat:endlat)      ! u-wind component
    real(r8), intent(in) :: v3  (plon, plev, beglat:endlat)      ! v-wind component
    real(r8), intent(in) :: q3  (plon, plev, pcnst, beglat:endlat) ! constituents
    real(r8), intent(in) :: omga(plon, plev, beglat:endlat)      ! vertical velocity
    real(r8), intent(in) :: phis(plon, beglat:endlat)            ! Surface geopotential
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)
    type(physics_state), intent(inout), dimension(begchunk:endchunk) :: phys_state
    type(physics_tend ), intent(inout), dimension(begchunk:endchunk) :: phys_tend
    
!
!---------------------------Local workspace-----------------------------
#if (! defined SPMD)
    real(r8) :: buf1(1), buf2(1)     ! transpose buffers
    integer  :: buf1win, buf2win     ! MPI-2 window ids
    integer  :: spmdbuf_siz = 0
    integer  :: block_buf_nrecs = 0
    integer  :: chunk_buf_nrecs = 0
    integer  :: mpicom = 0
    logical  :: local_dp_map=.true. 
#endif

    integer :: i,k,j,m,lchnk         ! indices
    integer :: ncol                  ! number of columns in current chunk
    integer :: lats(pcols)           ! array of latitude indices
    integer :: lons(pcols)           ! array of longitude indices
    integer :: tsize                 ! amount of data per grid point passed to physics
    integer :: bpter(plon,0:plev)    ! offsets into block buffer for packing data
    integer :: cpter(pcols,0:pver)   ! offsets into chunk buffer for unpacking data
    logical :: wetq(pcnst)           ! 'moist-type' constituent flag
    real(r8) :: rlat(pcols)          ! array of latitudes (radians)
    real(r8) :: rlon(pcols)          ! array of longitudes (radians)
    real(r8) :: zvirv(pcols,pver)    ! Local zvir array pointer

    type(physics_buffer_desc), pointer :: pbuf_chnk(:)

!-----------------------------------------------------------------------

! Determine which constituents are wet and which are dry
    do m=2,pcnst
       if (cnst_get_type_byind(m).eq.'wet') then  
          wetq(m) = .true.
       else
          wetq(m) = .false.
       endif
    enddo

!-----------------------------------------------------------------------
! Copy data from dynamics data structure to physics data structure
!-----------------------------------------------------------------------
    if (local_dp_map) then

!$OMP PARALLEL DO PRIVATE (LCHNK, NCOL, I, K, M, LONS, LATS)

       do lchnk = begchunk,endchunk
          ncol = phys_state(lchnk)%ncol
          call get_lon_all_p(lchnk, ncol, lons)
          call get_lat_all_p(lchnk, ncol, lats)

          do i=1,ncol
             phys_state(lchnk)%ps   (i)     = ps  (lons(i),lats(i))
             phys_state(lchnk)%phis (i)     = phis(lons(i),lats(i))
          end do

          do k=1,plev
             do i=1,ncol
                phys_state(lchnk)%t    (i,k)   = t3  (lons(i),k,lats(i))
                phys_state(lchnk)%u    (i,k)   = u3  (lons(i),k,lats(i))
                phys_state(lchnk)%v    (i,k)   = v3  (lons(i),k,lats(i))
                phys_state(lchnk)%omega(i,k)   = omga(lons(i),k,lats(i))
                phys_state(lchnk)%q(i,k,1)     = q3  (lons(i),k,1,lats(i))
             end do
          end do

! convert moist-type constituents (except specific humidity) from dry to moist basis

          do m=2,pcnst
             if (wetq(m)) then  
                do k=1,plev
                   do i=1,ncol
                      phys_state(lchnk)%q(i,k,m) = q3(lons(i),k,m,lats(i))*(1._r8 - q3(lons(i),k,1,lats(i)))
                   end do
                end do
             else
                do k=1,plev
                   do i=1,ncol
                      phys_state(lchnk)%q(i,k,m) = q3(lons(i),k,m,lats(i))
                   end do
                end do
             endif
          end do
          
       end do

    else

       tsize = 4 + pcnst

       if (tsize*max(block_buf_nrecs,chunk_buf_nrecs) > spmdbuf_siz) then
          call endrun ('p_d_coupling: communication buffers (spmdbuf_siz) too small')
       endif

!$OMP PARALLEL DO PRIVATE (J, BPTER, I, K, M)
       do j=beglat,endlat

          call block_to_chunk_send_pters(j,plon,plev+1,tsize,bpter)

!DIR$ CONCURRENT
          do i=1, plon

             buf1(bpter(i,0))   = ps  (i,j)
             buf1(bpter(i,0)+1) = phis(i,j)

!DIR$ CONCURRENT
             do k=1,plev

                buf1(bpter(i,k))   = t3  (i,k,j)
                buf1(bpter(i,k)+1) = u3  (i,k,j)
                buf1(bpter(i,k)+2) = v3  (i,k,j)
                buf1(bpter(i,k)+3) = omga(i,k,j)
                buf1(bpter(i,k)+4) = q3  (i,k,1,j)

                ! convert moist-type constituents (except specific humidity) from dry to moist basis

                do m=2,pcnst
                   if (wetq(m)) then  
                      buf1(bpter(i,k)+3+m) = q3(i,k,m,j)*(1._r8 - q3(i,k,1,j))
                   else
                      buf1(bpter(i,k)+3+m) = q3(i,k,m,j)
                   endif
                end do

             end do

          end do

       end do

       call t_barrierf ('sync_blk_to_chk', mpicom)
       call t_startf ('block_to_chunk')
       call transpose_block_to_chunk(tsize, buf1, buf2, buf2win)
       call t_stopf  ('block_to_chunk')

!$OMP PARALLEL DO PRIVATE (LCHNK, NCOL, CPTER, I, K, M)
       do lchnk = begchunk,endchunk
          ncol = phys_state(lchnk)%ncol

          call block_to_chunk_recv_pters(lchnk,pcols,pver+1,tsize,cpter)

          do i=1,ncol

             phys_state(lchnk)%ps   (i)     = buf2(cpter(i,0))
             phys_state(lchnk)%phis (i)     = buf2(cpter(i,0)+1)

             do k=1,plev

                phys_state(lchnk)%t    (i,k)   = buf2(cpter(i,k))
                phys_state(lchnk)%u    (i,k)   = buf2(cpter(i,k)+1)
                phys_state(lchnk)%v    (i,k)   = buf2(cpter(i,k)+2)
                phys_state(lchnk)%omega (i,k)   = buf2(cpter(i,k)+3)

                do m=1,pcnst
                   phys_state(lchnk)%q (i,k,m) = buf2(cpter(i,k)+3+m)
                end do

             end do

          end do

       end do

    endif

!-----------------------------------------------------------------------
! Fill auxilliary arrays in physics data structure
!-----------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE (LCHNK, NCOL, I, K, M, LONS, LATS, ZVIRV, pbuf_chnk)

    do lchnk = begchunk,endchunk
       ncol = phys_state(lchnk)%ncol

! pressure arrays
       call plevs0(ncol, pcols, pver, &
                   phys_state(lchnk)%ps,   phys_state(lchnk)%pint,    &
                   phys_state(lchnk)%pmid, phys_state(lchnk)%pdel)

! log(pressure) arrays and Exner function
       do k=1,pver+1
          do i=1,ncol
             phys_state(lchnk)%lnpint(i,k) = log(phys_state(lchnk)%pint(i,k))
          end do
       end do
       do k=1,pver
          do i=1,ncol
             phys_state(lchnk)%rpdel(i,k)  = 1._r8/phys_state(lchnk)%pdel(i,k)
             phys_state(lchnk)%lnpmid(i,k) = log(phys_state(lchnk)%pmid(i,k))
             phys_state(lchnk)%exner (i,k) = (phys_state(lchnk)%pint(i,pver+1) &
                                             / phys_state(lchnk)%pmid(i,k))**cappa
          end do
       end do

!-----------------------------------------------------------------------------------
!  Need to fill zvir_loc 2D variable to be compatible with geopotential_t interface
!-----------------------------------------------------------------------------------
       zvirv(:,:) = zvir

! Compute initial geopotential heights
       call geopotential_t (phys_state(lchnk)%lnpint, phys_state(lchnk)%lnpmid  , phys_state(lchnk)%pint  , &
                            phys_state(lchnk)%pmid  , phys_state(lchnk)%pdel    , phys_state(lchnk)%rpdel , &
                            phys_state(lchnk)%t     , phys_state(lchnk)%q(:,:,1), rairv(:,:,lchnk),  gravit, zvirv, &
                            phys_state(lchnk)%zi    , phys_state(lchnk)%zm      , ncol                )

! Compute initial dry static energy, include surface geopotential
       do k = 1, pver
          do i=1,ncol
             phys_state(lchnk)%s(i,k) = cpair*phys_state(lchnk)%t(i,k) &
                                      + gravit*phys_state(lchnk)%zm(i,k) + phys_state(lchnk)%phis(i)
          end do
       end do

       ! Dry pressure to use for this timestep
       call set_state_pdry(phys_state(lchnk))

       !
       ! Ensure tracers are all positive
       !
       call qneg3('D_P_COUPLING',lchnk  ,ncol    ,pcols   ,pver    , &
                  1, pcnst, qmin  ,phys_state(lchnk)%q)

       ! Compute energy and water integrals of input state
       pbuf_chnk => pbuf_get_chunk(pbuf2d, lchnk)
       call check_energy_timestep_init(phys_state(lchnk), phys_tend(lchnk), pbuf_chnk)

    end do

    return
  end subroutine d_p_coupling

!===============================================================================
  subroutine p_d_coupling(phys_state, phys_tend, t2, fu, fv, flx_net, q3)
!------------------------------------------------------------------------------
! Coupler for converting physics output variables into dynamics input variables
!------------------------------Arguments--------------------------------
    use constituents, only: cnst_get_type_byind

    type(physics_state),intent(in), dimension(begchunk:endchunk) :: phys_state
    type(physics_tend), intent(in), dimension(begchunk:endchunk) :: phys_tend

    real(r8), intent(out) :: t2(plon, plev, beglat:endlat)        ! temp tendency
    real(r8), intent(out) :: fu(plon, plev, beglat:endlat)        ! u wind tendency
    real(r8), intent(out) :: fv(plon, plev, beglat:endlat)        ! v wind tendency
    real(r8), intent(out) :: flx_net(plon,beglat:endlat)          ! net flux
    real(r8), intent(out) :: q3(plon, plev, pcnst, beglat:endlat) ! constituents
!
!---------------------------Local workspace-----------------------------
#if (! defined SPMD)
    real(r8) :: buf1(1), buf2(1)     ! transpose buffers
    integer  :: buf1win, buf2win     ! MPI-2 window ids
    integer  :: spmdbuf_siz = 0
    integer  :: block_buf_nrecs = 0
    integer  :: chunk_buf_nrecs = 0
    integer  :: mpicom = 0
    logical  :: local_dp_map=.true. 
#endif

    integer :: i,j,k,m,lchnk         ! indices
    integer :: ncol                  ! number of columns in current chunk
    integer :: lats(pcols)           ! array of latitude indices
    integer :: lons(pcols)           ! array of longitude indices
    integer :: tsize                 ! amount of data per grid point passed to physics
    integer :: bpter(plon,0:plev)    ! offsets into block buffer for packing data
    integer :: cpter(pcols,0:pver)   ! offsets into chunk buffer for unpacking data
    logical :: wetq(pcnst)           ! 'wet' constituent flag
!-----------------------------------------------------------------------

! Determine which constituents are wet and which are dry
    do m=2,pcnst
       if (cnst_get_type_byind(m).eq.'wet') then  
          wetq(m) = .true.
       else
          wetq(m) = .false.
       endif
    enddo

!-----------------------------------------------------------------------
! copy data from physics data structure to dynamics data structure
!-----------------------------------------------------------------------
    if (local_dp_map) then

!$OMP PARALLEL DO PRIVATE (LCHNK, NCOL, I, K, M, LONS, LATS)

      do lchnk = begchunk,endchunk
         ncol = get_ncols_p(lchnk)
         call get_lon_all_p(lchnk, ncol, lons)
         call get_lat_all_p(lchnk, ncol, lats)

         do k=1,plev
!DIR$ CONCURRENT
            do i=1,ncol
               t2(lons(i),k,lats(i))   = phys_tend(lchnk)%dTdt (i,k)
               fu(lons(i),k,lats(i))   = phys_tend(lchnk)%dudt (i,k)
               fv(lons(i),k,lats(i))   = phys_tend(lchnk)%dvdt (i,k)
               q3(lons(i),k,1,lats(i)) = phys_state(lchnk)%q(i,k,1)
            end do
         end do

!DIR$ CONCURRENT
         do i=1,ncol
            flx_net(lons(i),lats(i))   = phys_tend(lchnk)%flx_net(i)
         end do

         ! convert moist-type constituents (except specific humidity) from moist to dry basis

         do m=2,pcnst
            if (wetq(m)) then  
               do k=1,plev
!DIR$ CONCURRENT
                  do i=1,ncol
                     q3(lons(i),k,m,lats(i)) = phys_state(lchnk)%q(i,k,m) /     &
                          (1._r8 - phys_state(lchnk)%q(i,k,1))
                  end do
               end do
            else
               do k=1,plev
!DIR$ CONCURRENT
                  do i=1,ncol
                     q3(lons(i),k,m,lats(i)) = phys_state(lchnk)%q(i,k,m)
                  end do
               end do
            endif
         end do
      end do

    else

       tsize = 3 + pcnst

       if (tsize*max(block_buf_nrecs,chunk_buf_nrecs) > spmdbuf_siz) then
          call endrun ('d_p_coupling: communication buffers (spmdbuf_siz) too small')
       endif

!$OMP PARALLEL DO PRIVATE (LCHNK, NCOL, CPTER, I, K, M)
       do lchnk = begchunk,endchunk
          ncol = get_ncols_p(lchnk)

          call chunk_to_block_send_pters(lchnk,pcols,pver+1,tsize,cpter)

!DIR$ CONCURRENT
          do i=1,ncol

             buf2(cpter(i,0)) = phys_tend(lchnk)%flx_net(i)

!DIR$ CONCURRENT
             do k=1,plev

                buf2(cpter(i,k))   = phys_tend(lchnk)%dTdt (i,k)
                buf2(cpter(i,k)+1) = phys_tend(lchnk)%dudt (i,k)
                buf2(cpter(i,k)+2) = phys_tend(lchnk)%dvdt (i,k)
                buf2(cpter(i,k)+3) = phys_state(lchnk)%q(i,k,1)

                ! convert moist-type constituents (except specific humidity) from moist to dry basis

                do m=2,pcnst
                   if (wetq(m)) then  
                      buf2(cpter(i,k)+2+m) = phys_state(lchnk)%q(i,k,m) /     &
                           (1._r8 - phys_state(lchnk)%q(i,k,1))
                   else
                      buf2(cpter(i,k)+2+m) = phys_state(lchnk)%q(i,k,m)
                   endif
                end do

             end do

          end do

       end do

       call t_barrierf ('sync_chk_to_blk', mpicom)
       call t_startf ('chunk_to_block')
       call transpose_chunk_to_block(tsize, buf2, buf1, buf1win)
       call t_stopf  ('chunk_to_block')

!$OMP PARALLEL DO PRIVATE (J, BPTER, I, K, M)
       do j=beglat,endlat

          call chunk_to_block_recv_pters(j,plon,plev+1,tsize,bpter)

          do i=1, plon

             flx_net(i,j) = buf1(bpter(i,0))

             do k=1,plev

                t2(i,k,j) = buf1(bpter(i,k))
                fu(i,k,j) = buf1(bpter(i,k)+1)
                fv(i,k,j) = buf1(bpter(i,k)+2)

                do m=1,pcnst
                   q3(i,k,m,j) = buf1(bpter(i,k)+2+m)
                end do

             end do

          end do

       end do

    endif

    return
  end subroutine p_d_coupling
end module dp_coupling
