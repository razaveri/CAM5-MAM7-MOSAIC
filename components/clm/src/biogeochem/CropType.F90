module CropType

#include "shr_assert.h"

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing variables needed for the crop model
  !
  ! TODO(wjs, 2014-08-05) Move more crop-specific variables into here - many are
  ! currently in CNVegStateType
  !
  ! !USES:
  use shr_kind_mod        , only : r8 => shr_kind_r8
  use shr_log_mod         , only : errMsg => shr_log_errMsg
  use spmdMod             , only : masterproc
  use abortutils          , only : endrun
  use decompMod           , only : bounds_type
  use clm_varcon          , only : spval
  use clm_varctl          , only : iulog, use_crop
  !
  ! !PUBLIC TYPES:
  implicit none
  private
  !
  ! !PUBLIC DATA TYPES:
  !
  ! Crop state variables structure
  type, public :: crop_type

     ! Note that cropplant and harvdate could be 2D to facilitate rotation
     logical , pointer :: croplive_patch          (:)   ! patch Flag, true if planted, not harvested
     logical , pointer :: cropplant_patch         (:)   ! patch Flag, true if planted
     integer , pointer :: harvdate_patch          (:)   ! patch harvest date
     real(r8), pointer :: gddplant_patch          (:)   ! patch accum gdd past planting date for crop       (ddays)
     real(r8), pointer :: gddtsoi_patch           (:)   ! patch growing degree-days from planting (top two soil layers) (ddays)
     real(r8), pointer :: vf_patch                (:)   ! patch vernalization factor for cereal
     integer           :: CropRestYear                  ! restart year from initial conditions file - increment as time elapses

   contains
     ! Public routines
     procedure, public  :: Init
     procedure, public  :: InitAccBuffer
     procedure, public  :: InitAccVars
     procedure, public  :: Restart

     ! NOTE(wjs, 2014-09-29) need to rename this from UpdateAccVars to CropUpdateAccVars
     ! to prevent cryptic error messages with pgi (v. 13.9 on yellowstone)
     ! This is probably related to this bug
     ! <http://www.pgroup.com/userforum/viewtopic.php?t=4285>, which was fixed in pgi 14.7.
     procedure, public  :: CropUpdateAccVars

     procedure, public  :: CropRestIncYear

     ! Private routines
     procedure, private :: InitAllocate 
     procedure, private :: InitHistory
     procedure, private, nopass :: checkDates

  end type crop_type

  !------------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! !ARGUMENTS:
    class(crop_type) , intent(inout) :: this
    type(bounds_type), intent(in)    :: bounds
    !
    ! !LOCAL VARIABLES:
    
    character(len=*), parameter :: subname = 'Init'
    !-----------------------------------------------------------------------
    
    call this%InitAllocate(bounds)

    if (use_crop) then
       call this%InitHistory(bounds)
    end if

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine InitAllocate(this, bounds)
    !
    ! !ARGUMENTS:
    class(crop_type) , intent(inout) :: this
    type(bounds_type), intent(in)    :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    
    character(len=*), parameter :: subname = 'InitAllocate'
    !-----------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp

    allocate(this%croplive_patch (begp:endp)) ; this%croplive_patch (:) = .false.
    allocate(this%cropplant_patch(begp:endp)) ; this%cropplant_patch(:) = .false.
    allocate(this%harvdate_patch (begp:endp)) ; this%harvdate_patch (:) = huge(1) 
    allocate(this%gddplant_patch (begp:endp)) ; this%gddplant_patch (:) = spval
    allocate(this%gddtsoi_patch  (begp:endp)) ; this%gddtsoi_patch  (:) = spval
    allocate(this%vf_patch       (begp:endp)) ; this%vf_patch       (:) = 0.0_r8
    
    this%CropRestYear = 0

  end subroutine InitAllocate

  !-----------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! !USES:
    use histFileMod    , only : hist_addfld1d
    !
    ! !ARGUMENTS:
    class(crop_type),  intent(inout) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    
    character(len=*), parameter :: subname = 'InitHistory'
    !-----------------------------------------------------------------------
    
    begp = bounds%begp; endp = bounds%endp

    this%gddplant_patch(begp:endp) = spval
    call hist_addfld1d (fname='GDDPLANT', units='ddays', &
         avgflag='A', long_name='Accumulated growing degree days past planting date for crop', &
         ptr_patch=this%gddplant_patch, default='inactive')

    this%gddtsoi_patch(begp:endp) = spval
    call hist_addfld1d (fname='GDDTSOI', units='ddays', &
         avgflag='A', long_name='Growing degree-days from planting (top two soil layers)', &
         ptr_patch=this%gddtsoi_patch, default='inactive')

  end subroutine InitHistory


    !-----------------------------------------------------------------------
  subroutine InitAccBuffer (this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize accumulation buffer for all required module accumulated fields
    ! This routine set defaults values that are then overwritten by the
    ! restart file for restart or branch runs
    ! Each interval and accumulation type is unique to each field processed.
    ! Routine [initAccBuffer] defines the fields to be processed
    ! and the type of accumulation. 
    ! Routine [updateAccVars] does the actual accumulation for a given field.
    ! Fields are accumulated by calls to subroutine [update_accum_field]. 
    ! To accumulate a field, it must first be defined in subroutine [initAccVars] 
    ! and then accumulated by calls to [updateAccVars].
    !
    ! Should only be called if use_crop is true
    !
    ! !USES 
    use accumulMod       , only : init_accum_field
    !
    ! !ARGUMENTS:
    class(crop_type) , intent(in) :: this
    type(bounds_type), intent(in) :: bounds  

    !
    ! !LOCAL VARIABLES:
    integer, parameter :: not_used = huge(1)

    !---------------------------------------------------------------------

    call init_accum_field (name='GDDPLANT', units='K', &
         desc='growing degree-days from planting', accum_type='runaccum', accum_period=not_used,  &
         subgrid_type='pft', numlev=1, init_value=0._r8)

    call init_accum_field (name='GDDTSOI', units='K', &
         desc='growing degree-days from planting (top two soil layers)', accum_type='runaccum', accum_period=not_used,  &
         subgrid_type='pft', numlev=1, init_value=0._r8)

  end subroutine InitAccBuffer

  !-----------------------------------------------------------------------
  subroutine InitAccVars(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module variables that are associated with
    ! time accumulated fields. This routine is called for both an initial run
    ! and a restart run (and must therefore must be called after the restart file 
    ! is read in and the accumulation buffer is obtained)
    !
    ! !USES:
    use accumulMod       , only : extract_accum_field
    use clm_time_manager , only : get_nstep
    !
    ! !ARGUMENTS:
    class(crop_type),  intent(inout) :: this
    type(bounds_type), intent(in)    :: bounds
    !
    ! !LOCAL VARIABLES:
    integer  :: begp, endp
    integer  :: nstep
    integer  :: ier
    real(r8), pointer :: rbufslp(:)  ! temporary
    
    character(len=*), parameter :: subname = 'InitAccVars'
    !-----------------------------------------------------------------------
    
    begp = bounds%begp; endp = bounds%endp

    ! Allocate needed dynamic memory for single level patch field
    allocate(rbufslp(begp:endp), stat=ier)
    if (ier/=0) then
       write(iulog,*)' in '
       call endrun(msg=" allocation error for rbufslp"//&
            errMsg(__FILE__, __LINE__))
    endif

    nstep = get_nstep()

    call extract_accum_field ('GDDPLANT', rbufslp, nstep) 
    this%gddplant_patch(begp:endp) = rbufslp(begp:endp)

    call extract_accum_field ('GDDTSOI', rbufslp, nstep) 
    this%gddtsoi_patch(begp:endp)  = rbufslp(begp:endp)

    deallocate(rbufslp)

  end subroutine InitAccVars

  !-----------------------------------------------------------------------
  subroutine Restart(this, bounds, ncid, flag)
    !
    ! !USES:
    use restUtilMod
    use ncdio_pio
    !
    ! !ARGUMENTS:
    class(crop_type), intent(inout)  :: this
    type(bounds_type), intent(in)    :: bounds 
    type(file_desc_t), intent(inout) :: ncid   
    character(len=*) , intent(in)    :: flag   
    !
    ! !LOCAL VARIABLES:
    integer, pointer :: temp1d(:) ! temporary
    integer :: p
    logical :: readvar   ! determine if variable is on initial file

    character(len=*), parameter :: subname = 'Restart'
    !-----------------------------------------------------------------------

    if (use_crop) then
       allocate(temp1d(bounds%begp:bounds%endp))
       if (flag == 'write') then 
          do p= bounds%begp,bounds%endp
             if (this%croplive_patch(p)) then
                temp1d(p) = 1
             else
                temp1d(p) = 0
             end if
          end do
       end if
       call restartvar(ncid=ncid, flag=flag,  varname='croplive', xtype=ncd_log,  &
            dim1name='pft', &
            long_name='Flag that crop is alive, but not harvested', &
            interpinic_flag='interp', readvar=readvar, data=temp1d)
       if (flag == 'read') then 
          do p= bounds%begp,bounds%endp
             if (temp1d(p) == 1) then
                this%croplive_patch(p) = .true.
             else
                this%croplive_patch(p) = .false.
             end if
          end do
       end if
       deallocate(temp1d)

       allocate(temp1d(bounds%begp:bounds%endp))
       if (flag == 'write') then 
          do p= bounds%begp,bounds%endp
             if (this%cropplant_patch(p)) then
                temp1d(p) = 1
             else
                temp1d(p) = 0
             end if
          end do
       end if
       call restartvar(ncid=ncid, flag=flag,  varname='cropplant', xtype=ncd_log,  &
            dim1name='pft', &
            long_name='Flag that crop is planted, but not harvested' , &
            interpinic_flag='interp', readvar=readvar, data=temp1d)
       if (flag == 'read') then 
          do p= bounds%begp,bounds%endp
             if (temp1d(p) == 1) then
                this%cropplant_patch(p) = .true.
             else
                this%cropplant_patch(p) = .false.
             end if
          end do
       end if
       deallocate(temp1d)

       call restartvar(ncid=ncid, flag=flag,  varname='harvdate', xtype=ncd_int,  &
            dim1name='pft', long_name='harvest date', units='jday', nvalid_range=(/1,366/), & 
            interpinic_flag='interp', readvar=readvar, data=this%harvdate_patch)

       call restartvar(ncid=ncid, flag=flag,  varname='vf', xtype=ncd_double,  &
            dim1name='pft', long_name='vernalization factor', units='', &
            interpinic_flag='interp', readvar=readvar, data=this%vf_patch)

       call restartvar(ncid=ncid, flag=flag,  varname='restyear', xtype=ncd_int,  &
            long_name='Number of years prognostic crop ran', units="years", &
            interpinic_flag='copy', readvar=readvar, data=this%CropRestYear)
       if (flag=='read' .and. readvar)  then
          call this%checkDates( )
       end if
    end if

  end subroutine Restart


  !-----------------------------------------------------------------------
  subroutine CropUpdateAccVars(this, bounds, t_ref2m_patch, t_soisno_col)
    !
    ! !DESCRIPTION:
    ! Update accumulated variables. Should be called every time step.
    ! Should only be called if use_crop is true.
    !
    ! !USES:
    use accumulMod       , only : update_accum_field, extract_accum_field, accumResetVal
    use shr_const_mod    , only : SHR_CONST_CDAY, SHR_CONST_TKFRZ
    use clm_time_manager , only : get_step_size, get_nstep
    use clm_varpar       , only : nlevsno, nlevgrnd
    use pftconMod        , only : nwwheat, nirrig_wwheat, pftcon 
    use ColumnType       , only : col
    use PatchType        , only : patch
    !
    ! !ARGUMENTS:
    class(crop_type)       , intent(inout) :: this
    type(bounds_type)      , intent(in)    :: bounds
    real(r8)               , intent(in)    :: t_ref2m_patch( bounds%begp:)
    real(r8)               , intent(inout) :: t_soisno_col(bounds%begc:, -nlevsno+1:)
    !
    ! !LOCAL VARIABLES:
    integer :: p,c   ! indices
    integer :: ivt   ! vegetation type
    integer :: dtime ! timestep size [seconds]
    integer :: nstep ! timestep number
    integer :: ier   ! error status
    integer :: begp, endp
    integer :: begc, endc
    real(r8), pointer :: rbufslp(:)      ! temporary single level - patch level
    character(len=*), parameter :: subname = 'CropUpdateAccVars'
    !-----------------------------------------------------------------------
    
    begp = bounds%begp; endp = bounds%endp
    begc = bounds%begc; endc = bounds%endc

    ! Enforce expected array sizes
    SHR_ASSERT_ALL((ubound(t_ref2m_patch)  == (/endp/))          , errMsg(__FILE__, __LINE__))
    SHR_ASSERT_ALL((ubound(t_soisno_col)   == (/endc,nlevgrnd/)) , errMsg(__FILE__, __LINE__))

    dtime = get_step_size()
    nstep = get_nstep()

    ! Allocate needed dynamic memory for single level patch field

    allocate(rbufslp(begp:endp), stat=ier)
    if (ier/=0) then
       write(iulog,*)'update_accum_hist allocation error for rbuf1dp'
       call endrun(msg=errMsg(__FILE__, __LINE__))
    endif

    ! Accumulate and extract GDDPLANT
    
    do p = begp,endp
       if (this%croplive_patch(p)) then ! relative to planting date
          ivt = patch%itype(p)
          rbufslp(p) = max(0._r8, min(pftcon%mxtmp(ivt), &
               t_ref2m_patch(p)-(SHR_CONST_TKFRZ + pftcon%baset(ivt)))) &
               * dtime/SHR_CONST_CDAY
          if (ivt == nwwheat .or. ivt == nirrig_wwheat) then
             rbufslp(p) = rbufslp(p) * this%vf_patch(p)
          end if
       else
          rbufslp(p) = accumResetVal
       end if
    end do
    call update_accum_field  ('GDDPLANT', rbufslp, nstep)
    call extract_accum_field ('GDDPLANT', this%gddplant_patch, nstep)

    ! Accumulate and extract GDDTSOI
    ! In agroibis this variable is calculated
    ! to 0.05 m, so here we use the top two soil layers

    do p = begp,endp
       if (this%croplive_patch(p)) then ! relative to planting date
          ivt = patch%itype(p)
          c   = patch%column(p)
          rbufslp(p) = max(0._r8, min(pftcon%mxtmp(ivt), &
               ((t_soisno_col(c,1)*col%dz(c,1) + &
               t_soisno_col(c,2)*col%dz(c,2))/(col%dz(c,1)+col%dz(c,2))) - &
               (SHR_CONST_TKFRZ + pftcon%baset(ivt)))) * dtime/SHR_CONST_CDAY
          if (ivt == nwwheat .or. ivt == nwwheat) then
             rbufslp(p) = rbufslp(p) * this%vf_patch(p)
          end if
       else
          rbufslp(p) = accumResetVal
       end if
    end do
    call update_accum_field  ('GDDTSOI', rbufslp, nstep)
    call extract_accum_field ('GDDTSOI', this%gddtsoi_patch, nstep)

    deallocate(rbufslp)

  end subroutine CropUpdateAccVars

  !-----------------------------------------------------------------------
  subroutine CropRestIncYear (this)
    !
    ! !DESCRIPTION: 
    ! Increment the crop restart year, if appropriate
    !
    ! This routine should be called every time step, but only once for all clumps (to
    ! avoid inadvertently updating nyrs multiple times)
    !
    ! !USES:
    use clm_time_manager , only : get_curr_date, is_first_step
    !
    ! !ARGUMENTS:
    class(crop_type) :: this
    !
    ! !LOCAL VARIABLES:
    integer kyr   ! current year
    integer kmo   ! month of year  (1, ..., 12)
    integer kda   ! day of month   (1, ..., 31)
    integer mcsec ! seconds of day (0, ..., seconds/day)
    !-----------------------------------------------------------------------

    ! Update restyear only when running with prognostic crop
    if ( use_crop )then

       ! Update restyear when it's the start of a new year - but don't do that at the
       ! very start of the run
       call get_curr_date (   kyr, kmo, kda, mcsec)
       if ((kmo == 1 .and. kda == 1 .and. mcsec == 0) .and. .not. is_first_step()) then
          this%CropRestYear = this%CropRestYear + 1
       end if

    end if

  end subroutine CropRestIncYear

  !-----------------------------------------------------------------------
  subroutine checkDates( )
    !
    ! !DESCRIPTION: 
    ! Make sure the dates are compatible. The date given to startup the model
    ! and the date on the restart file must be the same although years can be
    ! different. The dates need to be checked when the restart file is being
    ! read in for a startup or branch case (they are NOT allowed to be different
    ! for a restart case).
    !
    ! For the prognostic crop model the date of planting is tracked and growing
    ! degree days is tracked (with a 20 year mean) -- so shifting the start dates
    ! messes up these bits of saved information.
    !
    ! !ARGUMENTS:
    use clm_time_manager, only : get_driver_start_ymd, get_start_date
    use clm_varctl      , only : iulog
    use clm_varctl      , only : nsrest, nsrBranch, nsrStartup
    !
    ! !LOCAL VARIABLES:
    integer :: stymd       ! Start date YYYYMMDD from driver
    integer :: styr        ! Start year from driver
    integer :: stmon_day   ! Start date MMDD from driver
    integer :: rsmon_day   ! Restart date MMDD from restart file
    integer :: rsyr        ! Restart year from restart file
    integer :: rsmon       ! Restart month from restart file
    integer :: rsday       ! Restart day from restart file
    integer :: tod         ! Restart time of day from restart file
    character(len=*), parameter :: formDate = '(A,i4.4,"/",i2.2,"/",i2.2)' ! log output format
    character(len=32) :: subname = 'CropRest::checkDates'
    !-----------------------------------------------------------------------
    !
    ! If branch or startup make sure the startdate is compatible with the date
    ! on the restart file.
    !
    if ( nsrest == nsrBranch .or. nsrest == nsrStartup )then
       stymd       = get_driver_start_ymd()
       styr        = stymd / 10000
       stmon_day   = stymd - styr*10000
       call get_start_date( rsyr, rsmon, rsday, tod )
       rsmon_day = rsmon*100 + rsday
       if ( masterproc ) &
            write(iulog,formDate) 'Date on the restart file is: ', rsyr, rsmon, rsday
       if ( stmon_day /= rsmon_day )then
          write(iulog,formDate) 'Start date is: ', styr, stmon_day/100, &
               (stmon_day - stmon_day/100)
          call endrun(msg=' ERROR: For prognostic crop to work correctly, the start date (month and day)'// &
               ' and the date on the restart file needs to match (years can be different)'//&
               errMsg(__FILE__, __LINE__))
       end if
    end if

  end subroutine checkDates

end module CropType

