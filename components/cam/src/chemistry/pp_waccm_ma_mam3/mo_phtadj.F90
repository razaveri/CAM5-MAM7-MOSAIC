




      module mo_phtadj

      private
      public :: phtadj

      contains

      subroutine phtadj( p_rate, inv, m, ncol )

      use chem_mods, only : nfs, phtcnt
      use shr_kind_mod, only : r8 => shr_kind_r8
      use ppgrid, only : pver

      implicit none

!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in) :: inv(:,:,:)
      real(r8), intent(in) :: m(:,:)
      real(r8), intent(inout) :: p_rate(:,:,:)

!--------------------------------------------------------------------
! ... local variables
!--------------------------------------------------------------------
      integer :: k
      real(r8) :: im(ncol)

      do k = 1,pver
         im(:ncol) = 1._r8 / m(:ncol,k)
         p_rate(:,k, 71) = p_rate(:,k, 71) * inv(:,k, 2) * im(:)
         p_rate(:,k, 75) = p_rate(:,k, 75) * inv(:,k, 2) * im(:)
         p_rate(:,k, 76) = p_rate(:,k, 76) * inv(:,k, 2) * im(:)
         p_rate(:,k, 78) = p_rate(:,k, 78) * inv(:,k, 2) * im(:)
         p_rate(:,k, 83) = p_rate(:,k, 83) * inv(:,k, 2) * im(:)
         p_rate(:,k, 87) = p_rate(:,k, 87) * inv(:,k, 2) * im(:)
         p_rate(:,k, 88) = p_rate(:,k, 88) * inv(:,k, 2) * im(:)
         p_rate(:,k, 90) = p_rate(:,k, 90) * inv(:,k, 2) * im(:)
      end do

      end subroutine phtadj

      end module mo_phtadj
