        interface
          subroutine rslcvx(fami,kpg,ksp,imat,nmat,mater,sig,vin,seuil&
     &)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(3)
            real(kind=8) :: seuil
          end subroutine rslcvx
        end interface
