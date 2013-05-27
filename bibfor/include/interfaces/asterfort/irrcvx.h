        interface
          subroutine irrcvx(fami,kpg,ksp,nmat,mater,sig,vin,seuil)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: seuil
          end subroutine irrcvx
        end interface
