        interface
          subroutine lceib1(fami,kpg,ksp,imate,compor,ndim,epsm,sref,&
     &sechm,hydrm,t,lambda,deuxmu,epsthe,kdess,bendo,gamma,seuil)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: imate
            character(len=16) :: compor(*)
            integer :: ndim
            real(kind=8) :: epsm(6)
            real(kind=8) :: sref
            real(kind=8) :: sechm
            real(kind=8) :: hydrm
            integer :: t(3,3)
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: epsthe(2)
            real(kind=8) :: kdess
            real(kind=8) :: bendo
            real(kind=8) :: gamma
            real(kind=8) :: seuil
          end subroutine lceib1
        end interface
