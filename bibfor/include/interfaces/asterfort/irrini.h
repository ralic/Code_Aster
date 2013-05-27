        interface
          subroutine irrini(fami,kpg,ksp,typess,essai,mod,nmat,materf,&
     &yd,deps,dy)
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: typess
            real(kind=8) :: essai
            character(len=8) :: mod
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: yd(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: dy(*)
          end subroutine irrini
        end interface
