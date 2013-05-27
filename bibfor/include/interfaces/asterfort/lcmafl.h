        interface
          subroutine lcmafl(fami,kpg,ksp,poum,nmater,imat,necoul,nbval&
     &,valres,nmat,itbint,nfs,nsg,hsri,nbsys)
            integer :: nsg
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            character(len=16) :: nmater
            integer :: imat
            character(len=16) :: necoul
            integer :: nbval
            real(kind=8) :: valres(nmat)
            integer :: itbint
            integer :: nfs
            real(kind=8) :: hsri(nsg,nsg)
            integer :: nbsys
          end subroutine lcmafl
        end interface
