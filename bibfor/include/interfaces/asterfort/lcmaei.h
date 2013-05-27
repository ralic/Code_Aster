        interface
          subroutine lcmaei(fami,kpg,ksp,poum,nmater,imat,necris,&
     &necoul,nbval,valres,nmat,itbint,nfs,nsg,hsri,ifa,nomfam,nbsys)
            integer :: nsg
            integer :: nmat
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            character(len=16) :: nmater
            integer :: imat
            character(len=16) :: necris
            character(len=16) :: necoul
            integer :: nbval
            real(kind=8) :: valres(nmat)
            integer :: itbint
            integer :: nfs
            real(kind=8) :: hsri(nsg,nsg)
            integer :: ifa
            character(len=16) :: nomfam
            integer :: nbsys
          end subroutine lcmaei
        end interface
