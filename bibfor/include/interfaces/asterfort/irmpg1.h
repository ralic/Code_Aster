        interface
          subroutine irmpg1(nofimd,nomfpg,nbnoto,nbrepg,nbsp,ndim,&
     &typgeo,refcoo,gscoo,wg,raux1,raux2,raux3,nolopg,nomasu,codret)
            character(*) :: nofimd
            character(len=16) :: nomfpg
            integer :: nbnoto
            integer :: nbrepg
            integer :: nbsp
            integer :: ndim
            integer :: typgeo
            real(kind=8) :: refcoo(*)
            real(kind=8) :: gscoo(*)
            real(kind=8) :: wg(*)
            real(kind=8) :: raux1(*)
            real(kind=8) :: raux2(*)
            real(kind=8) :: raux3(*)
            character(*) :: nolopg
            character(*) :: nomasu
            integer :: codret
          end subroutine irmpg1
        end interface
