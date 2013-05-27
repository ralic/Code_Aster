        interface
          subroutine rairep(noma,ioc,km,rigi,nbgr,ligrma,nbno,tabnoe,&
     &rignoe,rigto,amoto,rirot,ndim)
            integer :: nbgr
            character(len=8) :: noma
            integer :: ioc
            character(len=8) :: km
            real(kind=8) :: rigi(6)
            character(len=24) :: ligrma(nbgr)
            integer :: nbno
            character(len=8) :: tabnoe(*)
            real(kind=8) :: rignoe(*)
            real(kind=8) :: rigto(*)
            real(kind=8) :: amoto(*)
            real(kind=8) :: rirot(3)
            integer :: ndim
          end subroutine rairep
        end interface
