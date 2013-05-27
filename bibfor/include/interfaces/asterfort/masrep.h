        interface
          subroutine masrep(noma,ioc,rigi,lvale,nbgr,ligrma,nbno,&
     &tabnoe,rignoe,rigto,ndim)
            integer :: nbgr
            character(len=8) :: noma
            integer :: ioc
            real(kind=8) :: rigi(6)
            logical :: lvale
            character(len=24) :: ligrma(nbgr)
            integer :: nbno
            character(len=8) :: tabnoe(*)
            real(kind=8) :: rignoe(*)
            real(kind=8) :: rigto(*)
            integer :: ndim
          end subroutine masrep
        end interface
