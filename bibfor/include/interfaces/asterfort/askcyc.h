        interface
          subroutine askcyc(craid,ndim,soumat,beta,ni,nj,na,axok,liax,&
     &nbliax,libid)
            integer :: nbliax
            complex(kind=8) :: craid(*)
            integer :: ndim
            character(len=24) :: soumat
            real(kind=8) :: beta
            integer :: ni
            integer :: nj
            integer :: na
            logical :: axok
            integer :: liax(nbliax)
            integer :: libid(*)
          end subroutine askcyc
        end interface
