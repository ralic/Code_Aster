        interface
          subroutine xmrept(jcesd,jcesv,jcesl,izone,ndim,defico,geom,&
     &statue,mmait,amait,nmait)
            integer :: jcesd(10)
            integer :: jcesv(10)
            integer :: jcesl(10)
            integer :: izone
            integer :: ndim
            character(len=24) :: defico
            real(kind=8) :: geom(3)
            integer :: statue
            integer :: mmait
            integer :: amait
            integer :: nmait
          end subroutine xmrept
        end interface
