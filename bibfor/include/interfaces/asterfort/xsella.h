        interface
          subroutine xsella(fiss,nbno,narz,tabnoz,pickno,nbpino)
            integer :: narz
            character(len=8) :: fiss
            integer :: nbno
            integer :: tabnoz(3,narz)
            integer :: pickno(narz)
            integer :: nbpino
          end subroutine xsella
        end interface
