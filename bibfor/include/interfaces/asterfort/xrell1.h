        interface
          subroutine xrell1(tabnoz,ndim,nar,pickno,nbpino,nliseq)
            integer :: nbpino
            integer :: nar
            integer :: ndim
            integer :: tabnoz(3,nar)
            integer :: pickno(nbpino)
            character(len=19) :: nliseq
          end subroutine xrell1
        end interface
