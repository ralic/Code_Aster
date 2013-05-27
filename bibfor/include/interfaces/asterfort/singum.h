        interface
          subroutine singum(nomail,ndim,nnoem,nelem,itype,xy)
            integer :: nelem
            integer :: nnoem
            character(len=8) :: nomail
            integer :: ndim
            integer :: itype(nelem)
            real(kind=8) :: xy(3,nnoem)
          end subroutine singum
        end interface
