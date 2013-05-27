        interface
          subroutine nmgvdn(ndim,nno1,nno2,iu,ia)
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iu(ndim*nno1)
            integer :: ia(nno2)
          end subroutine nmgvdn
        end interface
