        interface
          subroutine prfcur(vec1,nbn,vec2,nbp,interp,prolgd)
            integer :: nbp
            integer :: nbn
            integer :: vec1(nbn)
            real(kind=8) :: vec2(nbp)
            character(len=8) :: interp
            character(len=2) :: prolgd
          end subroutine prfcur
        end interface
