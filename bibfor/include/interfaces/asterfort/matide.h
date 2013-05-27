        interface
          subroutine matide(matz,nbcmp,licmp,modlag,tdiag,vdiag)
            integer :: nbcmp
            character(*) :: matz
            character(len=8) :: licmp(nbcmp)
            character(len=16) :: modlag
            character(len=8) :: tdiag
            real(kind=8) :: vdiag
          end subroutine matide
        end interface
