        interface
          subroutine rcevol(typtab,nommat,symax,nbopt,option)
            character(len=16) :: typtab
            character(len=8) :: nommat
            real(kind=8) :: symax
            integer :: nbopt
            character(len=16) :: option(*)
          end subroutine rcevol
        end interface
