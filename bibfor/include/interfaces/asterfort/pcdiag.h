        interface
          subroutine pcdiag(n,icpl,icpc,icpd)
            integer :: n
            integer :: icpl(0:n)
            integer(kind=4) :: icpc(*)
            integer :: icpd(n)
          end subroutine pcdiag
        end interface
