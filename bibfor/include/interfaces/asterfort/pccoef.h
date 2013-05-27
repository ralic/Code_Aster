        interface
          subroutine pccoef(n,in,ip,ac,icpl,icpc,acpc,cx)
            integer :: n
            integer :: in(n)
            integer(kind=4) :: ip(*)
            real(kind=8) :: ac(*)
            integer :: icpl(0:n)
            integer(kind=4) :: icpc(*)
            real(kind=8) :: acpc(*)
            real(kind=8) :: cx(n)
          end subroutine pccoef
        end interface
