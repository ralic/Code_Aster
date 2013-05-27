        interface
          subroutine btdbpr(b,d,jacob,nbsig,nbinco,btdb)
            integer :: nbsig
            real(kind=8) :: b(nbsig,1)
            real(kind=8) :: d(nbsig,1)
            real(kind=8) :: jacob
            integer :: nbinco
            real(kind=8) :: btdb(81,1)
          end subroutine btdbpr
        end interface
