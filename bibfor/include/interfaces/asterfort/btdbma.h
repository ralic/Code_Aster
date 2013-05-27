        interface
          subroutine btdbma(b,d,jacob,nbsig,nbinco,btdb)
            integer :: nbinco
            integer :: nbsig
            real(kind=8) :: b(nbsig,nbinco)
            real(kind=8) :: d(nbsig,nbsig)
            real(kind=8) :: jacob
            real(kind=8) :: btdb(nbinco,nbinco)
          end subroutine btdbma
        end interface
