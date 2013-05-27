        interface
          subroutine b1tdb2(b1,b2,d,jacob,nbsig,nbinco,btdb)
            integer :: nbinco
            integer :: nbsig
            real(kind=8) :: b1(nbsig,nbinco)
            real(kind=8) :: b2(nbsig,nbinco)
            real(kind=8) :: d(nbsig,nbsig)
            real(kind=8) :: jacob
            real(kind=8) :: btdb(nbinco,nbinco)
          end subroutine b1tdb2
        end interface
