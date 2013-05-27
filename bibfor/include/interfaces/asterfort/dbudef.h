        interface
          subroutine dbudef(depl,b,d,nbsig,nbinco,sigma)
            integer :: nbsig
            real(kind=8) :: depl(1)
            real(kind=8) :: b(nbsig,1)
            real(kind=8) :: d(nbsig,1)
            integer :: nbinco
            real(kind=8) :: sigma(1)
          end subroutine dbudef
        end interface
