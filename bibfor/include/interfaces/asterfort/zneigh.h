        interface
          subroutine zneigh(rnorm,n,h,ldh,ritz,bounds,q,ldq,workl,&
     &rwork,ierr)
            integer :: ldq
            integer :: ldh
            integer :: n
            real(kind=8) :: rnorm
            complex(kind=8) :: h(ldh,n)
            complex(kind=8) :: ritz(n)
            complex(kind=8) :: bounds(n)
            complex(kind=8) :: q(ldq,n)
            complex(kind=8) :: workl(n*(n+3))
            real(kind=8) :: rwork(n)
            integer :: ierr
          end subroutine zneigh
        end interface
