        interface
          subroutine dneigh(rnorm,n,h,ldh,ritzr,ritzi,bounds,q,ldq,&
     &workl,ierr)
            integer :: ldq
            integer :: ldh
            integer :: n
            real(kind=8) :: rnorm
            real(kind=8) :: h(ldh,n)
            real(kind=8) :: ritzr(n)
            real(kind=8) :: ritzi(n)
            real(kind=8) :: bounds(n)
            real(kind=8) :: q(ldq,n)
            real(kind=8) :: workl(n*(n+3))
            integer :: ierr
          end subroutine dneigh
        end interface
