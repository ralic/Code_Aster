        interface
          subroutine chauxi(ndim,mu,ka,r,t,invp,lcour,courb,du1dm,&
     &du2dm,du3dm,u1l,u2l,u3l)
            integer :: ndim
            real(kind=8) :: mu
            real(kind=8) :: ka
            real(kind=8) :: r
            real(kind=8) :: t
            real(kind=8) :: invp(3,3)
            logical :: lcour
            real(kind=8) :: courb(3,3,3)
            real(kind=8) :: du1dm(3,3)
            real(kind=8) :: du2dm(3,3)
            real(kind=8) :: du3dm(3,3)
            real(kind=8) :: u1l(3)
            real(kind=8) :: u2l(3)
            real(kind=8) :: u3l(3)
          end subroutine chauxi
        end interface
