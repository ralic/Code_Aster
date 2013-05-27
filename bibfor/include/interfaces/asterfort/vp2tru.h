        interface
          subroutine vp2tru(method,ty,alpha,beta,signes,a,nbvect,w,z,&
     &wk,mxiter,ier,nitqr)
            integer :: nbvect
            character(len=8) :: method
            character(len=1) :: ty
            real(kind=8) :: alpha(nbvect)
            real(kind=8) :: beta(nbvect)
            real(kind=8) :: signes(nbvect)
            real(kind=8) :: a(nbvect,*)
            real(kind=8) :: w(*)
            real(kind=8) :: z(*)
            real(kind=8) :: wk(*)
            integer :: mxiter
            integer :: ier
            integer :: nitqr
          end subroutine vp2tru
        end interface
