        interface
          subroutine mefgec(ndim,nbcyl,som,xint,yint,rint,dcent,ficent&
     &,d,fi)
            integer :: nbcyl
            integer :: ndim(14)
            real(kind=8) :: som(9)
            real(kind=8) :: xint(*)
            real(kind=8) :: yint(*)
            real(kind=8) :: rint(*)
            real(kind=8) :: dcent(*)
            real(kind=8) :: ficent(*)
            real(kind=8) :: d(nbcyl,nbcyl)
            real(kind=8) :: fi(nbcyl,nbcyl)
          end subroutine mefgec
        end interface
