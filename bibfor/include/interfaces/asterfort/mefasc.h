        interface
          subroutine mefasc(ndim,nbcyl,nbgrp,nbtron,numgrp,idir,igrp,&
     &som,rint,dcent,ficent,d,fi,a,b)
            integer :: nbtron
            integer :: nbcyl
            integer :: ndim(14)
            integer :: nbgrp
            integer :: numgrp(*)
            integer :: idir
            integer :: igrp
            real(kind=8) :: som(9)
            real(kind=8) :: rint(*)
            real(kind=8) :: dcent(nbcyl)
            real(kind=8) :: ficent(nbcyl)
            real(kind=8) :: d(nbcyl,nbcyl)
            real(kind=8) :: fi(nbcyl,nbcyl)
            real(kind=8) :: a(2*nbtron*(nbcyl+1),*)
            real(kind=8) :: b(*)
          end subroutine mefasc
        end interface
