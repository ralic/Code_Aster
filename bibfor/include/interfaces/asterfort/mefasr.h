        interface
          subroutine mefasr(ndim,nbcyl,nbgrp,nbtron,numgrp,idir,igrp,&
     &xint,yint,rint,sgn,orig,beta,a,b)
            integer :: nbtron
            integer :: nbcyl
            integer :: ndim(14)
            integer :: nbgrp
            integer :: numgrp(*)
            integer :: idir
            integer :: igrp
            real(kind=8) :: xint(*)
            real(kind=8) :: yint(*)
            real(kind=8) :: rint(*)
            integer :: sgn(*)
            integer :: orig(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: a(2*nbtron*nbcyl,*)
            real(kind=8) :: b(*)
          end subroutine mefasr
        end interface
