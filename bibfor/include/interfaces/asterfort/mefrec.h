        interface
          subroutine mefrec(ndim,nbcyl,nbgrp,numgrp,xint,yint,rint,sgn&
     &,orig,beta,ppxx,ppxy,ppyx,ppyy,vnxx,vnxy,vnyx,vnyy,tmp)
            integer :: nbgrp
            integer :: nbcyl
            integer :: ndim(14)
            integer :: numgrp(*)
            real(kind=8) :: xint(*)
            real(kind=8) :: yint(*)
            real(kind=8) :: rint(*)
            integer :: sgn(*)
            integer :: orig(*)
            real(kind=8) :: beta(*)
            real(kind=8) :: ppxx(nbcyl,nbgrp)
            real(kind=8) :: ppxy(nbcyl,nbgrp)
            real(kind=8) :: ppyx(nbcyl,nbgrp)
            real(kind=8) :: ppyy(nbcyl,nbgrp)
            real(kind=8) :: vnxx(nbcyl,nbgrp)
            real(kind=8) :: vnxy(nbcyl,nbgrp)
            real(kind=8) :: vnyx(nbcyl,nbgrp)
            real(kind=8) :: vnyy(nbcyl,nbgrp)
            real(kind=8) :: tmp(4,*)
          end subroutine mefrec
        end interface
