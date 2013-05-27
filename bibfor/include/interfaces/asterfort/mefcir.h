        interface
          subroutine mefcir(ndim,nbcyl,nbgrp,numgrp,som,rint,dcent,&
     &ficent,d,fi,ppxx,ppxy,ppyx,ppyy,vnxx,vnxy,vnyx,vnyy,tmp)
            integer :: nbgrp
            integer :: nbcyl
            integer :: ndim(14)
            integer :: numgrp(*)
            real(kind=8) :: som(9)
            real(kind=8) :: rint(*)
            real(kind=8) :: dcent(*)
            real(kind=8) :: ficent(*)
            real(kind=8) :: d(*)
            real(kind=8) :: fi(*)
            real(kind=8) :: ppxx(nbcyl,nbgrp)
            real(kind=8) :: ppxy(nbcyl,nbgrp)
            real(kind=8) :: ppyx(nbcyl,nbgrp)
            real(kind=8) :: ppyy(nbcyl,nbgrp)
            real(kind=8) :: vnxx(nbcyl,nbgrp)
            real(kind=8) :: vnxy(nbcyl,nbgrp)
            real(kind=8) :: vnyx(nbcyl,nbgrp)
            real(kind=8) :: vnyy(nbcyl,nbgrp)
            real(kind=8) :: tmp(4,*)
          end subroutine mefcir
        end interface
