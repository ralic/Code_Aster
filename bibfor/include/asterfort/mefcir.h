!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine mefcir(ndim, nbcyl, nbgrp, numgrp, som,&
                      rint, dcent, ficent, d, fi,&
                      ppxx, ppxy, ppyx, ppyy, vnxx,&
                      vnxy, vnyx, vnyy, tmp)
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
        real(kind=8) :: ppxx(nbcyl, nbgrp)
        real(kind=8) :: ppxy(nbcyl, nbgrp)
        real(kind=8) :: ppyx(nbcyl, nbgrp)
        real(kind=8) :: ppyy(nbcyl, nbgrp)
        real(kind=8) :: vnxx(nbcyl, nbgrp)
        real(kind=8) :: vnxy(nbcyl, nbgrp)
        real(kind=8) :: vnyx(nbcyl, nbgrp)
        real(kind=8) :: vnyy(nbcyl, nbgrp)
        real(kind=8) :: tmp(4, *)
    end subroutine mefcir
end interface
