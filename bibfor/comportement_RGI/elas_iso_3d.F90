subroutine elas_iso_3d(depst6, e1, xnu1, dsige6)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!       calcul d'un increment de contrainte pour la loi elastique  isotr
!=====================================================================
    implicit none
        real(kind=8) :: depst6(6)
        real(kind=8) :: e1
        real(kind=8) :: xnu1
        real(kind=8) :: dsige6(6)
!       depst6: pour les 3 1er, gamma pour les autres
        real(kind=8) :: dgamd6(6),depsv,xk0,xmu0
        integer::i
!       increment de la deformation volumique
    depsv=0.d0
    do i = 1, 3
        depsv=depsv+depst6(i)
    end do
!       increment des gammas
    do i = 1, 3
        dgamd6(i)=(depst6(i)-depsv/3.d0)*2.d0
    end do
    xk0=e1/3.d0/(1.d0-2.d0*xnu1)
    xmu0=e1/2.d0/(1.d0+xnu1)
!       calcul des increments de contraintes effectives dans le squelett
    do i = 1, 3
        dsige6(i)=xk0*depsv+xmu0*dgamd6(i)
    end do
    do i = 4, 6
        dsige6(i)=xmu0*depst6(i)
    end do
end subroutine
