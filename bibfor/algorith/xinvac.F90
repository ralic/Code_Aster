subroutine xinvac(elp, ndim, tabar, s, ksi)
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterc/r8prem.h"
#include "asterfort/vecini.h"
#include "asterfort/xnewto.h"
    integer :: ndim
    real(kind=8) :: s, ksi(ndim), tabar(*)
    character(len=8) :: elp
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                      TROUVER LES PTS MILIEUX ENTRE LES EXTREMITES DE
!                      L'ARETE ET LE POINT D'INTERSECTION
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       TABAR   : COORDONNEES DES 3 NOEUDS DE L'ARETE
!       S       : ABSCISSE CURVILIGNE DU POINT SUR L'ARETE
!
!     SORTIE
!       XE      : COORDONNES DE REFERENCE DU POINT
!     ----------------------------------------------------------------
!
    real(kind=8) :: coef1, coef2, coef3, ptint(1)
    real(kind=8) :: pt1(ndim), pt2(ndim), pt3(ndim)
    real(kind=8) :: d, epsmax, tab(8, ndim), rbid3(3)
    integer :: itemax, i, ibid, num, ibid3(3)
    character(len=6) :: name
!
!.....................................................................
!
    call jemarq()
!
    itemax=500
    epsmax=1.d-9
    name='XINVAC'
    ptint(1)=0.d0
!
!    CALCUL DE COEF1, COEF2, COEF3, D
    coef1=0.d0
    coef2=0.d0
    coef3=0.d0
    call vecini(ndim, 0.d0, pt1)
    call vecini(ndim, 0.d0, pt2)
    call vecini(ndim, 0.d0, pt3)
!
    do i = 1, ndim
        pt1(i)=tabar(i)
        pt2(i)=tabar(ndim+i)
        pt3(i)=tabar(2*ndim+i)
    end do
!
    do  i = 1, ndim
        coef1 = coef1 + (pt1(i)-2*pt3(i)+pt2(i))* (pt1(i)-2*pt3(i)+ pt2(i))
    end do
!
    do i = 1, ndim
        coef2 = coef2 + (pt2(i)-pt1(i))*(pt1(i)-2*pt3(i)+pt2(i))
    end do
!
    do  i = 1, ndim
        coef3 = coef3 + (pt2(i)-pt1(i))*(pt2(i)-pt1(i))/4
    end do
!
    d = coef2*coef2 - 4*coef1*coef3
!
!    CALCUL COORDONNEES DE REFERENCE DU POINT
!
    if (abs(coef1) .le. r8prem()) then
        ksi(1) = (s/sqrt(coef3))-1
    else if (abs(coef1).gt.r8prem()) then
        if (abs(d) .le. r8prem()) then
            num=1
        else if (d.gt.r8prem()) then
            num=2
        else if (d.lt.-r8prem()) then
            num=3
        endif
!
        call xnewto(elp, name, num, ibid, ibid3,&
                    ndim, ptint, ndim, tabar, rbid3, rbid3,&
                    tab, ibid, ibid, s, itemax,&
                    epsmax, ksi)
    endif
!
    call jedema()
end subroutine
