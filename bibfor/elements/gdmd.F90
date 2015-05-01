subroutine gdmd(x0pg, pn, pm, d)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LA
!           MATRICE D AUX POINTS DE GAUSS. LA MATRICE D INTERVIENT DANS
!           LA RIGIDITE GEOMETRIQUE COMME LA MATRICE DE COMPORTEMENT C
!           INTERVIENT DANS LA RIGIDITE MATERIELLE.
!
!     IN  : VALEURS AU POINT DE GAUSS
!           X0PG      : DERIVEES DES COORDONNEES PAR RAP. A L'ABS. CURV.
!           PN        : RESULTANTE DES FORCES EN AX.GENE.
!           PM        : MOMENT RESULTANT EN AXES GENERAUX
!
!     OUT : D         : MATRICE 9*9
! ------------------------------------------------------------------
    implicit none
#include "asterfort/antisy.h"
#include "blas/ddot.h"
    real(kind=8) :: x0pg(3), pn(3), pm(3), d(9, 9), pntild(3, 3), pmtild(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: scal, un, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    do 2 j = 1, 9
        do 1 i = 1, 9
            d(i,j) = zero
 1      end do
 2  end do
    call antisy(pn, un, pntild)
    call antisy(pm, un, pmtild)
    do 6 i = 1, 3
        do 5 j = 1, 3
            d(i,6+j) = -pntild(i,j)
            d(3+i,6+j) = -pmtild(i,j)
            d(6+i, j) = pntild(i,j)
 5      end do
 6  end do
    scal=ddot(3,pn,1,x0pg,1)
    do 10 j = 1, 3
        do 9 i = 1, 3
            d(6+i,6+j) = pn(i) * x0pg(j)
 9      end do
        d(6+j,6+j) = d(6+j,6+j) - scal
10  end do
end subroutine
