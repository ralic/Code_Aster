subroutine arlpff(x1, x2, x3, xl,phiy,phiz,B1d,B1dnrj)

! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"

    real(kind=8) :: xl,B1d(3,12),B1dnrj(6,12)
    real(kind=8)  :: phiy,phiz
    real(kind=8)  :: x1,x2,x3
! --------------------------------------------------------------------
    real(kind=8)  :: x0,y0,z0
    real(kind=8)  :: zero,ntc1,ntc2,dy,dz,n1,n2,n3,n4,n5,n6,n7,n8
    real(kind=8)  :: dntc1,dntc2,dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8
! ......................................................................
!    - CALCUL DES FONCTIONS DE FORME DE DEPLACEMENT
!      GENERALISEES POUTRE 6 DDL A TROIS  POINTS DE GAUSS

!    - ARGUMENTS:
!        DONNEES:           X       -->   VECTEUR DU POINT DE CALCUL DES F FORMES ET DERIVEES
!                           XL      -->   LONGUEUR DE L'ELEMENT
!                          PHIY     -->  COEFF DE CISAILLEMENT SUIVANT Y
!                          PHIZ     -->  COEFF DE CISAILLEMENT SUIVANT Z

!        RESULTATS:
!                             N     <--  MATRICE D'INTERPOLATION

!     DESCRIPTION DE LA NUMEROTATION DU SEG2

!       +-----------------+
!       1                 2
!    - L'ORDRE DES DDL EST  : 1   UX   SUIVANT L'AXE DE LA POUTRE
!                             2   UY   I
!                             3   UZ   I DANS LA SECTION
!                             4   TX   ROTATIONS SUIVANT OX,OY,OZ
!                             5   TY   
!                             6   TZ   
! ----------------------------------------------------------------------
    call jemarq()

! --- ATTRIBUTION DES VALEURS

    x0 = x1
    y0 = x2
    z0 = x3
    zero = 0.d0

! --- FONCTIONS DE FORME DE TRACTION/COMPRESSION/TORSION

    ntc1 = 1.d0-x0
    dntc1 = -1.d0
    ntc2 = x0
    dntc2 = 0.d0

! --- FONCTIONS DE FORME DE FLEXION ET DERIVEES PREMIERES
! --- DEPLACEMENTS (XOZ)

    n1  = (2.d0*x0**3-3.d0*x0**2-x0*phiy+1.d0+phiy) 
    dn1 = (6.d0*x0**2-6.d0*x0-phiy)
    n2  = (-x0**3+0.5d0*(4.d0+phiy)*x0**2-x0*0.5d0*(2.d0+phiy))*xl
    dn2 = (-3.d0*x0**2+(4.d0+phiy)*x0-0.5d0*(2.d0+phiy))*xl
    n3  = -(2.d0*x0**3-3.d0*x0**2-x0*phiy)
    dn3 = -(6.d0*x0**2-6.d0*x0-phiy)
    n4  = (-x0**3+0.5d0*(2.d0-phiy)*x0**2+x0*0.5d0*phiy)*xl
    dn4 = (-3.d0*x0**2+(2.d0-phiy)*x0+0.5d0*phiy)*xl

! --- ROTATIONS (XOZ)

    n5  = 6.d0*x0*(1.d0-x0)/xl
    dn5 = 6.d0*(1.d0-2.d0*x0)/xl
    n6  = (3.d0*x0**2-(4.d0+phiy)*x0+1+phiy)
    dn6  = (6.d0*x0-(4.d0+phiy))
    n7  = -n5
    dn7  = -dn5
    n8  = (3.d0*x0**2+(-2.d0+phiy)*x0)
    dn8  = (6.d0*x0+(-2.d0+phiy))

!   REMPLISSAGE DU VECTEUR

    dy = 1.d0/ (1.d0+phiy)
    dz = 1.d0/ (1.d0+phiz)

    B1d(1,1) = ntc1
    B1d(1,2) = y0*dz*n5
    B1d(1,3) = z0*dy*n5
    B1d(1,4) = zero
    B1d(1,5) = z0*dy*n6
    B1d(1,6) = -y0*dz*n6
    B1d(1,7) = ntc2
    B1d(1,8) = y0*dz*n7
    B1d(1,9) = z0*dy*n7
    B1d(1,10) = zero
    B1d(1,11) = z0*dy*n8
    B1d(1,12) = -y0*dz*n8

    B1d(2,1) = zero
    B1d(2,2) = dz*n1
    B1d(2,3) = zero
    B1d(2,4) = -z0*ntc1
    B1d(2,5) = zero
    B1d(2,6) = -dz*n2
    B1d(2,7) = zero
    B1d(2,8) = dz*n3
    B1d(2,9) = zero
    B1d(2,10) = -z0*ntc2
    B1d(2,11) = zero
    B1d(2,12) = -dy*n4

    B1d(3,1) = zero
    B1d(3,2) = zero
    B1d(3,3) = dy*n1
    B1d(3,4) = y0*ntc1
    B1d(3,5) = dy*n2
    B1d(3,6) = zero
    B1d(3,7) = zero
    B1d(3,8) = zero
    B1d(3,9) = dy*n3
    B1d(3,10) = y0*ntc2
    B1d(3,11) = dy*n4
    B1d(3,12) = zero

    B1dnrj(1,1) = dntc1
    B1dnrj(1,2) = y0*dz*dn5
    B1dnrj(1,3) = z0*dy*dn5
    B1dnrj(1,4) = zero
    B1dnrj(1,5) = z0*dy*dn6
    B1dnrj(1,6) = -y0*dz*dn6
    B1dnrj(1,7) = dntc2
    B1dnrj(1,8) = y0*dz*dn7
    B1dnrj(1,9) = z0*dy*dn7
    B1dnrj(1,10) = zero
    B1dnrj(1,11) = z0*dy*dn8
    B1dnrj(1,12) = -y0*dz*dn8

    B1dnrj(2,1) = zero
    B1dnrj(2,2) = zero
    B1dnrj(2,3) = zero
    B1dnrj(2,4) = zero
    B1dnrj(2,5) = zero
    B1dnrj(2,6) = zero
    B1dnrj(2,7) = zero
    B1dnrj(2,8) = zero
    B1dnrj(2,9) = zero
    B1dnrj(2,10) = zero
    B1dnrj(2,11) = zero
    B1dnrj(2,12) = zero

    B1dnrj(3,1) = zero
    B1dnrj(3,2) = zero
    B1dnrj(3,3) = zero
    B1dnrj(3,4) = zero
    B1dnrj(3,5) = zero
    B1dnrj(3,6) = zero
    B1dnrj(3,7) = zero
    B1dnrj(3,8) = zero
    B1dnrj(3,9) = zero
    B1dnrj(3,10) = zero
    B1dnrj(3,11) = zero
    B1dnrj(3,12) = zero

    B1dnrj(4,1) = zero
    B1dnrj(4,2) = dz*dn1 + dz*n5
    B1dnrj(4,3) = zero
    B1dnrj(4,4) = -z0*dntc1
    B1dnrj(4,5) = zero
    B1dnrj(4,6) = -dz*dn2 - dz*n6
    B1dnrj(4,7) = zero
    B1dnrj(4,8) = dz*dn3 + dz*n7
    B1dnrj(4,9) = zero
    B1dnrj(4,10) = -z0*dntc2
    B1dnrj(4,11) = zero
    B1dnrj(4,12) = -dy*dn4 - dy*n8

    B1dnrj(5,1) = zero
    B1dnrj(5,2) = zero
    B1dnrj(5,3) = dy*dn1 + dy*n5
    B1dnrj(5,4) = y0*dntc1
    B1dnrj(5,5) = dy*dn2 + dy*n6
    B1dnrj(5,6) = zero
    B1dnrj(5,7) = zero
    B1dnrj(5,8) = zero
    B1dnrj(5,9) = dy*dn3 + dy*n7
    B1dnrj(5,10) = y0*dntc2
    B1dnrj(5,11) = dy*dn4 + dy*n8
    B1dnrj(5,12) = zero

    B1dnrj(6,1) = zero
    B1dnrj(6,2) = zero
    B1dnrj(6,3) = zero
    B1dnrj(6,4) = zero
    B1dnrj(6,5) = zero
    B1dnrj(6,6) = zero
    B1dnrj(6,7) = zero
    B1dnrj(6,8) = zero
    B1dnrj(6,9) = zero
    B1dnrj(6,10) = zero
    B1dnrj(6,11) = zero
    B1dnrj(6,12) = zero

    call jedema()

end subroutine
