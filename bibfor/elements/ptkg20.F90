subroutine ptkg20(sf, a, xiz, xiy, iyr,&
                  izr, l, ey, ez, dsm)
    implicit none
#include "asterfort/pouex7.h"
    real(kind=8) :: a, xiz, xiy, ey, ez, l, iyr, izr, yrsiz, zrsiy
    real(kind=8) :: sf(*), dsm(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! INSPI PTKG00
!     CALCUL DE LA MATRICE DE RAIDEUR GEOMETRIQUE  (POU_D_TG)
! ======================================================================
!     ------------------------------------------------------------------
! IN  SF     - (14) COMPOSANTES EFFORTS DANS LES ELEMENTS
! IN  A          - AIRE DE LA SECTION DROITE INITIALE
! IN  XIZ        - MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
! IN  XIY        - MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
! IN  EY         - COMPOSANTE SUIVANT Y PRINCIPAL DE GT.
! IN  EZ         - COMPOSANTE SUIVANT Z PRINCIPAL DE GT.
! IN  L          - LONGUEUR DE L ELEMENT
! OUT DSM    - (105) MATRICE DE RIGIDITE GEOMETRIQUE
!     ------------------------------------------------------------------
    real(kind=8) :: fxb, mya, myb, mza, mzb, ktild, un2, usez, usey
    real(kind=8) :: ymapb, zmapb
    integer :: ip(14)
!-----------------------------------------------------------------------
    real(kind=8) :: ymab, zmab, zp1
!-----------------------------------------------------------------------
    data  ip/ 0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91 /
!     ------------------------------------------------------------------
!
!        FXA=FX(A), FXB=FX(B),... EFFORTS DANS LA POUTRE
!        (ORIENTEE DE A VERS B) EN A ET B
!
    fxb = sf(8 )
    mya = sf(5 )
    mza = sf(6 )
    myb = sf(12)
    mzb = sf(13)
!
!     1/ AFFECTATION DES VALEURS :
!     ----------------------------
!     --- TRIANGULAIRE SUPERIEURE DE LA MATRICE ---
!
    un2 = 1.20000d0
    zp1 = 0.10000d0
    yrsiz = -iyr/xiz +2.d0*ey
    zrsiy = izr/xiy -2.d0*ez
    usez = un2*fxb*ez/l
    usey = un2*fxb*ey/l
    ymab = 0.5000d0*(mya - myb)
    zmab = 0.5000d0*(mza - mzb)
    ymapb = (mya + myb)/l
    zmapb = (mza + mzb)/l
    ktild = fxb*((xiy+xiz)/a + ey*ey + ez*ez)
!
!
    dsm(2+ip(2 )) = un2*fxb / l
    dsm(2+ip(4 )) = usez +un2*ymab/l +0.500d0*ymapb
    dsm(2+ip(6 )) = fxb*zp1
    dsm(2+ip(7 )) = fxb*ez*zp1 -zp1*myb +zp1*l*ymapb
    dsm(2+ip(9 )) = -un2*fxb / l
    dsm(2+ip(11)) = -usez -un2*ymab/l +0.500d0*ymapb
    dsm(2+ip(13)) = fxb*zp1
    dsm(2+ip(14)) = fxb*ez * zp1 +zp1*mya -zp1*l*ymapb
!
    dsm(3+ip(3 )) = un2 * fxb / l
    dsm(3+ip(4 )) = -usey +un2*zmab/l +0.500d0*zmapb
    dsm(3+ip(5 )) = -fxb*zp1
    dsm(3+ip(7 )) = -ey*fxb*zp1 -zp1*mzb +zp1*l*zmapb
    dsm(3+ip(10)) = -un2 * fxb / l
    dsm(3+ip(11)) = usey -un2*zmab/l +0.500d0*zmapb
    dsm(3+ip(12)) = -fxb*zp1
    dsm(3+ip(14)) = -ey*fxb*zp1 +zp1*mza -zp1*l*zmapb
!
    dsm(4+ip(4 )) = un2*ktild/l -un2*zmab*yrsiz/l -un2*ymab*zrsiy/l -ey*zmapb+ez*ymapb +0.5d0*(&
                    -(iyr/xiz)*zmapb+(izr/xiy)*ymapb)
    dsm(4+ip(5 )) = fxb*ey*zp1 +zp1*mzb +zp1*l*zmapb -0.5d0*mza
    dsm(4+ip(6 )) = fxb*ez*zp1 -zp1*myb -zp1*l*ymapb +0.5d0*mya
    dsm(4+ip(7 )) = ktild*zp1 +zp1*mzb*yrsiz+0.1d0*myb*zrsiy
    dsm(4+ip(9 )) = -usez -un2*ymab/l -0.500d0*ymapb
    dsm(4+ip(10)) = usey -un2*zmab/l -0.500d0*zmapb
    dsm(4+ip(11)) = -dsm(4+ip(4))
    dsm(4+ip(12)) = fxb*ey*zp1 -zp1*mza -zp1*l*zmapb
    dsm(4+ip(13)) = fxb*ez*zp1 +zp1*mya +zp1*l*ymapb
    dsm(4+ip(14)) = ktild*zp1 -zp1*mya*zrsiy-0.1d0*mza*yrsiz
!
    dsm(5+ip(5 )) = 2.d0*fxb*l/15.d0
    dsm(5+ip(7 )) = 2.d0*l*fxb*ey/15.d0 -(3.d0*mza-mzb)*l/30.d0
    dsm(5+ip(10)) = fxb*zp1
    dsm(5+ip(11)) = -ey*fxb*zp1 -zp1*mzb -zp1*l*zmapb
    dsm(5+ip(12)) = -fxb*l/30.d0
    dsm(5+ip(14)) = -l*fxb*ey/30.d0 +zmab*l/30.d0 +l*l*zmapb/60.000d0
!
    dsm(6+ip(6 )) = 2.d0*fxb*l/15.d0
    dsm(6+ip(7 )) = 2.d0*l*fxb*ez/15.d0 +(3.d0*mya-myb)*l/30.d0
    dsm(6+ip(9 )) = -fxb*zp1
    dsm(6+ip(11)) = -fxb*ez*zp1 +zp1*myb +zp1*l*ymapb
    dsm(6+ip(13)) = -fxb*l/30.d0
    dsm(6+ip(14)) = -l*fxb*ez/30.d0 -ymab*l/30.d0 -l*l*ymapb/60.000d0
!
    dsm(7+ip(7 )) = 2.d0*ktild*l/15.d0 -(3.d0*mza-mzb)*l*yrsiz/ 30.d0 -(3.d0*mya-myb)*l*zrsiy/30.&
                    &d0
    dsm(7+ip(9 )) = -fxb*ez*zp1 +zp1*myb -zp1*l*ymapb
    dsm(7+ip(10 )) = fxb*ey*zp1 +zp1*mzb -zp1*l*zmapb
    dsm(7+ip(11 )) = -ktild*zp1 -zp1*(myb*zrsiy+mzb*yrsiz)
    dsm(7+ip(12 )) = -ey*l*fxb/30.d0 +zmab*l/30.d0 -l*l*zmapb/60.000d0
    dsm(7+ip(13 )) = -ez*l*fxb/30.d0 -ymab*l/30.d0 +l*l*ymapb/60.000d0
    dsm(7+ip(14 )) = -ktild*l/30.d0 +(zmab*yrsiz+ymab*zrsiy)* l/30.d0
!
!
    dsm(9+ip(9 )) = un2 * fxb / l
    dsm(9+ip(11)) = usez +un2*ymab/l -0.500d0*ymapb
    dsm(9+ip(13)) = -fxb*zp1
    dsm(9+ip(14)) = -ez*fxb*zp1 -zp1*mya +zp1*l*ymapb
!
    dsm(10+ip(10)) = un2 * fxb / l
    dsm(10+ip(11)) = -usey +un2*zmab/l -0.500d0*zmapb
    dsm(10+ip(12)) = fxb*zp1
    dsm(10+ip(14)) = fxb*ey*zp1 -zp1*mza +zp1*l*zmapb
!
    dsm(11+ip(11)) = un2 * ktild/l -un2*(zmab*yrsiz+ymab*zrsiy)/l +ey*zmapb-ez*ymapb +0.5d0*( (iy&
                     &r/xiz)*zmapb-(izr/xiy)*ymapb)
    dsm(11+ip(12)) = -fxb*ey*zp1 +zp1*mza +zp1*l*zmapb -0.5d0*mzb
    dsm(11+ip(13)) = -fxb*ez*zp1 -zp1*mya -zp1*l*ymapb +0.5d0*myb
    dsm(11+ip(14)) = -ktild*zp1 +zp1*(mza*yrsiz+mya*zrsiy)
!
    dsm(12+ip(12)) = 2.d0*fxb*l/15.d0
    dsm(12+ip(14)) = 2.d0*l*fxb*ey/15.d0-(mza-3.d0*mzb)*l/30.d0
!
    dsm(13+ip(13)) = 2.d0*fxb*l/15.d0
    dsm(13+ip(14)) = 2.d0*ez*fxb*l/15.d0+(mya-3.d0*myb)*l/30.d0
!
    dsm(14+ip(14)) = 2.d0*ktild*l/15.d0-(mza - 3.d0*mzb)*yrsiz *l/30.d0 -(mya - 3.d0*myb)*zrsiy*l&
                     &/30.d0
!
!
!     2/ CHANGEMENT DE VARIABLES DY(T),DZ(T) --> DY(G),DZ(G) (EXCENTR.)
!     -----------------------------------------------------------------
!
    call pouex7(dsm(1), ey, ez)
!
!
end subroutine
