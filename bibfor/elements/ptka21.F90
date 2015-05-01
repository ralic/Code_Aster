subroutine ptka21(sk, e, a, xl, xiy,&
                  xiz, xjx, xig, g, alfay,&
                  alfaz, ey, ez)
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
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/pouex7.h"
#include "asterfort/utmess.h"
    real(kind=8) :: sk(*)
    real(kind=8) :: e, a, xl, xiy, xiz, xjx, xig, g, alfay, alfaz, ey, ez
!    -------------------------------------------------------------------
!    * CE SOUS PROGRAMME CALCULE LA MATRICE DE RAIDEUR DE L'ELEMENT DE
!    POUTRE DROITE A SECTION CONSTANTE ET A 7 DDL PAR NOEUD.
!
!    * DESCRIPTION DE L'ELEMENT:
!      C'EST UN ELEMENT A DEUX NOEUDS ET A 7 DEGRES DE LIBERTES PAR
!      NOEUDS (3 DEPLACEMENTS ,3 ROTATIONS ET 1 DDL DE GAUCHISSEMENT).
!      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT ET L'EXCENTRICITE
!      DU CENTRE DE ROTATION (CENTRE DE TORSION) PAR RAPPORT A LA FIBRE
!      NEUTRE (LIEU DES CENTRES DE GRAVITES).
!
!    * REMARQUE :
!      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
!      UNICOLONNE
!    -------------------------------------------------------------------
!  DONNEES NON MODIFIEES
!
! IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
! IN -------------------------------------------------------------------
! IN R*8  ! E      !     -   ! MODULE D'ELASTICITE DU MATERIAU
! IN R*8  ! A      !     -   ! AIRE DE LA SECTION DROITE DE L'ELEMENT
! IN R*8  ! XL     !     -   ! LONGUEUR DE L ELEMENT
! IN R*8  ! XIY    !     -   ! MOMENT D INERTIE / Y PRINCIPAL
! IN R*8  ! XIZ    !     -   ! MOMENT D INERTIE / Z PRINCIPAL
! IN R*8  ! XJX    !     -   ! CONSTANTE DE TORSION
! IN R*8  ! XIG    !     -   ! MOMENT D'INERTIE DE GAUCHISSEMENT
! IN R*8  ! G      !     -   ! MODULE DE CISAILLEMENT DU MATERIAU
! IN R*8  ! ALFAY  !     -   ! COEFFICIENT DE CISAILLEMENT AXE Y (+)
! IN R*8  ! ALFAZ  !     -   ! COEFFICIENT DE CISAILLEMENT AXE Z (+)
! IN R*8  ! EY     !     -   ! COMPOSANTE GT SUR Y PRINCIPAL
! IN R*8  ! EZ     !     -   ! COMPOSANTE GT SUR Z PRINCIPAL
! IN
! IN (+) REMARQUES :
! IN  -  LE COEFFICIENT DE CISAILLEMENT EST L'INVERSE DU COEFFICIENT DE
! IN     FORME ( IL EST DONC SUPERIEUR A 1)
! IN  -  SI ALFAY OU ALFAZ EST NUL ALORS ON CONSIDERE L'ELEMENT DE TYPE
! IN     EULER-BERNOULLI (I.E.  SANS EFFORT TRANCHANT)
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 !   SK   ! (105)   ! MATRICE ELEMENTAIRE UNICOLONNE
!
!
! LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
! LOC ------------------------------------------------------------------
! LOC I   ! IP     !   14    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
!     ------------------------------------------------------------------
    integer :: ip(14)
    real(kind=8) :: zero
    real(kind=8) :: xl2, xl3, phiy, phiz, eiy, eiz
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    parameter (zero=0.d0)
    data ip/0,1,3,6,10,15,21,28,36,45,55,66,78,91/
! ---------------------------------------------------------------------
    do 1,i = 1,105
    sk(i) = zero
    1 end do
!
!     -- SI G  ET E SONT NULS : K=0
    if (abs(g) .lt. 1.d0/r8gaem()) then
        if (abs(e) .lt. 1.d0/r8gaem()) goto 9999
        call utmess('F', 'ELEMENTS2_54')
    endif
!
!     1/ TRACTION - COMPRESSION
!     -------------------------
    sk(1) = e*a/xl
    sk(ip(8)+1) = -sk(1)
    sk(ip(8)+8) = sk(1)
!
!
!     2/ FLEXION
!     ----------
!        2.1) CALCUL DES CONSTANTES
    xl2 = xl*xl
    xl3 = xl*xl2
    eiy = e*xiy
    eiz = e*xiz
    phiy = (12.d0*eiz*alfay)/ (g*a*xl2)
    phiz = (12.d0*eiy*alfaz)/ (g*a*xl2)
!
!        2.2) REMPLISSAGE DE LA MATRICE
!
!        FLEXION DANS LE PLAN XOY
    sk(ip(2)+2) = 12.d0*eiz/ ((1.d0+phiy)*xl3)
    sk(ip(6)+2) = 6.d0*eiz/ ((1.d0+phiy)*xl2)
    sk(ip(9)+2) = -sk(ip(2)+2)
    sk(ip(13)+2) = sk(ip(6)+2)
    sk(ip(6)+6) = (4.d0+phiy)*eiz/ ((1.d0+phiy)*xl)
    sk(ip(9)+6) = -sk(ip(6)+2)
    sk(ip(13)+6) = (2.d0-phiy)*eiz/ ((1.d0+phiy)*xl)
    sk(ip(9)+9) = sk(ip(2)+2)
    sk(ip(13)+9) = -sk(ip(6)+2)
    sk(ip(13)+13) = sk(ip(6)+6)
!
!        FLEXION DANS LE PLAN XOZ
    sk(ip(3)+3) = 12.d0*eiy/ ((1.d0+phiz)*xl3)
    sk(ip(5)+3) = -6.d0*eiy/ ((1.d0+phiz)*xl2)
    sk(ip(10)+3) = -sk(ip(3)+3)
    sk(ip(12)+3) = sk(ip(5)+3)
    sk(ip(5)+5) = (4.d0+phiz)*eiy/ ((1.d0+phiz)*xl)
    sk(ip(10)+5) = -sk(ip(5)+3)
    sk(ip(12)+5) = (2.d0-phiz)*eiy/ ((1.d0+phiz)*xl)
    sk(ip(10)+10) = sk(ip(3)+3)
    sk(ip(12)+10) = -sk(ip(5)+3)
    sk(ip(12)+12) = sk(ip(5)+5)
!
!     3/ TORSION ET GAUCHISSEMENT
!     ---------------------------
    sk(ip(4)+4) = (3.d0*g*xjx/ (5.d0*xl/2.d0)) + (12.d0*e*xig/xl3)
    sk(ip(7)+4) = (g*xjx/10.d0) + (6.0d0*e*xig/xl2)
    sk(ip(7)+7) = (2.d0*g*xjx*xl/15.d0) + (4.d0*e*xig/xl)
    sk(ip(11)+4) = -sk(ip(4)+4)
    sk(ip(11)+7) = -sk(ip(7)+4)
    sk(ip(11)+11) = sk(ip(4)+4)
    sk(ip(14)+4) = sk(ip(7)+4)
    sk(ip(14)+7) = - (g*xjx*xl/30.d0) + (2.d0*e*xig/xl)
    sk(ip(14)+11) = -sk(ip(7)+4)
    sk(ip(14)+14) = sk(ip(7)+7)
!
!     4/ CHANGEMENT DE VARIABLES DY(C),DZ(C) --> DY(G),DZ(G) (EXCENTR.)
!     -----------------------------------------------------------------
    call pouex7(sk(1), ey, ez)
!
9999  continue
end subroutine
