subroutine nmfisa(axi, geom, kpg, poids, b)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
!
    implicit none
#include "asterf_types.h"
#include "asterfort/r8inir.h"
    aster_logical :: axi
    integer :: kpg
    real(kind=8) :: geom(2, 4), poids, b(2, 8)
!
!-----------------------------------------------------------------------
!
! BUT:
!     POUR LE POINT DE GAUSS KPG :
!     CALCUL DU POIDS DU POINT DE GAUSS
!     CALCUL DE LA MATRICE B DONNANT LES SAUT PAR ELEMENTS A PARTIR DES
!     DEPLACEMENTS AUX NOEUDS : SU = B U
!
! REMARQUE :
!
!   LA MATRICE B INCLUE LE CHANGEMENT DE REPERE LOCAL/GLOBAL :
!     U    = DEPLACEMENT DANS LE REPERE GLOBAL
!     ULOC = DEPLACEMENT DANS LE REPERE LOCAL A L'ELEMENT
!
!   B S'ECRIT SOUS LA FORME : B = BTILD RTILD
!   AVEC :
!            SU   = BTILD ULOC
!            ULOC = RTILD U
!
!
! IN  : AXI TRUE EN AXI
! IN  : GEOM,KPG
! OUT : POIDS, B
!
!-----------------------------------------------------------------------
    real(kind=8) :: co, si, c, s, coef(2), aire, rayon
!-----------------------------------------------------------------------
!
    coef(1) = 0.5d0*(1.d0 + sqrt(3.d0)/3.d0)
    coef(2) = 0.5d0*(1.d0 - sqrt(3.d0)/3.d0)
!
! -- CALCUL DU POIDS DU POINT DE GAUSS
!
    aire = sqrt( (geom(1,2)-geom(1,1))**2 + (geom(2,2)-geom(2,1))**2 )
!
!     DISTANCE DU PG A L'AXE DE REVOLUTION (I.E ABSCISSE DU PG)
    rayon = geom(1,1)*coef(kpg) + geom(1,2)*(1.d0-coef(kpg))
    if (axi) aire = aire*rayon
!
    poids = aire/2
!
! -- CALCUL DE LA MATRICE B
!
    call r8inir(16, 0.d0, b, 1)
!
    co = (geom(2,2) - geom(2,1))
    si = -(geom(1,2) - geom(1,1))
!
    c = co / sqrt(co*co + si*si)
    s = si / sqrt(co*co + si*si)
!
! SAISIE DE LA MATRICE B : APPLICATION LINEAIRE DONNANT LE SAUT DE
! DEPLACEMENT DANS L'ELEMENT (SU_N,SU_T) A PARTIR DES DEPLACEMENTS
! AUX NOEUDS :
!
    b(1,1) = c*coef(kpg)
    b(1,2) = s*coef(kpg)
    b(1,3) = c*(1.d0-coef(kpg))
    b(1,4) = s*(1.d0-coef(kpg))
    b(1,5) = -c*(1.d0-coef(kpg))
    b(1,6) = -s*(1.d0-coef(kpg))
    b(1,7) = -c*coef(kpg)
    b(1,8) = -s*coef(kpg)
!
    b(2,1) = -s*coef(kpg)
    b(2,2) = c*coef(kpg)
    b(2,3) = -s*(1.d0-coef(kpg))
    b(2,4) = c*(1.d0-coef(kpg))
    b(2,5) = s*(1.d0-coef(kpg))
    b(2,6) = -c*(1.d0-coef(kpg))
    b(2,7) = s*coef(kpg)
    b(2,8) = -c*coef(kpg)
!
end subroutine
