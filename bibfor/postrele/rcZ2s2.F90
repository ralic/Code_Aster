subroutine rcZ2s2(typ, pi, mi, pj, mj, s2)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jedema.h"
    character(len=2) :: typ
    real(kind=8) :: pi, mi(*), pj, mj(*), s2
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!     CALCUL DU SN OU SN* (partie B3600)
!
! IN  : TYP    : SN OU SP
! IN  : PI     : PRESSION ASSOCIEE A L'ETAT I
! IN  : MI     : EFFORTS ET MOMENTS ASSOCIEES A L'ETAT I
! IN  : PJ     : PRESSION ASSOCIEE A L'ETAT J
! IN  : MJ     : EFFORTS ET MOMENTS ASSOCIEES A L'ETAT J
! OUT : SN2    : PARTIE CALCULEE de SN
!     ------------------------------------------------------------------
!
    real(kind=8) :: racine, k1, c1, k2, c2, rayon, ep, inertie
    real(kind=8) :: coefp, coefm, pij, mij(12), k2tub, c2tub, k2cor
    integer :: jvalin, i
    real(kind=8) :: c2cor, rtub, itub, rcor, icor, coefcor, coeftub
    real(kind=8) :: mijcor(3), racicor
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    s2 = 0.d0
    racine = 0.d0
    racicor = 0.d0
!
!--- RECUPERATION DES CARACTERISTIQUES GEOMETRIQUES
!--- ET INDICES DE CONTRAINTE
!
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
    k1 = zr(jvalin)
    c1 = zr(jvalin+1)
    k2 = zr(jvalin+2)
    c2 = zr(jvalin+3) 
    rayon = zr(jvalin+6) 
    ep = zr(jvalin+7) 
    inertie = zr(jvalin+8)
    k2tub = zr(jvalin+11)
    c2tub = zr(jvalin+12)
    k2cor = zr(jvalin+13)
    c2cor = zr(jvalin+14)
    rtub = zr(jvalin+15)
    itub = zr(jvalin+16)
    rcor = zr(jvalin+17)
    icor = zr(jvalin+18)
!
   if (typ .eq. 'SN') then
       coefp = c1
       coefm = c2
       coefcor = c2cor
       coeftub = c2tub
   else
       coefp = k1*c1
       coefm = k2*c2
       coefcor = k2cor*c2cor
       coeftub = k2tub*c2tub
   endif 
!
! --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
!
    pij = pi - pj
!
! --- SOMME QUADRATIQUE DES VARIATIONS DE MOMENT RESULTANT
!
    do 20 i = 1, 3
        mij(i) = mi(3+i) - mj(3+i)
        racine = racine + mij(i)**2        
        mijcor(i) = mi(9+i) - mj(9+i)
        racicor = racicor + mijcor(i)**2
 20 continue
!
! --- CALCUL DE SN ou SP(PARTIE B3600)
!
    s2 = coefp*rayon*abs(pij)/ep
    if (rcor+rtub .eq. 0) then
        s2 = s2 + coefm*rayon*sqrt(racine)/inertie
    else
        s2 = s2 + coefcor*rcor*sqrt(racicor)/icor
        s2 = s2 + coeftub*rtub*sqrt(racine)/itub
    endif
!
    call jedema()
end subroutine
