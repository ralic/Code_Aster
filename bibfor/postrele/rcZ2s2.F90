subroutine rcZ2s2(typ, pi, mi, pj, mj, seisme, mse, s2)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jedema.h"
    character(len=2) :: typ
    real(kind=8) :: pi, mi(*), pj, mj(*), mse(*), s2
    aster_logical :: seisme
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
!     CALCUL de la partie simplifiée (B3600) DU SN ou du SP
!
! IN  : TYP    : SN OU SP
! IN  : PI     : PRESSION ASSOCIEE A L'ETAT I
! IN  : MI     : EFFORTS ET MOMENTS ASSOCIEES A L'ETAT I
! IN  : PJ     : PRESSION ASSOCIEE A L'ETAT J
! IN  : MJ     : EFFORTS ET MOMENTS ASSOCIEES A L'ETAT J
! IN  : SEISME : CALCUL AVEC OU SANS SEISME
! IN  : MSE    : MOMENTS ASSOCIEES AU SEISME
! OUT : S2     : PARTIE B3600 de SN/SP
!     ------------------------------------------------------------------
!
    real(kind=8) :: s2p, s2m, racine, racicor, k1, c1, k2, c2
    integer :: jvalin, i, i0, i1, i2, i3, i4, i5, i6
    real(kind=8) :: rayon, ep, inertie, k2tub, c2tub, k2cor
    real(kind=8) :: c2cor, rtub, itub, rcor, icor, coefp, coefm
    real(kind=8) :: coefcor, coeftub, pij, mij(3), mijcor(3)
    real(kind=8) :: s2ms, racines, racicors, fact, e1(2), e2(2)
    real(kind=8) :: e3(2), e4(2), e5(2), e6(2), mijs(3), mijcors(3) 
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    s2 = 0.d0
    s2p = 0.d0
    s2m = 0.d0
    racine = 0.d0
    racicor = 0.d0
!-----------------------------------------------------
!--- RECUPERATION DES CARACTERISTIQUES GEOMETRIQUES
!--- ET INDICES DE CONTRAINTE
!-----------------------------------------------------
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
!-----------------------------------------------------
! --- CALCUL DE LA PARTIE DUE A LA PRESSION S2P
!-----------------------------------------------------
    pij = pi - pj
    s2p = coefp*rayon*abs(pij)/ep
!---------------------------------------------------------
! --- CALCUL DE LA PARTIE DUE AUX MOMENTS S2M SANS SEISME
!---------------------------------------------------------
    do 100 i = 1, 3    
        mij(i) = mi(i+3) - mj(i+3)
        racine = racine + mij(i)**2
        mijcor(i) = mi(9+i) - mj(9+i)
        racicor = racicor + mijcor(i)**2
 100 continue
!
    if (rcor+rtub .eq. 0) then
        s2m = coefm*rayon*sqrt(racine)/inertie
    else
        s2m = coefcor*rcor*sqrt(racicor)/icor+ coeftub*rtub*sqrt(racine)/itub
    endif
!---------------------------------------------------------
! --- CALCUL DE LA PARTIE DUE AUX MOMENTS S2M AVEC SEISME
!---------------------------------------------------------
    if (.not. seisme) goto 999
!
    s2m = 0.d0
    s2ms = 0.d0
    racines = 0.d0
    racicors = 0.d0
    fact = 2
    do 2 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e1(i0) = i1 * fact
        e2(i0) = i1 * fact
        e3(i0) = i1 * fact
        e4(i0) = i1 * fact
        e5(i0) = i1 * fact
        e6(i0) = i1 * fact
 2  continue
!
    do 60 i1 = 1, 2
        do 50 i2 = 1, 2
           do 40 i3 = 1, 2
               do 30 i4 = 1, 2
                   do 20 i5 = 1, 2
                       do 10 i6 = 1, 2
                           mijs(1) = mi(4) - mj(4)+ mse(4)*e1(i1)
                           mijs(2) = mi(5) - mj(5)+ mse(5)*e2(i2)
                           mijs(3) = mi(6) - mj(6)+ mse(6)*e3(i3)
                           racines = mijs(1)**2+mijs(2)**2+mijs(3)**2
                           mijcors(1) = mi(10) - mj(10)+ mse(10)*e4(i4)
                           mijcors(2) = mi(11) - mj(11)+ mse(11)*e5(i5)
                           mijcors(3) = mi(12) - mj(12)+ mse(12)*e6(i6)
                           racicors = mijcors(1)**2+mijcors(2)**2+mijcors(3)**2
!
                           if (rcor+rtub .eq. 0) then
                               s2ms = coefm*rayon*sqrt(racines)/inertie
                           else
                               s2ms = coefcor*rcor*sqrt(racicors)/icor+&
                                     coeftub*rtub*sqrt(racines)/itub
                           endif
                           s2m = max(s2m,s2ms)
10                     continue
20                 continue
30             continue
40         continue
50      continue
60  continue
!
999 continue
!!-------------------------------
! --- SOMME PRESSION + MOMENTS
!!-------------------------------
    s2 = s2p+s2m
!
    call jedema()
!
end subroutine
