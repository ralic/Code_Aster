subroutine cbvalr(rouc, neq, smhc, smdi, idlexc,&
                  coefr, coefc, valmi, valmr, valmc)
! aslint: disable=W1304
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-------------------------------------------------------------------
!     BUT : ACCUMULTATION DANS VALMR (OU VALMC) DE COEF*VALMI
!     ROUC=
!        /'RR' : ON UTILISE VALMR ET COEFR
!        /'RC' : ON UTILISE VALMR ET COEFC
!        /'CR' : ON UTILISE VALMC ET COEFR
!        /'CC' : ON UTILISE VALMC ET COEFC
!-------------------------------------------------------------------
#include "asterfort/assert.h"
    character(len=2) :: rouc
    integer(kind=4) :: smhc(*)
    integer :: neq, smdi(*), idlexc(*)
    integer :: kin, idebli, ilig, ifinli, ind, jcol
    real(kind=8) :: coefr, valmi(*), valmr(*)
    complex(kind=8) :: coefc, valmc(*)
!     ------------------------------------------------------------------
    kin = 0
    idebli = 1
!
!
    if (rouc .eq. 'RR') then
!     -------------------------------
        do 20 ilig = 1, neq
            ifinli = smdi(ilig)
            do 10 ind = idebli, ifinli
                kin = kin + 1
                jcol = smhc(ind)
                valmr(kin) = valmr(kin) + coefr*valmi(kin)* (1-idlexc( jcol))* (1-idlexc(ilig))
10          continue
            idebli = smdi(ilig) + 1
20      continue
!
!
    else if (rouc.eq.'RC') then
!     -------------------------------
        do 40 ilig = 1, neq
            ifinli = smdi(ilig)
            do 30 ind = idebli, ifinli
                kin = kin + 1
                jcol = smhc(ind)
                valmr(kin) = valmr(kin) + dble(coefc*valmi(kin)* (1-idlexc(jcol))* (1-idlexc(ilig&
                             &)))
30          continue
            idebli = smdi(ilig) + 1
40      continue
!
!
    else if (rouc.eq.'CR') then
!     -------------------------------
        do 60 ilig = 1, neq
            ifinli = smdi(ilig)
            do 50 ind = idebli, ifinli
                kin = kin + 1
                jcol = smhc(ind)
                valmc(kin) = valmc(kin) + coefr*valmi(kin)* (1-idlexc( jcol))* (1-idlexc(ilig))
50          continue
            idebli = smdi(ilig) + 1
60      continue
!
!
    else if (rouc.eq.'CC') then
!     -------------------------------
        do 80 ilig = 1, neq
            ifinli = smdi(ilig)
            do 70 ind = idebli, ifinli
                kin = kin + 1
                jcol = smhc(ind)
                valmc(kin) = valmc(kin) + coefc*valmi(kin)* (1-idlexc( jcol))* (1-idlexc(ilig))
70          continue
            idebli = smdi(ilig) + 1
80      continue
!
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
