subroutine rc32sp(typz, lieu, numsip, pi, mi,&
                  numsiq, pj, mj, seisme, mse,&
                  spij, typeke, spmeca, spther)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rc32s0.h"
#include "asterfort/rc32s2.h"
#include "asterfort/rc32st.h"
    integer :: numsip, numsiq
    real(kind=8) :: pi, mi(*), pj, mj(*), mse(*), spij(2), typeke, spmeca(2)
    real(kind=8) :: spther(2)
    logical(kind=1) :: seisme
    character(len=4) :: lieu
    character(len=*) :: typz
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     CALCUL DU SP
!
!     ------------------------------------------------------------------
! IN  : TYPZ   : 'SP_SITU'  : CALCUL DU SP POUR LA SITUATION
!              : 'SP_COMB'  : CALCUL DU SP POUR COMBINAISON SITUATION
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : PI     : PRESSION ASSOCIEE A L'ETAT STABILISE I
! IN  : MI     : EFFORTS ASSOCIEES A L'ETAT STABILISE I
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
! IN  : MJ     : EFFORTS ASSOCIEES A L'ETAT STABILISE J
! IN  : MSE    : EFFORTS DUS AU SEISME
! OUT : SPIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES TOTALES
! OUT : SPMECA : AMPLITUDE DE VARIATION DES CONTRAINTES MECANIQUES
! OUT : SPTHER : AMPLITUDE DE VARIATION DES CONTRAINTES THERMIQUES
!
    integer :: icmp, jsigu, icmps, long, nbinst, nbthep, nbtheq, jther, numth
    integer :: jthunq, i1, jthunp, jthun
    real(kind=8) :: pij, mij(12), sp, sij(6), sigu, sij0(6), sqma(6), sqmi(6)
    real(kind=8) :: sp1, sp2, spth(6), spqma(2), spqmi(2), sqth(6)
    character(len=4) :: typ2
    character(len=8) ::  type, knumes, knumet
! DEB ------------------------------------------------------------------
    type = typz
!
    spij(1) = 0.d0
    spij(2) = 0.d0
    spther(1) = 0.d0
    spther(2) = 0.d0
    spmeca(1) = 0.d0
    spmeca(2) = 0.d0
    do 8 i1 = 1, 6
        sqma(i1) = 0.d0
        sqmi(i1) = 0.d0
 8  end do
!
! --- CONTRAINTES LINEAIRISEES DUES AUX CHARGEMENTS UNITAIRES
!
    call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
!
! --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
!
    pij = pi - pj
!
! --- VARIATION DE MOMENT RESULTANT
!
    do 10 icmp = 1, 12
        mij(icmp) = mi(icmp) - mj(icmp)
10  end do
!
! --- CALCUL DES CONTRAINTES EN PEAU PAR COMBINAISON LINEAIRE
!     POUR LE CHARGEMENT PIJ, MIJ
!
    do 30 icmps = 1, 6
        sij(icmps) = 0.d0
        sij0(icmps) = 0.d0
        do 20 icmp = 1, 12
            sigu = zr(jsigu-1+6*(icmp-1)+icmps)
            sij(icmps) = sij(icmps) + mij(icmp)*sigu
20      continue
! ----- PRESSION
        sigu = zr(jsigu-1+72+icmps)
        sij(icmps) = sij(icmps) + pij*sigu
30  end do
!
!
! CAS DE KE_MECA (PAS DE PARTITION MECANIQUE - THERMIQUE)
!
! --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
!
    if (numsip .ne. 0) then
        knumes = 'S       '
        call codent(numsip, 'D0', knumes(2:8))
        call jelira(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'LONUTI', nbthep)
        if (nbthep .eq. 0) then
            nbinst = 0
            jthun = 1
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            if (seisme) then
                call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                            nbinst, zr(jthun), sp)
            else
                call rc32st(sij, nbinst, zr(jthun), sp)
            endif
            spij(1) = max(spij(1),sp)
            if (typ2 .eq. 'COMB') spij(2) = max(spij(2),sp)
        else
            call jeveuo(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'L', jther)
            numth = zi(jther)
            knumet = 'T       '
            call codent(numth, 'D0', knumet(2:8))
            call jelira(jexnom('&&RC3200.THER_UNIT .'//lieu, knumet), 'LONUTI', long)
            call jeveuo(jexnom('&&RC3200.THER_UNIT .'//lieu, knumet), 'L', jthunp)
            nbinst = 2
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            do 14 i1 = 1, 6
                spth(i1) = zr(jthunp+6+i1-1) -zr(jthunp+i1-1)
14          continue
            if (typ2 .eq. 'SITU') then
                if (seisme) then
                    call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                                1, spth, sp)
                else
                    call rc32st(sij, nbinst, spth, sp)
                endif
                spij(1) = max(spij(1),sp)
! --- CALCUL DE KE_THER POUR LA SITUATION P
                if (typeke .gt. 0.d0) then
                    call rc32s2(sij0, spth, spther)
                endif
            endif
        endif
    endif
!
!
! --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q
!
    if (numsiq .ne. 0) then
        knumes = 'S       '
        call codent(numsiq, 'D0', knumes(2:8))
        call jelira(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'LONUTI', nbtheq)
        if (nbtheq .eq. 0) then
            nbinst = 0
            jthun = 1
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            if (seisme) then
                call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                            nbinst, zr(jthun), sp)
            else
                call rc32st(sij, nbinst, zr(jthun), sp)
            endif
            spij(1) = max(spij(1),sp)
            if (typ2 .eq. 'COMB') spij(2) = max(spij(2),sp)
! - CAS NBQ = 0 / NBP != 0
            if (typ2 .eq. 'COMB' .and. nbthep .ne. 0) then
                if (seisme) then
                    call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                                1, spth, sp)
                    spij(1) = sp
                else
                    call rc32s2(sij, spth, spij)
                endif
            endif
        else
            call jeveuo(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'L', jther)
            numth = zi(jther)
            knumet = 'T       '
            call codent(numth, 'D0', knumet(2:8))
            call jelira(jexnom('&&RC3200.THER_UNIT .'//lieu, knumet), 'LONUTI', long)
            call jeveuo(jexnom('&&RC3200.THER_UNIT .'//lieu, knumet), 'L', jthunq)
            nbinst = 2
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            if (typ2 .eq. 'SITU') then
                if (seisme) then
                    call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                                nbinst, spth, sp)
                else
                    call rc32st(sij, nbinst, spth, sp)
                endif
                spij(1) = sp
            else
! - CAS NBP = 0 / NBQ != 0
                if (nbthep .eq. 0) then
                    do 113 i1 = 1, 6
                        sqth(i1) = zr(jthunq+i1-1) - zr(jthunq+6+i1-1)
113                  continue
                    if (seisme) then
                        call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                                    nbinst, sqth, sp)
                        spij(1) = sp
                    else
                        call rc32s2(sij, sqth, spij)
                    endif
                else
                    do 114 i1 = 1, 6
                        sqmi(i1) = zr(jthunp+i1-1) - zr(jthunq+6+i1-1)
                        sqma(i1) = zr(jthunp+6+i1-1) - zr(jthunq+i1-1)
114                  continue
                    if (seisme) then
                        call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                                    1, sqmi, sp1)
                        call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                                    1, sqma, sp2)
                        spij(1) = max(sp1,sp2)
                        spij(2) = min(sp1,sp2)
                    else
                        call rc32s2(sij, sqmi, spqmi)
                        call rc32s2(sij, sqma, spqma)
                        spij(1) = max(spqma(1),spqmi(1))
                        spij(2) = min(spqma(1),spqmi(1))
                    endif
! --- CALCUL DE KE_THER POUR LA COMBINAISON DES SITUATIONS P ET Q
                    if (typeke .gt. 0.d0) then
                        call rc32s2(sij0, sqmi, spqmi)
                        call rc32s2(sij0, sqma, spqma)
                        spther(1) = max(spqma(1),spqmi(1))
                        spther(2) = min(spqma(1),spqmi(1))
                    endif
                endif
            endif
        endif
    endif
!
! CAS DE KE_MIXTE (PARTITION MECANIQUE - THERMIQUE)
!
    if (typeke .gt. 0.d0) then
!
! --- CALCUL DE KE_MECA
        nbinst = 0
        jthun = 1
        typ2 = '????'
        if (type .eq. 'SP_COMB') then
            typ2 = 'COMB'
        else if (type .eq. 'SP_SITU') then
            typ2 = 'SITU'
        endif
        if (seisme) then
            call rc32s0(typ2, mij, pij, mse, zr(jsigu),&
                        nbinst, zr(jthun), sp)
        else
            call rc32st(sij, nbinst, zr(jthun), sp)
        endif
        spmeca(1) =sp
!
    endif
!
end subroutine
