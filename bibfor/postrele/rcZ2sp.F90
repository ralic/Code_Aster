subroutine rcZ2sp(typz, lieu, numsip, pi, mi,&
                  numsiq, pj, mj, seisme, mse, sn,&
                  spij, spmeca)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rcZ2s0.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/rcZ2st.h"
#include "asterfort/getvtx.h"
    integer :: numsip, numsiq
    real(kind=8) :: pi, mi(*), pj, mj(*), mse(*), spij(2), spmeca(2)
    real(kind=8) :: sn
    aster_logical :: seisme
    character(len=4) :: lieu
    character(len=*) :: typz
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
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
!
    integer :: icmp, long, nbinst, nbthep, nbtheq, n1, nb
    integer :: jthunq, i1, jthunp, jthun, jvalin, nbprep, nbp
    integer :: nbpreq, nbq, nbmecap, nbmecaq, indicp, indicq
    real(kind=8) :: pij, mij(12), sp, sij, sqma(6), sqmi(6)
    real(kind=8) :: sp1, sp2, spth(6), spqma(2), spqmi(2), sqth(6)
    real(kind=8) :: racine, c1, c2, diam, ep, inertie, k1, k2, k3, c3
    real(kind=8) :: spqmec1(6), spqmec2(6)
    character(len=4) :: typ2, typ3
    character(len=8) :: type, knumes, knumet, typeke
    character(len=16) :: typmec
    aster_logical :: transip, transif
! DEB ------------------------------------------------------------------
    call jemarq()
    type = typz
    typ3 = 'SP'
!
    call getvtx(' ', 'TYPE_KE', scal=typeke, nbret=nb)
!
    spij(1) = 0.d0
    spij(2) = 0.d0
    spmeca(1) = 0.d0
    spmeca(2) = 0.d0
!
    do 8 i1 = 1, 6
        sqma(i1) = 0.d0
        sqmi(i1) = 0.d0
  8 end do
!
    sij = 0.d0
    racine = 0.d0
!
    transip=.false.
    transif=.false.
    call getvtx(' ', 'TYPE_RESU_MECA', scal=typmec, nbret=n1)
!
    if (typmec .eq. 'ZE200b') then
        transip = .true.
    endif
!
    if (typmec .eq. 'B3200_T') then
        transif = .true.
    endif
!
!--- RECUPERATION DES CARACTERISTIQUES GEOMETRIQUES
!--- ET INDICES DE CONTRAINTE
!
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
    k1 = zr(jvalin)
    c1 = zr(jvalin+1)
    k2 = zr(jvalin+2)
    c2 = zr(jvalin+3)
    k3 = zr(jvalin+4)  
    c3 = zr(jvalin+5) 
    diam = zr(jvalin+6)*2
    ep = zr(jvalin+7) 
    inertie = zr(jvalin+8) 
!
! --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
!
    pij = pi - pj
!
! --- SOMME QUADRATIQUE DES VARIATIONS DE MOMENT RESULTANT
!
    do 10 icmp = 1, 12
        mij(icmp) = mi(icmp) - mj(icmp)
        racine = racine + mij(icmp)**2
 10 end do
!
! --- CALCUL DE SP (PARTIE B3600)
!
    if (.not. transif) then
        if (.not. transip) then 
            sij = (k1-k3+1)*c1*diam*abs(pij)/(2*ep)
            sij = sij + (k2-k3+1)*c2*diam*sqrt(racine)/(2*inertie)
            sij = sij + (k3-1)*sn
        else
            sij = sij + k2*c2*diam*sqrt(racine)/(2*inertie)
            sij = sij + (k3-1)*sn      
        endif
    endif
!
! --- CALCUL DE SP (PARTIE B3200)
!
! --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
!
    if (numsip .ne. 0) then
        knumes = 'S       '
        call codent(numsip, 'D0', knumes(2:8))
        call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
        if (transip) then 
            call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
        else
            nbprep = 0
        endif
        if (transif) then 
            call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
        else
            nbmecap = 0
        endif
        nbp = max(nbthep, nbprep,nbmecap)
        if (nbp .eq. 0) then
            nbinst = 0
            jthun = 1
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            if (seisme) then
                call rcZ2s0(typ3, mij, pij, mse,&
                            nbinst, zr(jthun), sp)
            else
                call rcZ2st(sij, nbinst, zr(jthun), sp)
            endif
            spij(1) = max(spij(1),sp)
            if (typ2 .eq. 'COMB') spij(2) = max(spij(2),sp)
            if (typeke .eq. 'KE_MIXTE') spmeca(1)=sp
        else
            knumet = 'S       '
            call codent(numsip, 'D0', knumet(2:8))
            call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumet), 'LONUTI', long)
            call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumet), 'L', jthunp)
            nbinst = 2
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            do 14 i1 = 1, 6
                spth(i1) = zr(jthunp+6+i1-1) -zr(jthunp+i1-1)
 14         continue
            if (typ2 .eq. 'SITU') then
                if (seisme) then
                    call rcZ2s0(typ3, mij, pij, mse,&
                                1, spth, sp)
                else
                    call rcZ2st(sij, nbinst, spth, sp)
                endif
                spij(1) = max(spij(1),sp)
! CAS DE KE_MIXTE (CALCUL DE SP_MECA) pour la situation p
                if (typeke .eq. 'KE_MIXTE') then
                    nbinst = 2
! on vient chercher les contraintes mécaniques seules
                    indicp = jthunp + nbinst*6*4
                    do 116 i1 = 1, 6
                        spqmec2(i1) = zr(indicp+6+i1-1) - zr(indicp+i1-1)
116                 continue
                    call rcZ2s2(sij, spqmec2, spqma)
                    spmeca(1) = spqma(1)
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
        call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
        if (transip) then
            call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbpreq)
        else
            nbpreq = 0
        endif
        if (transif) then
            call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecaq)
        else
            nbmecaq = 0
        endif 
        nbq = max (nbtheq, nbpreq, nbmecaq)
        if (nbq .eq. 0) then
            nbinst = 0
            jthun = 1
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            if (seisme) then
                call rcZ2s0(typ3, mij, pij, mse, &
                            nbinst, zr(jthun), sp)
            else
                call rcZ2st(sij, nbinst, zr(jthun), sp)
            endif
            spij(1) = max(spij(1),sp)
            if (typ2 .eq. 'COMB') spij(2) = max(spij(2),sp)
! - CAS NBQ = 0 / NBP != 0
            if (typ2 .eq. 'COMB' .and. nbp .ne. 0) then
                if (seisme) then
                    call rcZ2s0(typ3, mij, pij, mse, &
                                1, spth, sp)
                    spij(1) = sp
                else
                    call rcZ2s2(sij, spth, spij)
                endif
            endif
        else
            knumet = 'S       '
            call codent(numsiq, 'D0', knumet(2:8))
            call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumet), 'LONUTI', long)
            call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumet), 'L', jthunq)
            nbinst = 2
            typ2 = '????'
            if (type .eq. 'SP_COMB') then
                typ2 = 'COMB'
            else if (type .eq. 'SP_SITU') then
                typ2 = 'SITU'
            endif
            if (typ2 .eq. 'SITU') then
                if (seisme) then
                    call rcZ2s0(typ3, mij, pij, mse,&
                                nbinst, spth, sp)
                else
                    call rcZ2st(sij, nbinst, spth, sp)
                endif
                spij(1) = sp
            else
! - CAS NBP = 0 / NBQ != 0
                if (nbp .eq. 0) then
                    do 113 i1 = 1, 6
                        sqth(i1) = zr(jthunq+i1-1) - zr(jthunq+6+i1-1)
113                 continue
                    if (seisme) then
                        call rcZ2s0(typ3, mij, pij, mse,&
                                    nbinst, sqth, sp)
                        spij(1) = sp
                    else
                        call rcZ2s2(sij, sqth, spij)
                    endif
! CAS DE KE_MIXTE (CALCUL DE SP_MECA) pour la situation p
                    if (typeke .eq. 'KE_MIXTE') then
                       nbinst = 2
! on vient chercher les contraintes mécaniques seules
                        indicq = jthunq + nbinst*6*4
                        do 118 i1 = 1, 6
                            spqmec2(i1) = zr(indicq+6+i1-1) - zr(indicq+i1-1)
118                     continue
                        call rcZ2s2(sij, spqmec2, spqma)
                        spmeca(1) = spqma(1)
                    endif
                else
                    do 114 i1 = 1, 6
                        sqmi(i1) = zr(jthunp+i1-1) - zr(jthunq+6+i1-1)
                        sqma(i1) = zr(jthunp+6+i1-1) - zr(jthunq+i1-1)
114                 continue
                    if (seisme) then
                        call rcZ2s0(typ3, mij, pij, mse,&
                                    1, sqmi, sp1)
                        call rcZ2s0(typ3, mij, pij, mse,&
                                    1, sqma, sp2)
                        spij(1) = max(sp1,sp2)
                        spij(2) = min(sp1,sp2)
                    else
                        call rcZ2s2(sij, sqmi, spqmi)
                        call rcZ2s2(sij, sqma, spqma)
                        spij(1) = max(spqma(1),spqmi(1))
                        spij(2) = min(spqma(1),spqmi(1))
                    endif
! CAS DE KE_MIXTE (CALCUL DE SP_MECA) pour les situations p et q
                    if (typeke .eq. 'KE_MIXTE') then
                        nbinst = 2
! on vient chercher les contraintes mécaniques seules
                        indicp = jthunp + nbinst*6*4
                        indicq = jthunq + nbinst*6*4
                        do 112 i1 = 1, 6
                            spqmec1(i1) = zr(indicp+i1-1) - zr(indicq+6+i1-1)
                            spqmec2(i1) = zr(indicp+6+i1-1) - zr(indicq+i1-1)
112                     continue
                        call rcZ2s2(sij, spqmec1, spqmi)
                        call rcZ2s2(sij, spqmec2, spqma)
                        spmeca(1) = max(spqma(1),spqmi(1))
                    endif
                endif
            endif
        endif
    endif
!
    call jedema()
end subroutine
