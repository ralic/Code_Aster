subroutine rcZ2sn(typz, lieu, numsip, pi, mi,&
                  numsiq, pj, mj, seisme, mse,&
                  snij)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rcZ2s0.h"
#include "asterfort/rcZ2st.h"
    integer :: numsip, numsiq
    real(kind=8) :: pi, mi(*), pj, mj(*), mse(*), snij
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
!     CALCUL DU SN OU SN*
!
! IN  : TYPZ   : 'SN_SITU'  : CALCUL DU SN POUR LA SITUATION
!              : 'SN*_SITU' : CALCUL DU SN* POUR LA SITUATION
!              : 'SN_COMB'  : CALCUL DU SN POUR COMBINAISON SITUATION
!              : 'SN*_COMB' : CALCUL DU SN* POUR COMBINAISON SITUATION
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : PI     : PRESSION ASSOCIEE A L'ETAT STABILISE I
! IN  : MI     : EFFORTS ASSOCIEES A L'ETAT STABILISE I (6)
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
! IN  : MJ     : EFFORTS ASSOCIEES A L'ETAT STABILISE J (6)
! IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
! IN  : MSE    : EFFORTS DUS AU SEISME
! OUT : SNIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES LINEARISEES
!     ------------------------------------------------------------------
!
    integer :: icmp, nbinst, long, i1, nbthep, nbtheq, jther, ith
    integer :: numth, jthun, indicp, indicq, jvalin
    real(kind=8) :: pij, mij(6), sn, sij, sqma(6), sqmi(6), sn1, sn2
    real(kind=8) :: snth(6), racine, c1, c2, diam, ep, inertie
    character(len=4) :: typ2
    character(len=8) :: type, knumes, knumet
! DEB ------------------------------------------------------------------
    type = typz
    sn = 0.d0
    sn1 = 0.d0
    sn2 = 0.d0
    sij = 0.d0
    racine = 0.d0
!
!--- RECUPERATION DES CARACTERISTIQUES GEOMETRIQUES
!--- ET INDICES DE CONTRAINTE
!
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
    c1 = zr(jvalin)  
    c2 = zr(jvalin+1) 
    diam = zr(jvalin+4) 
    ep = zr(jvalin+5) 
    inertie = zr(jvalin+6) 
!
! --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
!
    pij = pi - pj
!
! --- SOMME QUADRATIQUE DES VARIATIONS DE MOMENT RESULTANT
!
    do 10 icmp = 1, 6
        mij(icmp) = mi(icmp) - mj(icmp)
        racine = racine + mij(icmp)**2
 10 end do
!
! --- CALCUL DE SN (PARTIE B3600)
!
    sij = c1*diam*abs(pij)/(2*ep)
    sij = sij + c2*diam*sqrt(racine)/(2*inertie)
!
! --- CALCUL DE SN (PARTIE B3200)
! --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
!
    if (numsip .ne. 0) then
        knumes = 'S       '
        call codent(numsip, 'D0', knumes(2:8))
        call jelira(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'LONUTI', nbthep)
        if (nbthep .eq. 0) then
            nbinst = 0
            indicp = 1
            if (type .eq. 'SN_COMB' .or. type .eq. 'SN*_COMB') then
                typ2 = 'COMB'
                elseif( type .eq. 'SN_SITU' .or. type .eq. 'SN*_SITU' )&
            then
                typ2 = 'SITU'
            endif
            if (seisme) then
                call rcZ2s0(mij, pij, mse,&
                            nbinst, zr(indicp), sn)
            else
                call rcZ2st(sij, nbinst, zr(indicp), sn)
            endif
            snij = max( snij , sn )
        else
            call jeveuo(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'L', jther)
            do 100 ith = 1, nbthep
                numth = zi(jther+ith-1)
                knumet = 'T       '
                call codent(numth, 'D0', knumet(2:8))
                call jelira(jexnom('&&RC3200.THER .'//lieu, knumet), 'LONUTI', long)
                call jeveuo(jexnom('&&RC3200.THER .'//lieu, knumet), 'L', jthun)
                nbinst = 2
                if (type .eq. 'SN_COMB') then
                    indicp = jthun + 6*nbinst
                    typ2 = 'COMB'
                else if (type .eq. 'SN*_COMB') then
                    indicp = jthun + 12*nbinst
                    typ2 = 'COMB'
                else if (type .eq. 'SN_SITU') then
                    indicp = jthun + 6*nbinst
                    typ2 = 'SITU'
                else if (type .eq. 'SN*_SITU') then
                    indicp = jthun + 12*nbinst
                    typ2 = 'SITU'
                endif
                do 14 i1 = 1, 6
                    snth(i1) = zr(indicp+6+i1-1) -zr(indicp+i1-1)
 14             continue
                if (seisme) then
                    call rcZ2s0(mij, pij, mse,&
                                nbinst, snth, sn)
                else
                    if (typ2 .eq. 'SITU') then
                        call rcZ2st(sij, nbinst, snth, sn)
                    else
                        sn = 0.d0
                    endif
                endif
                snij = max( snij , sn )
100         continue
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
            indicq = 1
            if (type .eq. 'SN_COMB' .or. type .eq. 'SN*_COMB') then
                typ2 = 'COMB'
                elseif( type .eq. 'SN_SITU' .or. type .eq. 'SN*_SITU' )&
            then
                typ2 = 'SITU'
            endif
            if (seisme) then
                call rcZ2s0(mij, pij, mse,&
                            nbinst, zr(indicq), sn)
            else
                call rcZ2st(sij, nbinst, zr(indicq), sn)
            endif
            snij = max( snij , sn )
            if (typ2 .eq. 'COMB' .and. nbthep .ne. 0) then
                if (seisme) then
                    call rcZ2s0(mij, pij, mse,&
                                nbinst, snth, sn)
                else
                    call rcZ2st(sij, 2, snth, sn)
                endif
                snij = max( snij , sn )
            endif
        else
            call jeveuo(jexnom('&&RC3200.SITU_THERMIQUE', knumes), 'L', jther)
            do 110 ith = 1, nbtheq
                numth = zi(jther+ith-1)
                knumet = 'T       '
                call codent(numth, 'D0', knumet(2:8))
                call jelira(jexnom('&&RC3200.THER .'//lieu, knumet), 'LONUTI', long)
                call jeveuo(jexnom('&&RC3200.THER .'//lieu, knumet), 'L', jthun)
                nbinst = 2
                if (type .eq. 'SN_COMB') then
                    indicq = jthun + 6*nbinst
                    typ2 = 'COMB'
                else if (type .eq. 'SN*_COMB') then
                    indicq = jthun + 12*nbinst
                    typ2 = 'COMB'
                else if (type .eq. 'SN_SITU') then
                    indicq = jthun + 6*nbinst
                    typ2 = 'SITU'
                else if (type .eq. 'SN*_SITU') then
                    indicq = jthun + 12*nbinst
                    typ2 = 'SITU'
                endif
                do 24 i1 = 1, 6
                    snth(i1) = zr(indicq+6+i1-1) -zr(indicq+i1-1)
 24             continue
                if (typ2 .eq. 'SITU') then
                    if (seisme) then
                        call rcZ2s0(mij, pij, mse,&
                                    nbinst, snth, sn)
                    else
                        call rcZ2st(sij, nbinst, snth, sn)
                    endif
                else
                    if (nbthep .ne. 0) then
                        do 114 i1 = 1, 6
                            sqmi(i1) = zr(indicp+i1-1) - zr(indicq+6+ i1-1)
                            sqma(i1) = zr(indicp+6+i1-1) - zr(indicq+ i1-1)
114                     continue
                        if (seisme) then
                            call rcZ2s0(mij, pij, mse,&
                                        nbinst, sqmi, sn1)
                            call rcZ2s0(mij, pij, mse,&
                                        nbinst, sqma, sn2)
                            sn = max( sn1, sn2 )
                        else
                            call rcZ2st(sij, nbinst, sqmi, sn1)
                            call rcZ2st(sij, nbinst, sqma, sn2)
                            sn = max( sn1, sn2 )
                        endif
                    endif
                endif
                snij = max( snij , sn )
110         continue
        endif
    endif
!
end subroutine
