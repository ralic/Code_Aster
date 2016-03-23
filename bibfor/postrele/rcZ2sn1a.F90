subroutine rcZ2sn1a(lieu, numsip, numsiq, seismeb32,&
                    instsn, sn1, sp3, spmeca3)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rctres.h"
#include "asterfort/jedema.h"
    character(len=4) :: lieu
    integer :: numsip, numsiq
    aster_logical :: seismeb32
    real(kind=8) :: sn1, sp3, spmeca3, instsn(2)
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
!     CALCUL DU SN OU SN* (partie B3200) par la méthode du TRESCA SIGNE
!
! IN  : LIEU   : ORIG ou EXTR
! IN  : NUMSIP : NUMERO SITUATION P
! IN  : NUMSIQ : NUMERO SITUATION Q
! IN  : SEISMEB32: SI SEISME AVEC B3200_T
! OUT : SN1    : PARTIE B3200 de SN
! OUT : SP3    : PARTIE B3200 de SP qui dépend des instants de SN
! OUT : SPMECA3: PARTIE B3200 de SPMECA qui dépend des instants de SN
!     ------------------------------------------------------------------
!
    character(len=2) :: typ
    character(len=8) :: knumes
    integer :: nbthep, nbprep, nbmecap, nbp, long, jtranp, indicp, ind2
    real(kind=8) :: sntran(6), tresca, sqmi(6), sqma(6), tresca1, tresca2
    integer :: nbtheq, nbpreq, nbmecaq, nbq, jtranq, indicq, jvalin, ind1
    real(kind=8) :: k1, c1, k2, c2, k3, c3, snther(6), snpres(6), snmec(6)
    real(kind=8) :: tresthpq, tresprpq, tresmecpq, e1(2), e2(2), e3(2)
    integer :: jseis, j, i1, i2, i3, i4, i5, i6, i0
    real(kind=8) :: e4(2), e5(2), e6(2), sntrans(6), sntrans1(6), sntrans2(6)
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sn1 = 0.d0
    typ ='00'
    instsn(1)=0.d0
    instsn(2)=0.d0
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
    endif
!
    do 2 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e1(i0) = i1
        e2(i0) = i1
        e3(i0) = i1
        e4(i0) = i1
        e5(i0) = i1
        e6(i0) = i1
 2  continue
!
!--------------------------------------
!     DANS LE CAS D'UNE SITUATION
!--------------------------------------
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
    nbp=max(nbthep,nbprep, nbmecap)
    if (nbp .ne. 0) then
        typ='PP'
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranp)
        instsn(1) = zr(jtranp+84)
        instsn(2) = zr(jtranp+85)
        indicp = jtranp + 6*2
        do 14 j = 1, 6
            sntran(j) = zr(indicp+6+j-1) -zr(indicp+j-1)
 14     continue
    else
        do 114 j = 1, 6
            sntran(j) = 0.d0
 114     continue
    endif
!
    if (seismeb32) then
        do 66 i1 = 1, 2
            do 56 i2 = 1, 2
                do 46 i3 = 1, 2
                    do 36 i4 = 1, 2
                        do 26 i5 = 1, 2
                            do 16 i6 = 1, 2
                                do 76 j = 1, 6
                                    sntrans(j) = sntran(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
76                              continue
                                call rctres(sntrans,tresca)
                                sn1 = max(sn1,tresca)
16                          continue
26                      continue
36                  continue
46              continue
56          continue
66      continue
    else
        call rctres(sntran,tresca)
        sn1 = tresca
    endif
!
    if (numsip .eq. numsiq) goto 888
!--------------------------------------
!     DANS LE CAS D'UNE COMBINAISON
!--------------------------------------
    knumes = 'S       '
    call codent(numsiq, 'D0', knumes(2:8))
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbpreq)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecaq)
    nbq = max(nbpreq,nbtheq, nbmecaq)
    if (nbq .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranq)
        indicq = jtranq + 6*2
        do 24 i1 = 1, 6
            sntran(i1) = zr(indicq+6+i1-1) -zr(indicq+i1-1)
 24     continue
        if (seismeb32) then
            do 65 i1 = 1, 2
                do 55 i2 = 1, 2
                    do 45 i3 = 1, 2
                        do 35 i4 = 1, 2
                            do 25 i5 = 1, 2
                                do 15 i6 = 1, 2
                                    do 75 j = 1, 6
                                        sntrans(j) = sntran(j)+e1(i1)*zr(jseis+j-1)+&
                                                    e2(i2)*zr(jseis+1*6+j-1)+&
                                                    e3(i3)*zr(jseis+2*6+j-1)+&
                                                    e4(i4)*zr(jseis+3*6+j-1)+&
                                                    e5(i5)*zr(jseis+4*6+j-1)+&
                                                    e6(i6)*zr(jseis+5*6+j-1)
75                                  continue
                                    call rctres(sntrans,tresca)
                                    if (tresca .gt. sn1) then
                                        sn1=tresca
                                        typ='QQ'
                                        instsn(1) = zr(jtranq+84)
                                        instsn(2) = zr(jtranq+85)
                                    endif
15                              continue
25                          continue
35                      continue
45                  continue
55              continue
65          continue
        else
            call rctres(sntran,tresca)
            if (tresca .gt. sn1) then
                sn1=tresca
                typ='QQ'
                instsn(1) = zr(jtranq+84)
                instsn(2) = zr(jtranq+85)
            endif
        endif
        if (nbp .ne. 0) then
            do 115 i1 = 1, 6
                sqmi(i1) = zr(indicp+i1-1) - zr(indicq+6+ i1-1)
                sqma(i1) = zr(indicp+6+i1-1) - zr(indicq+ i1-1)
115         continue
            if (seismeb32) then
                do 68 i1 = 1, 2
                    do 58 i2 = 1, 2
                        do 48 i3 = 1, 2
                            do 38 i4 = 1, 2
                                do 28 i5 = 1, 2
                                    do 18 i6 = 1, 2
                                        do 78 j = 1, 6
                                            sntrans1(j) = sqmi(j)+e1(i1)*zr(jseis+j-1)+&
                                                        e2(i2)*zr(jseis+1*6+j-1)+&
                                                        e3(i3)*zr(jseis+2*6+j-1)+&
                                                        e4(i4)*zr(jseis+3*6+j-1)+&
                                                        e5(i5)*zr(jseis+4*6+j-1)+&
                                                        e6(i6)*zr(jseis+5*6+j-1)
                                            sntrans2(j) = sqma(j)+e1(i1)*zr(jseis+j-1)+&
                                                        e2(i2)*zr(jseis+1*6+j-1)+&
                                                        e3(i3)*zr(jseis+2*6+j-1)+&
                                                        e4(i4)*zr(jseis+3*6+j-1)+&
                                                        e5(i5)*zr(jseis+4*6+j-1)+&
                                                        e6(i6)*zr(jseis+5*6+j-1)
78                                      continue
                                        call rctres(sntrans1,tresca1)
                                        call rctres(sntrans2,tresca2)
                                        if (tresca1 .gt. sn1) then
                                            sn1=tresca1
                                            typ='QP'
                                            instsn(1) = zr(jtranp+84) 
                                            instsn(2) = zr(jtranq+85) 
                                        endif
                                        if (tresca2 .gt. sn1) then
                                            sn1=tresca2
                                            typ='PQ'
                                            instsn(1) = zr(jtranq+84) 
                                            instsn(2) = zr(jtranp+85)
                                        endif
18                                  continue
28                              continue
38                          continue
48                      continue
58                  continue
68              continue
            else
                call rctres(sqmi,tresca1)
                call rctres(sqma,tresca2)
                if (tresca1 .gt. sn1) then
                    sn1=tresca1
                    typ='QP'
                    instsn(1) = zr(jtranp+84) 
                    instsn(2) = zr(jtranq+85) 
                endif
                if (tresca2 .gt. sn1) then
                    sn1=tresca2
                    typ='PQ'
                    instsn(1) = zr(jtranq+84) 
                    instsn(2) = zr(jtranp+85) 
                endif
            endif
        endif
    endif
!
888 continue
!
!-----------------------------------------------------------
!   CALCUL DE LA PARTIE SP3 (QUI DEPEND DES INSTANTS DE SN)
!-----------------------------------------------------------
    sp3 = 0.d0
    spmeca3 = 0.d0
!
    if (typ .eq. '00') goto 999
!
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
    k1 = zr(jvalin)
    c1 = zr(jvalin+1)
    k2 = zr(jvalin+2)
    c2 = zr(jvalin+3) 
    k3 = zr(jvalin+4)
    c3 = zr(jvalin+5)
!
    if (typ .eq. 'PP') then
        ind1 = jtranp
        ind2 = jtranp
    else if (typ .eq. 'QQ') then
        ind1 = jtranq
        ind2 = jtranq
    else if (typ .eq. 'PQ') then
        ind1 = jtranp
        ind2 = jtranq
    else if (typ .eq. 'QP') then
        ind1 = jtranq
        ind2 = jtranp
    endif
!
    do 34 i1 = 1, 6
        snther(i1) = zr(ind1+42+i1-1) -zr(ind2+36+i1-1)
        snpres(i1) = zr(ind1+54+i1-1) -zr(ind2+48+i1-1)
        snmec(i1) = zr(ind1+66+i1-1) -zr(ind2+60+i1-1)
 34 continue
    call rctres(snther,tresthpq)
    call rctres(snpres,tresprpq)
    call rctres(snmec,tresmecpq)
    sp3 = (k3*c3-1)*tresthpq+(k1*c1-1)*tresprpq+(k2*c2-1)*tresmecpq
    spmeca3 = (k1*c1-1)*tresprpq+(k2*c2-1)*tresmecpq
!
999 continue
!
    call jedema()
end subroutine
