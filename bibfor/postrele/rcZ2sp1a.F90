subroutine rcZ2sp1a(lieu, numsip, numsiq,&
                    seismeb32, instsp, sp1, spmeca1)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/rctres.h"
#include "asterfort/jedema.h"
    character(len=4) :: lieu
    integer :: numsip, numsiq
    aster_logical :: seismeb32
    real(kind=8) :: sp1(2), spmeca1(2), instsp(4)
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
!     CALCUL DU SP avec la méthode TRESCA SIGNE
!
!     ------------------------------------------------------------------
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! OUT : SP1    : PARTIE B3200 du SP
!
    real(kind=8) :: trescapp, sptran(6), trescaqq, trescapq(2)
    character(len=8) :: knumes
    integer :: nbthep, nbprep, nbmecap, nbp, long, jtranp
    integer :: nbtheq, nbpreq, nbmecaq, nbq, jtranq, jseis
    real(kind=8) :: sqtran(6), sqmi(6), sqma(6), tresca1, tresca2
    real(kind=8) :: trescamax, spme(6), tresmepp
    real(kind=8) :: sqme(6), tresmeqq, sqmemi(6), sqmema(6), tresme1
    real(kind=8) :: tresme2, tresmepq(2), instp1, instp2, instq1, instq2
    integer :: j, i1, i2, i3, i4, i5, i6, i0
    real(kind=8) :: e1(2), e2(2), e3(2), e4(2), e5(2), e6(2), sptrans(6)
    real(kind=8) :: spmes(6), trescame, tresca, sptrans1(6), sptrans2(6)
    real(kind=8) :: spmes1(6), spmes2(6), tresca1b, tresca2b, tresme1b, tresme2b
    real(kind=8) :: instp1c, instp2c, instq1c, instq2c
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
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
    sp1(1) = 0.d0
    sp1(2) = 0.d0
    spmeca1(1) = 0.d0
    spmeca1(2) = 0.d0
    nbp = 0
    nbq = 0
    instsp(1) = 0.d0
    instsp(2) = 0.d0
    instsp(3) = 0.d0
    instsp(4) = 0.d0
!
    trescapp = 0.d0
    tresmepp = 0.d0
!
!--------------------------------------
!   DANS LE CAS D'UNE SITUATION SEULE
!--------------------------------------
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
    nbp=max(nbthep,nbprep, nbmecap)
    if (nbp .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranp)
        do 14 i1 = 1, 6
            sptran(i1) = zr(jtranp+6+i1-1) -zr(jtranp+i1-1)
            spme(i1) = zr(jtranp+78+i1-1) -zr(jtranp+72+i1-1)
 14     continue
        instp1 = zr(jtranp+86)
        instp2 = zr(jtranp+87)
        call rctres(sptran,trescapp)
        call rctres(spme,tresmepp)
    else
        do 114 j = 1, 6
            sptran(j) = 0.d0
            spme(j) = 0.d0
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
                                    sptrans(j) = sptran(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
                                    spmes(j) = spme(j)+e1(i1)*zr(jseis+j-1)+&
                                               e2(i2)*zr(jseis+1*6+j-1)+&
                                               e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                               e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
76                              continue
                                call rctres(sptrans,tresca)
                                call rctres(spmes,trescame)
                                trescapp = max(trescapp,tresca)
                                tresmepp = max(tresmepp,trescame)
16                          continue
26                      continue
36                  continue
46              continue
56          continue
66      continue
    else
        call rctres(sptran,trescapp)
        call rctres(spme,tresmepp)
    endif
    sp1(1)=trescapp
    spmeca1(1)=tresmepp
!
    if (numsip .eq. numsiq) goto 888
!
!--------------------------------------
!     DANS LE CAS D'UNE COMBINAISON
!--------------------------------------
    trescaqq = 0.d0
    tresmeqq = 0.d0
    trescapq = 0.d0
    tresca1 = 0.d0
    tresca2 = 0.d0
    tresme1 = 0.d0
    tresme2 = 0.d0
    knumes = 'S       '
    call codent(numsiq, 'D0', knumes(2:8))
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbpreq)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecaq)
    nbq = max(nbpreq,nbtheq, nbmecaq)
    if (nbq .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranq)
        do 24 i1 = 1, 6
            sqtran(i1) = zr(jtranq+6+i1-1) -zr(jtranq+i1-1)
            sqme(i1) = zr(jtranq+78+i1-1) -zr(jtranq+72+i1-1)
 24     continue
        instq1 = zr(jtranq+86)
        instq2 = zr(jtranq+87)
        if (seismeb32) then
            do 166 i1 = 1, 2
                do 156 i2 = 1, 2
                    do 146 i3 = 1, 2
                        do 136 i4 = 1, 2
                            do 126 i5 = 1, 2
                                do 116 i6 = 1, 2
                                    do 176 j = 1, 6
                                        sptrans(j) = sqtran(j)+e1(i1)*zr(jseis+j-1)+&
                                                    e2(i2)*zr(jseis+1*6+j-1)+&
                                                    e3(i3)*zr(jseis+2*6+j-1)+&
                                                    e4(i4)*zr(jseis+3*6+j-1)+&
                                                    e5(i5)*zr(jseis+4*6+j-1)+&
                                                    e6(i6)*zr(jseis+5*6+j-1)
                                        spmes(j) = sqme(j)+e1(i1)*zr(jseis+j-1)+&
                                                   e2(i2)*zr(jseis+1*6+j-1)+&
                                                   e3(i3)*zr(jseis+2*6+j-1)+&
                                                   e4(i4)*zr(jseis+3*6+j-1)+&
                                                   e5(i5)*zr(jseis+4*6+j-1)+&
                                                   e6(i6)*zr(jseis+5*6+j-1)
176                                 continue
                                    call rctres(sptrans,tresca)
                                    call rctres(spmes,trescame)
                                    trescaqq = max(trescaqq,tresca)
                                    tresmeqq = max(tresmeqq,trescame)
116                             continue
126                         continue
136                     continue
146                 continue
156             continue
166         continue
        else
            call rctres(sqtran,trescaqq)
            call rctres(sqme,tresmeqq)
        endif
        if (nbp .ne. 0) then
            do 214 i1 = 1, 6
                sqmi(i1) = zr(jtranp+i1-1) - zr(jtranq+6+i1-1)
                sqma(i1) = zr(jtranp+6+i1-1) - zr(jtranq+i1-1)
                sqmemi(i1) = zr(jtranp+72+i1-1) - zr(jtranq+78+i1-1)
                sqmema(i1) = zr(jtranp+78+i1-1) - zr(jtranq+72+i1-1)
214         continue
            if (seismeb32) then
                do 266 i1 = 1, 2
                    do 256 i2 = 1, 2
                        do 246 i3 = 1, 2
                            do 236 i4 = 1, 2
                                do 226 i5 = 1, 2
                                    do 216 i6 = 1, 2
                                        do 276 j = 1, 6
                                            sptrans1(j) = sqmi(j)+e1(i1)*zr(jseis+j-1)+&
                                                          e2(i2)*zr(jseis+1*6+j-1)+&
                                                          e3(i3)*zr(jseis+2*6+j-1)+&
                                                          e4(i4)*zr(jseis+3*6+j-1)+&
                                                          e5(i5)*zr(jseis+4*6+j-1)+&
                                                          e6(i6)*zr(jseis+5*6+j-1)
                                            sptrans2(j) = sqma(j)+e1(i1)*zr(jseis+j-1)+&
                                                          e2(i2)*zr(jseis+1*6+j-1)+&
                                                          e3(i3)*zr(jseis+2*6+j-1)+&
                                                          e4(i4)*zr(jseis+3*6+j-1)+&
                                                          e5(i5)*zr(jseis+4*6+j-1)+&
                                                          e6(i6)*zr(jseis+5*6+j-1)
                                            spmes1(j) = sqmemi(j)+e1(i1)*zr(jseis+j-1)+&
                                                        e2(i2)*zr(jseis+1*6+j-1)+&
                                                        e3(i3)*zr(jseis+2*6+j-1)+&
                                                        e4(i4)*zr(jseis+3*6+j-1)+&
                                                        e5(i5)*zr(jseis+4*6+j-1)+&
                                                        e6(i6)*zr(jseis+5*6+j-1)
                                            spmes2(j) = sqmema(j)+e1(i1)*zr(jseis+j-1)+&
                                                        e2(i2)*zr(jseis+1*6+j-1)+&
                                                        e3(i3)*zr(jseis+2*6+j-1)+&
                                                        e4(i4)*zr(jseis+3*6+j-1)+&
                                                        e5(i5)*zr(jseis+4*6+j-1)+&
                                                        e6(i6)*zr(jseis+5*6+j-1)
276                                     continue
                                        call rctres(sptrans1,tresca1b)
                                        call rctres(sptrans2,tresca2b)
                                        call rctres(spmes1,tresme1b)
                                        call rctres(spmes2,tresme2b)
                                        tresca1=max(tresca1,tresca1b)
                                        tresca2=max(tresca2,tresca2b)
                                        tresme1=max(tresme1,tresme1b)
                                        tresme2=max(tresme2,tresme2b)
216                                 continue
226                             continue
236                         continue
246                     continue
256                 continue
266             continue
                if (tresca2 .gt. tresca1) then
                    instp1c = zr(jtranp+87)
                    instp2c = zr(jtranq+86)
                    instq1c = zr(jtranp+86)
                    instq2c = zr(jtranq+87)
                    trescapq(1) = tresca2
                    trescapq(2) = tresca1
                    tresmepq(1) = tresme2
                    tresmepq(2) = tresme1
                else
                    instp1c = zr(jtranp+86)
                    instp2c = zr(jtranq+87)
                    instq1c = zr(jtranp+87)
                    instq2c = zr(jtranq+86)
                    tresmepq(1) = tresca1
                    tresmepq(2) = tresca2
                    tresmepq(1) = tresme1
                    tresmepq(2) = tresme2
                endif
            else
                call rctres(sqmi,tresca1)
                call rctres(sqma,tresca2)
                call rctres(sqmemi,tresme1)
                call rctres(sqmema,tresme2)
                if (tresca2 .gt. tresca1) then
                    trescapq(1) = tresca2
                    trescapq(2) = tresca1
                    tresmepq(1) = tresme2
                    tresmepq(2) = tresme1
                    instp1c = zr(jtranp+87)
                    instp2c = zr(jtranq+86)
                    instq1c = zr(jtranp+86)
                    instq2c = zr(jtranq+87)
                else
                    instp1c = zr(jtranp+86)
                    instp2c = zr(jtranq+87)
                    instq1c = zr(jtranp+87)
                    instq2c = zr(jtranq+86)
                    tresmepq(1) = tresca1
                    tresmepq(2) = tresca2
                    tresmepq(1) = tresme1
                    tresmepq(2) = tresme2
                endif
            endif
        endif
    endif
!
    trescamax=max(trescapp, trescaqq, trescapq(1))
    if(trescamax .eq. trescapp) then
        sp1(1) = trescapp
        sp1(2) = trescaqq
        spmeca1(1) = tresmepp
        spmeca1(2) = tresmeqq
        if (nbp .ne. 0) then
            instsp(1)=instp1
            instsp(2)=instp2
        endif
        if (nbq .ne. 0) then
            instsp(3)=instq1
            instsp(4)=instq2
        endif
    else if (trescamax .eq. trescaqq) then
        sp1(1) = trescaqq
        sp1(2) = trescapp
        spmeca1(1) = tresmeqq
        spmeca1(2) = tresmepp
        if (nbp .ne. 0) then
            instsp(1)=instp1
            instsp(2)=instp2
        endif
        if (nbq .ne. 0) then
            instsp(3)=instq1
            instsp(4)=instq2
        endif
    else if (trescamax .eq. trescapq(1)) then
        sp1(1) = trescapq(1)
        sp1(2) = trescapq(2)
        spmeca1(1) = tresmepq(1)
        spmeca1(2) = tresmepq(2)
        if (nbq*nbp .ne. 0) then
            instsp(1)=instp1c
            instsp(2)=instq1c
            instsp(3)=instp2c
            instsp(4)=instq2c
        endif
    endif
!
888 continue
!
    sp1(1) = sp1(1)
    sp1(2) = sp1(2)
    spmeca1(1) = spmeca1(1)
    spmeca1(2) = spmeca1(2)
!
    call jedema()
end subroutine
