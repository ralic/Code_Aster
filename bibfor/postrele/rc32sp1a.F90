subroutine rc32sp1a(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, mse, pi, mi, pj, mj,&
                    instsp, sp1, spmeca1, noth)
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
#include "asterfort/jeexin.h"
    character(len=4) :: lieu
    integer :: numsip, numsiq
    aster_logical :: ze200, seismeb32, seismeunit, noth
    real(kind=8) :: sp1(2), spmeca1(2), instsp(4), pi, pj, mi(12), mj(12)
    real(kind=8) :: mse(12)
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
    real(kind=8) :: trescapp, sptran(6), trescapq(2)
    character(len=8) :: knumes
    integer :: nbthep, nbprep, nbmecap, nbp, long, jtranp
    integer :: nbtheq, nbpreq, nbmecaq, nbq, jtranq, jseis
    real(kind=8) :: sqmi(6), sqma(6), tresca1, tresca2
    real(kind=8) :: spme(6), tresmepp
    real(kind=8) :: sqmemi(6), sqmema(6), tresme1
    real(kind=8) :: tresme2, tresmepq(2), instpmin, instpmax, instqmin, instqmax
    integer :: j, i1, i2, i3, i4, i5, i6, i0, iret, jsigu, icmps, icmp
    real(kind=8) :: e1(2), e2(2), e3(2), e4(2), e5(2), e6(2), sptrans(6)
    real(kind=8) :: spmes(6), trescame, tresca, sptrans1(6), sptrans2(6)
    real(kind=8) :: spmes1(6), spmes2(6), tresca1b, tresca2b, tresme1b, tresme2b
    real(kind=8) :: sij(6), mij(6), pij
    real(kind=8) :: sigu, e0(2), spt(6), spmet(6)
    real(kind=8) :: sqmit(6), sqmat(6), sqmemit(6), sqmemat(6)
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6)
    integer :: i7, i8, i9, i10, i11, i12
    character(len=2) :: typ
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    typ ='00'
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
        do 15 j = 1, 6
            seisfx(j) = zr(jseis+j-1)
            seisfy(j) = zr(jseis+1*6+j-1)
            seisfz(j) = zr(jseis+2*6+j-1)
            seismx(j) = zr(jseis+3*6+j-1)
            seismy(j) = zr(jseis+4*6+j-1)
            seismz(j) = zr(jseis+5*6+j-1)
            seisfx2(j)= 0.d0
            seisfy2(j)= 0.d0
            seisfz2(j)= 0.d0
            seismx2(j)= 0.d0
            seismy2(j)= 0.d0
            seismz2(j)= 0.d0
 15     continue
    endif
!
    do 2 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e0(i0) = i1
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
    noth =.false.
    trescapp = 0.d0
    tresmepp = 0.d0
!
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
!
!--------------------------------------
!     SI B3200 de type unitaire
!--------------------------------------
    do 17 j = 1, 6
            sij(j) = 0.d0
17 continue
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iret)
        if (iret .ne. 0) then
            call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
        do 30 icmps = 1, 6
            do 20 icmp = 1, 12
                mij(icmp)=mi(icmp)-mj(icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+icmps)
                sij(icmps) = sij(icmps) + mij(icmp)*sigu
 20         continue
            pij=pi-pj
            sigu = zr(jsigu-1+72+icmps)
            sij(icmps) = sij(icmps) + pij*sigu
 30     continue
!
        endif
    endif
!
    if(seismeunit) then
        do 19 j = 1, 6
            seisfx(j) = 2*mse(1)*zr(jsigu-1+j)
            seisfy(j) = 2*mse(2)*zr(jsigu-1+1*6+j)
            seisfz(j) = 2*mse(3)*zr(jsigu-1+2*6+j)
            seismx(j) = 2*mse(4)*zr(jsigu-1+3*6+j)
            seismy(j) = 2*mse(5)*zr(jsigu-1+4*6+j)
            seismz(j) = 2*mse(6)*zr(jsigu-1+5*6+j)
            seisfx2(j)= 2*mse(7)*zr(jsigu-1+6*6+j)
            seisfy2(j)= 2*mse(8)*zr(jsigu-1+7*6+j)
            seisfz2(j)= 2*mse(9)*zr(jsigu-1+8*6+j)
            seismx2(j)= 2*mse(10)*zr(jsigu-1+9*6+j)
            seismy2(j)= 2*mse(11)*zr(jsigu-1+10*6+j)
            seismz2(j)= 2*mse(12)*zr(jsigu-1+11*6+j)
 19     continue
    endif
!
!--------------------------------------
!   DANS LE CAS D'UNE SITUATION SEULE
!--------------------------------------
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
    nbp=max(nbthep,nbprep, nbmecap)
    if (nbp .ne. 0) then
        typ='PP'
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranp)
        do 14 i1 = 1, 6
            sptran(i1) = zr(jtranp+6+i1-1) -zr(jtranp+i1-1)
            spme(i1) = zr(jtranp+78+i1-1) -zr(jtranp+72+i1-1)
 14     continue
        instpmin = zr(jtranp+86)
        instpmax = zr(jtranp+87)
    else
        do 114 j = 1, 6
            sptran(j) = 0.d0
            spme(j) = 0.d0
 114     continue
        instpmin= -1.0
        instpmax = -1.0
    endif
!
    if (seismeb32 .or. seismeunit) then
        do 66 i1 = 1, 2
          do 56 i2 = 1, 2
            do 46 i3 = 1, 2
              do 36 i4 = 1, 2
                do 26 i5 = 1, 2
                  do 16 i6 = 1, 2
                    do 166 i7 = 1, 2
                      do 156 i8 = 1, 2
                        do 146 i9 = 1, 2
                          do 136 i10 = 1, 2
                            do 126 i11 = 1, 2
                              do 116 i12 = 1, 2
                                do 86 i0 = 1, 2
                                  do 76 j = 1, 6
                                      sptrans(j) = sptran(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
                                      spmes(j) = spme(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
76                                continue
                                  call rctres(sptrans,tresca)
                                  call rctres(spmes,trescame)
                                  trescapp = max(trescapp,tresca)
                                  tresmepp = max(tresmepp,trescame)
86                              continue
116                           continue
126                         continue
136                       continue
146                     continue
156                   continue
166                 continue
16                continue
26              continue
36            continue
46          continue
56        continue
66      continue
    else
        do 96 i0 = 1, 2
            do 106 j=1,6
                 spt(j)=sptran(j)+e0(i0)*sij(j)
                 spmet(j)=spme(j)+e0(i0)*sij(j)
        106 continue
            call rctres(spt,tresca)
            call rctres(spmet,trescame)
            trescapp = max(trescapp,tresca)
            tresmepp = max(tresmepp,trescame)
96      continue
    endif
    sp1(1)=trescapp
    spmeca1(1)=tresmepp
    instsp(1)= instpmin
    instsp(2)= instpmax
!
    if (numsip .eq. numsiq) goto 888
!
!--------------------------------------
!     DANS LE CAS D'UNE COMBINAISON
!--------------------------------------
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
        instqmin = zr(jtranq+86)
        instqmax = zr(jtranq+87)
!
        if (nbp .ne. 0) then
            typ='PQ'
            do 214 i1 = 1, 6
                sqmi(i1) = zr(jtranp+i1-1) - zr(jtranq+6+i1-1)
                sqma(i1) = zr(jtranp+6+i1-1) - zr(jtranq+i1-1)
                sqmemi(i1) = zr(jtranp+72+i1-1) - zr(jtranq+78+i1-1)
                sqmema(i1) = zr(jtranp+78+i1-1) - zr(jtranq+72+i1-1)
214         continue
!
        else
            typ='QQ'
            do 314 i1 = 1, 6
                sqmi(i1) = 0.d0
                sqma(i1) = zr(jtranq+6+i1-1) - zr(jtranq+i1-1)
                sqmemi(i1) = 0.d0
                sqmema(i1) = zr(jtranq+78+i1-1) - zr(jtranq+72+i1-1)
314         continue
            instsp(1) = zr(jtranq+86)
            instsp(2) = zr(jtranq+87)
            instsp(3) = -1.0
            instsp(4) = -1.0
        endif
    else
        if (nbp .ne. 0) then
            typ='PP'
            do 414 i1 = 1, 6
                sqmi(i1) = 0.d0
                sqma(i1) = zr(jtranp+6+i1-1) - zr(jtranp+i1-1)
                sqmemi(i1) = 0.d0
                sqmema(i1) = zr(jtranp+78+i1-1) - zr(jtranp+72+i1-1)
414         continue
            instsp(1) = zr(jtranp+86)
            instsp(2) = zr(jtranp+87)
            instsp(3) = -1.0
            instsp(4) = -1.0
        else
            typ='00'
            noth = .true.
            instsp(1) = -1.0
            instsp(2) = -1.0
            instsp(3) = -1.0
            instsp(4) = -1.0
            do 514 i1 = 1, 6
                sqmi(i1) = 0.d0
                sqma(i1) = 0.d0
                sqmemi(i1) = 0.d0
                sqmema(i1) = 0.d0
514         continue
        endif
    endif
!
    if (seismeb32 .or. seismeunit) then
        do 68 i1 = 1, 2
          do 58 i2 = 1, 2
            do 48 i3 = 1, 2
              do 38 i4 = 1, 2
                do 28 i5 = 1, 2
                  do 18 i6 = 1, 2
                    do 168 i7 = 1, 2
                      do 158 i8 = 1, 2
                        do 148 i9 = 1, 2
                          do 138 i10 = 1, 2
                            do 128 i11 = 1, 2
                              do 118 i12 = 1, 2
                                do 88 i0 = 1, 2
                                  do 78 j = 1, 6
                                      sptrans1(j) = sqmi(j)+e0(i0)*sij(j)+e1(i1)*seisfx(j)+&
                                               e2(i2)*seisfy(j)+e3(i3)*seisfz(j)+&
                                               e4(i4)*seismx(j)+e5(i5)*seismy(j)+&
                                               e6(i6)*seismz(j)+e1(i7)*seisfx2(j)+&
                                               e2(i8)*seisfy2(j)+e3(i9)*seisfz2(j)+&
                                               e4(i10)*seismx2(j)+e5(i11)*seismy2(j)+&
                                               e6(i12)*seismz2(j)
                                      sptrans2(j) = sqma(j)+e0(i0)*sij(j)+e1(i1)*seisfx(j)+&
                                               e2(i2)*seisfy(j)+e3(i3)*seisfz(j)+&
                                               e4(i4)*seismx(j)+e5(i5)*seismy(j)+&
                                               e6(i6)*seismz(j)+e1(i7)*seisfx2(j)+&
                                               e2(i8)*seisfy2(j)+e3(i9)*seisfz2(j)+&
                                               e4(i10)*seismx2(j)+e5(i11)*seismy2(j)+&
                                               e6(i12)*seismz2(j)
                                      spmes1(j) = sqmemi(j)+e0(i0)*sij(j)+e1(i1)*seisfx(j)+&
                                               e2(i2)*seisfy(j)+e3(i3)*seisfz(j)+&
                                               e4(i4)*seismx(j)+e5(i5)*seismy(j)+&
                                               e6(i6)*seismz(j)+e1(i7)*seisfx2(j)+&
                                               e2(i8)*seisfy2(j)+e3(i9)*seisfz2(j)+&
                                               e4(i10)*seismx2(j)+e5(i11)*seismy2(j)+&
                                               e6(i12)*seismz2(j)
                                      spmes2(j) = sqmema(j)+e0(i0)*sij(j)+e1(i1)*seisfx(j)+&
                                               e2(i2)*seisfy(j)+e3(i3)*seisfz(j)+&
                                               e4(i4)*seismx(j)+e5(i5)*seismy(j)+&
                                               e6(i6)*seismz(j)+e1(i7)*seisfx2(j)+&
                                               e2(i8)*seisfy2(j)+e3(i9)*seisfz2(j)+&
                                               e4(i10)*seismx2(j)+e5(i11)*seismy2(j)+&
                                               e6(i12)*seismz2(j)
78                                continue
                                  call rctres(sptrans1,tresca1b)
                                  call rctres(sptrans2,tresca2b)
                                  call rctres(spmes1,tresme1b)
                                  call rctres(spmes2,tresme2b)
                                  tresca1=max(tresca1,tresca1b)
                                  tresca2=max(tresca2,tresca2b)
                                  tresme1=max(tresme1,tresme1b)
                                  tresme2=max(tresme2,tresme2b)
88                              continue
118                           continue
128                         continue
138                       continue
148                     continue
158                   continue
168                 continue
18                continue
28              continue
38            continue
48          continue
58        continue
68      continue
!
    else
        do 98 i0 = 1, 2
            do 108 j=1,6
                sqmit(j)=sqmi(j)+e0(i0)*sij(j)
                sqmat(j)=sqma(j)+e0(i0)*sij(j)
                sqmemit(j)=sqmemi(j)+e0(i0)*sij(j)
                sqmemat(j)=sqmema(j)+e0(i0)*sij(j)
        108 continue
            call rctres(sqmit,tresca1b)
            call rctres(sqmat,tresca2b)
            call rctres(sqmemit,tresme1b)
            call rctres(sqmemat,tresme2b)
            tresca1=max(tresca1,tresca1b)
            tresca2=max(tresca2,tresca2b)
            tresme1=max(tresme1,tresme1b)
            tresme2=max(tresme2,tresme2b)
98      continue
    endif
!
    if (tresca2 .gt. tresca1) then
        trescapq(1) = tresca2
        trescapq(2) = tresca1
        if(typ .eq. 'PQ') then
            instsp(1) = instpmax
            instsp(2) = instqmin
            instsp(3) = instpmin
            instsp(4) = instqmax
        endif
    else
        trescapq(1) = tresca1
        trescapq(2) = tresca2
        if(typ .eq. 'PQ') then
            instsp(1) = instpmin
            instsp(2) = instqmax
            instsp(3) = instpmax
            instsp(4) = instqmin
        endif
     endif
!
    if (tresme2 .gt. tresme1) then
        tresmepq(1) = tresme2
        tresmepq(2) = tresme1
    else
        tresmepq(1) = tresme1
        tresmepq(2) = tresme2
     endif
!
    sp1(1) = trescapq(1)
    sp1(2) = trescapq(2)
    spmeca1(1) = tresmepq(1)
    spmeca1(2) = tresmepq(2)
!
888 continue
!
    call jedema()
end subroutine
