subroutine rc32sp1a(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                    instsp, sp1, spme, mat1, mat2)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/jelira.h"
#include "asterfort/rctres.h"
#include "asterfort/jedema.h"
    aster_logical :: ze200, seismeb32, seismeunit, seismeze200
    character(len=4) :: lieu
    integer :: numsip, numsiq
    real(kind=8) :: mse(12), propi(20), propj(20), proqi(20)
    real(kind=8) :: proqj(20), instsp(4), sp1(2), spme(2)
    real(kind=8) :: mat1(7), mat2(7)
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
! OUT : SP1    
!
    integer :: k, jseis, j, iret, jsigu, icmp, nbthep, nbprep, nbmecap, nbp
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), sppp, sp2pp, trescapp, tresmepp
    character(len=8) :: knumes
    character(len=2) :: typ
    real(kind=8) :: sij(6), mij(12), sigu, pij, instpmin, instpmax, e0(2)
    integer :: long, jtranp, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10
    real(kind=8) :: e1(2), sptran(6), spm(6), spt(6), spmec(6), tresca
    integer :: i11, i12, nbtheq, nbpreq, nbmecaq, nbq, jtranq, kk
    real(kind=8) :: spqq, sp2qq, trescaqq, tresmeqq, sqtran(6), sqme(6)
    real(kind=8) :: instqmin, instqmax, sqt(6), sqmec(6), sp2pq(2)
    real(kind=8) ::  trescapq(2), tresmepq(2), sp2, sij1(6), sij2(6)
    real(kind=8) :: sij3(6), sij4(6), mij1(12), mij2(12), mij3(12), mij4(12)
    real(kind=8) :: pij1, pij2, pij3, pij4, smi(6), sma(6), smemi(6)
    real(kind=8) :: smema(6), instbid(4), tresca1(4), tresca2(4), tresme1(4)
    real(kind=8) :: tresme2(4), seis(6), st11(6), st12(6), st13(6), st14(6)
    real(kind=8) :: st21(6), st22(6), st23(6), st24(6), smec11(6), smec12(6)
    real(kind=8) :: smec13(6), smec14(6), smec21(6), smec22(6), smec23(6)
    real(kind=8) :: smec24(6), sppq(2)
    aster_logical :: tres
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sp1(1)  = 0.d0
    sp1(2)  = 0.d0
    spme(1) = 0.d0
    spme(2) = 0.d0
    do 10 k = 1, 7
        mat1(k) = propi(13+k)
        mat2(k) = propj(13+k)
10  continue
!
!--------------------------------------------
!     SI SEISME ET B3200 de type instantanée
!--------------------------------------------
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
        do 20 j = 1, 6
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
20      continue
    endif
!
!--------------------------------------
!     SI SEISME B3200 de type unitaire
!--------------------------------------
    iret  = 0
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iret)
        if (iret .ne. 0) then
            call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
        endif
    endif
!
    if(seismeunit) then
        do 30 j = 1, 6
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
30      continue
    endif
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE SITUATION SEULE
!                          CALCUL DE SP(P,P)
!--------------------------------------------------------------------
    typ   = '00'
    sppp  = 0.d0
    sp2pp = 0.d0
    trescapp = 0.d0
    tresmepp = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) call rcZ2s2('SP', propi, propj, seismeze200, mse, sp2pp)
!
!-- on calcule la partie b3200_unitaire
    do 40 j = 1, 6
        sij(j) = 0.d0
40  continue
    if (iret .ne. 0) then
        do 50 j = 1, 6
            do 60 icmp = 1, 12
                mij(icmp) = propi(1+icmp)-propj(1+icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sij(j) = sij(j)+mij(icmp)*sigu
60          continue
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+72+j)
            sij(j) = sij(j)+pij*sigu
50      continue
    endif
!
!-- on calcule la partie b3200_transitoire
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
    nbp = max(nbthep,nbprep, nbmecap)
    if (nbp .ne. 0) then
        typ='PP'
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranp)
        do 70 j = 1, 6
            sptran(j) = zr(jtranp+6+j-1) -zr(jtranp+j-1)
            spm(j)   = zr(jtranp+78+j-1)-zr(jtranp+72+j-1)
70      continue
        instpmin = zr(jtranp+86)
        instpmax = zr(jtranp+87)
    else
        do 80 j = 1, 6
            sptran(j) = 0.d0
            spm(j)   = 0.d0
80      continue
        instpmin = -1.0
        instpmax = -1.0
    endif
!
!-- on maximise sur les différentes possibilités de signes
    do 90 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e0(i0) = i1
        e1(i0) = i1
90  continue
!
    if (seismeb32 .or. seismeunit) then
        do 100 i1 = 1, 2
          do 101 i2 = 1, 2
            do 102 i3 = 1, 2
              do 103 i4 = 1, 2
                do 104 i5 = 1, 2
                  do 105 i6 = 1, 2
                    do 106 i7 = 1, 2
                      do 107 i8 = 1, 2
                        do 108 i9 = 1, 2
                          do 109 i10 = 1, 2
                            do 110 i11 = 1, 2
                              do 111 i12 = 1, 2
                                do 112 i0 = 1, 2
                                  do 113 j = 1, 6
                                      spt(j) =  sptran(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
                                      spmec(j) =  spm(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
113                               continue
                                  call rctres(spt,tresca)
                                  if (tresca .gt. trescapp) then
                                      trescapp = tresca
                                      call rctres(spmec,tresmepp)
                                  endif
112                             continue
111                           continue
110                         continue
109                       continue
108                     continue
107                   continue
106                 continue
105               continue
104             continue
103           continue
102         continue
101       continue
100     continue
    else
        do 114 i0 = 1, 2
            do 115 j=1,6
                spt(j)  = sptran(j)+e0(i0)*sij(j)
                spmec(j)= spm(j)+e0(i0)*sij(j)
        115 continue
            call rctres(spt,tresca)
            if (tresca .gt. trescapp) then
                trescapp = tresca
                call rctres(spmec,tresmepp)
            endif
114      continue
    endif
!
    sppp    = trescapp+sp2pp
    sp1(1)  = sppp
    spme(1) = tresmepp+sp2pp
    instsp(1) = instpmin
    instsp(2) = instpmax
    instsp(3) = -1.d0
    instsp(4) = -1.d0
!
    if (numsip .eq. numsiq) goto 888
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE COMBINAISON DE
!                          SITUATIONS P ET Q
!--------------------------------------------------------------------
!--------------------- CALCUL DE SP(Q,Q)-----------------------------
    spqq  = 0.d0
    sp2qq = 0.d0
    trescaqq = 0.d0
    tresmeqq = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) call rcZ2s2('SP', proqi, proqj, seismeze200, mse, sp2qq)
!
!-- on calcule la partie b3200_unitaire
    do 120 j = 1, 6
        sij(j) = 0.d0
120 continue
    if (iret .ne. 0) then
        do 130 j = 1, 6
            do 140 icmp = 1, 12
                mij(icmp) = proqi(1+icmp)-proqj(1+icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sij(j) = sij(j)+mij(icmp)*sigu
140         continue
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+72+j)
            sij(j) = sij(j)+pij*sigu
130     continue
    endif
!
!-- on calcule la partie b3200_transitoire
    knumes = 'S       '
    call codent(numsiq, 'D0', knumes(2:8))
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbpreq)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecaq)
    nbq = max(nbtheq,nbpreq, nbmecaq)
    if (nbq .ne. 0) then
        typ='QQ'
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranq)
        do 150 j = 1, 6
            sqtran(j) = zr(jtranq+6+j-1) -zr(jtranq+j-1)
            sqme(j)   = zr(jtranq+78+j-1)-zr(jtranq+72+j-1)
150     continue
        instqmin = zr(jtranq+86)
        instqmax = zr(jtranq+87)
    else
        do 160 j = 1, 6
            sqtran(j) = 0.d0
            sqme(j) = 0.d0
160     continue
        instqmin= -1.0
        instqmax = -1.0
    endif
!
!-- on maximise sur les possibilités de signes
    if (seismeb32 .or. seismeunit) then
        do 170 i1 = 1, 2
          do 171 i2 = 1, 2
            do 172 i3 = 1, 2
              do 173 i4 = 1, 2
                do 174 i5 = 1, 2
                  do 175 i6 = 1, 2
                    do 176 i7 = 1, 2
                      do 177 i8 = 1, 2
                        do 178 i9 = 1, 2
                          do 179 i10 = 1, 2
                            do 180 i11 = 1, 2
                              do 181 i12 = 1, 2
                                do 182 i0 = 1, 2
                                  do 183 j = 1, 6
                                      sqt(j) =  sqtran(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
                                      sqmec(j) =  sqme(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
183                               continue
                                  call rctres(sqt,tresca)
                                  if (tresca .gt. trescaqq) then
                                      trescaqq = tresca
                                      call rctres(sqmec,tresmeqq)
                                  endif
182                             continue
181                           continue
180                         continue
179                       continue
178                     continue
177                   continue
176                 continue
175               continue
174             continue
173           continue
172         continue
171       continue
170     continue
    else
        do 184 i0 = 1, 2
            do 185 j=1,6
                sqt(j)  = sqtran(j)+e0(i0)*sij(j)
                sqmec(j)= sqme(j)+e0(i0)*sij(j)
        185 continue
            call rctres(sqt,tresca)
            if (tresca .gt. trescaqq) then
                trescaqq = tresca
                call rctres(sqmec,tresmeqq)
            endif
184      continue
    endif
!
    spqq = trescaqq+sp2qq
!
! -- Comparaison de SP(P,P) et SP(Q,Q)
    if (spqq .gt. sppp) then
        sp1(1)  = spqq
        sp1(2)  = sppp
        spme(1) = tresmeqq+sp2qq
        spme(2) = tresmepp+sp2pp
        instsp(1)= instqmin
        instsp(2)= instqmax
        instsp(3)= instpmin
        instsp(4)= instpmax
        do 190 k = 1, 7
            mat1(k) = proqi(13+k)
            mat2(k) = proqj(13+k)
190     continue
    else
        sp1(2)  = spqq
        spme(2) = tresmeqq+sp2qq
        instsp(3)= instqmin
        instsp(4)= instqmax
    endif
!
!--------------------- CALCUL DE SP(P,Q)-----------------------------
    sp2pq(1) = 0.d0
    sp2pq(2) = 0.d0
    sppq(1) = 0.d0
    sppq(2) = 0.d0
    trescapq(1) = 0.d0
    trescapq(2) = 0.d0
    tresmepq(1) = 0.d0
    tresmepq(2) = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) then
        call rcZ2s2('SP', propi, proqi, seismeze200, mse, sp2pq(1))
        call rcZ2s2('SP', propj, proqj, seismeze200, mse, sp2)
        if (sp2 .gt. sp2pq(1)) then
            sp2pq(2) = sp2pq(1)
            sp2pq(1) = sp2
        endif
        call rcZ2s2('SP', propi, proqj, seismeze200, mse, sp2)
        if (sp2 .gt. sp2pq(1)) then
            sp2pq(1) = sp2
            call rcZ2s2('SP', propj, proqi, seismeze200, mse, sp2pq(2))
        endif
        call rcZ2s2('SP', propj, proqi, seismeze200, mse, sp2)
        if (sp2 .gt. sp2pq(1)) then
            sp2pq(1) = sp2
            call rcZ2s2('SP', propi, proqj, seismeze200, mse, sp2pq(2))
        endif
    endif
!
!-- on calcule la partie b3200_unitaire
    do 200 j = 1, 6
        sij1(j) = 0.d0
        sij2(j) = 0.d0
        sij3(j) = 0.d0
        sij4(j) = 0.d0
200 continue
    if (iret .ne. 0) then
        do 205 j = 1, 6
            do 210 icmp = 1, 12
                mij1(icmp) = propi(1+icmp)-proqi(1+icmp)
                mij2(icmp) = propj(1+icmp)-proqj(1+icmp)
                mij3(icmp) = propi(1+icmp)-proqj(1+icmp)
                mij4(icmp) = propj(1+icmp)-proqi(1+icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sij1(j) = sij1(j) + mij1(icmp)*sigu
                sij2(j) = sij2(j) + mij2(icmp)*sigu
                sij3(j) = sij3(j) + mij3(icmp)*sigu
                sij4(j) = sij4(j) + mij4(icmp)*sigu
210         continue
            pij1 = propi(1)-proqi(1)
            pij2 = propj(1)-proqj(1)
            pij3 = propi(1)-proqj(1)
            pij4 = propj(1)-proqi(1)
            sigu = zr(jsigu-1+72+j)
            sij1(j) = sij1(j) + pij1*sigu
            sij2(j) = sij2(j) + pij2*sigu
            sij3(j) = sij3(j) + pij3*sigu
            sij4(j) = sij4(j) + pij4*sigu
205     continue
    endif
!
!-- on calcule la partie b3200_transitoire
    if (nbq .ne. 0) then
        if (nbp .ne. 0) then
            typ='PQ'
            do 211 j = 1, 6
                smi(j)   = zr(jtranp+j-1)    - zr(jtranq+6+j-1)
                sma(j)   = zr(jtranp+6+j-1)  - zr(jtranq+j-1)
                smemi(j) = zr(jtranp+72+j-1) - zr(jtranq+78+j-1)
                smema(j) = zr(jtranp+78+j-1) - zr(jtranq+72+j-1)
211         continue
        else
            typ='QQ'
            do 212 j = 1, 6
                smi(j)   = 0.d0
                sma(j)   = zr(jtranq+6+j-1) - zr(jtranq+j-1)
                smemi(j) = 0.d0
                smema(j) = zr(jtranq+78+j-1)- zr(jtranq+72+j-1)
212         continue
            instbid(1) = instqmin
            instbid(2) = instqmax
            instbid(3) = -1.0
            instbid(4) = -1.0
        endif
    else
        if (nbp .ne. 0) then
            typ='PP'
            do 213 j = 1, 6
                smi(j)   = 0.d0
                sma(j)   = zr(jtranp+6+j-1) - zr(jtranp+j-1)
                smemi(j) = 0.d0
                smema(j) = zr(jtranp+78+j-1)- zr(jtranp+72+j-1)
213         continue
            instbid(1) = instpmin
            instbid(2) = instpmax
            instbid(3) = -1.0
            instbid(4) = -1.0
        else
            typ='00'
            do 214 j = 1, 6
                smi(j) = 0.d0
                sma(j) = 0.d0
                smemi(j) = 0.d0
                smema(j) = 0.d0
214         continue
            instbid(1) = -1.0
            instbid(2) = -1.0
            instbid(3) = -1.0
            instbid(4) = -1.0
        endif
    endif
!
!-- on maximise sur les possibilités de signes
    tresca1(1) = 0.d0
    tresca1(2) = 0.d0
    tresca1(3) = 0.d0
    tresca1(4) = 0.d0
    tresca2(1) = 0.d0
    tresca2(2) = 0.d0
    tresca2(3) = 0.d0
    tresca2(4) = 0.d0
    tresme1(1) = 0.d0
    tresme1(2) = 0.d0
    tresme1(3) = 0.d0
    tresme1(4) = 0.d0
    tresme2(1) = 0.d0
    tresme2(2) = 0.d0
    tresme2(3) = 0.d0
    tresme2(4) = 0.d0
    if (seismeb32 .or. seismeunit) then
        do 220 i1 = 1, 2
          do 221 i2 = 1, 2
            do 222 i3 = 1, 2
              do 223 i4 = 1, 2
                do 224 i5 = 1, 2
                  do 225 i6 = 1, 2
                    do 226 i7 = 1, 2
                      do 227 i8 = 1, 2
                        do 228 i9 = 1, 2
                          do 229 i10 = 1, 2
                            do 230 i11 = 1, 2
                              do 231 i12 = 1, 2
                                do 232 i0 = 1, 2
                                  do 233 j = 1, 6
                                      seis(j)= e1(i1)*seisfx(j)+e1(i2)*seisfy(j)+&
                                               e1(i3)*seisfz(j)+e1(i4)*seismx(j)+&
                                               e1(i5)*seismy(j)+e1(i6)*seismz(j)+&
                                               e1(i7)*seisfx2(j)+e1(i8)*seisfy2(j)+&
                                               e1(i9)*seisfz2(j)+e1(i10)*seismx2(j)+&
                                               e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
                                      st11(j) = smi(j)+e0(i0)*sij1(j)+seis(j)
                                      st12(j) = smi(j)+e0(i0)*sij2(j)+seis(j)
                                      st13(j) = smi(j)+e0(i0)*sij3(j)+seis(j)
                                      st14(j) = smi(j)+e0(i0)*sij4(j)+seis(j)
                                      st21(j) = sma(j)+e0(i0)*sij1(j)+seis(j)
                                      st22(j) = sma(j)+e0(i0)*sij2(j)+seis(j)
                                      st23(j) = sma(j)+e0(i0)*sij3(j)+seis(j)
                                      st24(j) = sma(j)+e0(i0)*sij4(j)+seis(j)
                                      smec11(j) = smemi(j)+e0(i0)*sij1(j)+seis(j)
                                      smec12(j) = smemi(j)+e0(i0)*sij2(j)+seis(j)
                                      smec13(j) = smemi(j)+e0(i0)*sij3(j)+seis(j)
                                      smec14(j) = smemi(j)+e0(i0)*sij4(j)+seis(j)
                                      smec21(j) = smema(j)+e0(i0)*sij1(j)+seis(j)
                                      smec22(j) = smema(j)+e0(i0)*sij2(j)+seis(j)
                                      smec23(j) = smema(j)+e0(i0)*sij3(j)+seis(j)
                                      smec24(j) = smema(j)+e0(i0)*sij4(j)+seis(j)
233                               continue
!
                                  call rctres(st11,tresca)
                                  if (tresca .gt. tresca1(1)) then
                                      tresca1(1) = tresca
                                      call rctres(smec11,tresme1(1))
                                  endif
                                  call rctres(st12,tresca)
                                  if (tresca .gt. tresca1(2)) then
                                      tresca1(2) = tresca
                                      call rctres(smec12,tresme1(2))
                                  endif
                                  call rctres(st13,tresca)
                                  if (tresca .gt. tresca1(3)) then
                                      tresca1(3) = tresca
                                      call rctres(smec13,tresme1(3))
                                  endif
                                  call rctres(st14,tresca)
                                  if (tresca .gt. tresca1(4)) then
                                      tresca1(4) = tresca
                                      call rctres(smec14,tresme1(4))
                                  endif
!
                                  call rctres(st21,tresca)
                                  if (tresca .gt. tresca2(1)) then
                                      tresca2(1) = tresca
                                      call rctres(smec21,tresme2(1))
                                  endif
                                  call rctres(st22,tresca)
                                  if (tresca .gt. tresca2(2)) then
                                      tresca2(2) = tresca
                                      call rctres(smec22,tresme2(2))
                                  endif
                                  call rctres(st23,tresca)
                                  if (tresca .gt. tresca2(3)) then
                                      tresca2(3) = tresca
                                      call rctres(smec23,tresme2(3))
                                  endif
                                  call rctres(st24,tresca)
                                  if (tresca .gt. tresca2(4)) then
                                      tresca2(4) = tresca
                                      call rctres(smec24,tresme2(4))
                                  endif
!
232                             continue
231                           continue
230                         continue
229                       continue
228                     continue
227                   continue
226                 continue
225               continue
224             continue
223           continue
222         continue
221       continue
220     continue
    else
        do 234 i0 = 1, 2
            do 235 j=1,6
                st11(j)=smi(j)+e0(i0)*sij1(j)
                st12(j)=smi(j)+e0(i0)*sij2(j)
                st13(j)=smi(j)+e0(i0)*sij3(j)
                st14(j)=smi(j)+e0(i0)*sij4(j)
                st21(j)=sma(j)+e0(i0)*sij1(j)
                st22(j)=sma(j)+e0(i0)*sij2(j)
                st23(j)=sma(j)+e0(i0)*sij3(j)
                st24(j)=sma(j)+e0(i0)*sij4(j)
                smec11(j)=smemi(j)+e0(i0)*sij1(j)
                smec12(j)=smemi(j)+e0(i0)*sij2(j)
                smec13(j)=smemi(j)+e0(i0)*sij3(j)
                smec14(j)=smemi(j)+e0(i0)*sij4(j)
                smec21(j)=smema(j)+e0(i0)*sij1(j)
                smec22(j)=smema(j)+e0(i0)*sij2(j)
                smec23(j)=smema(j)+e0(i0)*sij3(j)
                smec24(j)=smema(j)+e0(i0)*sij4(j)
235         continue
            call rctres(st11,tresca)
            if (tresca .gt. tresca1(1)) then
                tresca1(1) = tresca
                call rctres(smec11,tresme1(1))
            endif
            call rctres(st12,tresca)
            if (tresca .gt. tresca1(2)) then
                tresca1(2) = tresca
                call rctres(smec12,tresme1(2))
            endif
            call rctres(st13,tresca)
            if (tresca .gt. tresca1(3)) then
                tresca1(3) = tresca
                call rctres(smec13,tresme1(3))
            endif
            call rctres(st14,tresca)
            if (tresca .gt. tresca1(4)) then
                tresca1(4) = tresca
                call rctres(smec14,tresme1(4))
            endif
!
            call rctres(st21,tresca)
            if (tresca .gt. tresca2(1)) then
                tresca2(1) = tresca
                call rctres(smec21,tresme2(1))
            endif
            call rctres(st22,tresca)
            if (tresca .gt. tresca2(2)) then
                tresca2(2) = tresca
                call rctres(smec22,tresme2(2))
            endif
            call rctres(st23,tresca)
            if (tresca .gt. tresca2(3)) then
                tresca2(3) = tresca
                call rctres(smec23,tresme2(3))
            endif
            call rctres(st24,tresca)
            if (tresca .gt. tresca2(4)) then
                tresca2(4) = tresca
                call rctres(smec24,tresme2(4))
            endif
!
234     continue
    endif
!
! -- Comparaison de SP(P,Q) et SP(Q,P)
    if(typ .eq. 'PQ') then
       instbid(1) = instpmin
       instbid(2) = instqmax
       instbid(3) = instpmax
       instbid(4) = instqmin
    endif
!
    tres = .true.
    if (tresca1(1) .gt. trescapq(1)) then
        trescapq(1) = tresca1(1)
        tresmepq(1) = tresme1(1)
        do 240 k = 1, 7
            mat1(k) = propi(13+k)
            mat2(k) = proqi(13+k)
240     continue
    endif 
    if (tresca1(2) .gt. trescapq(1)) then
        trescapq(1) = tresca1(2)
        tresmepq(1) = tresme1(2)
        do 250 k = 1, 7
            mat1(k) = propj(13+k)
            mat2(k) = proqj(13+k)
250     continue
    endif
    if (tresca1(3) .gt. trescapq(1)) then
        trescapq(1) = tresca1(3)
        tresmepq(1) = tresme1(3)
        do 260 k = 1, 7
            mat1(k) = propi(13+k)
            mat2(k) = proqj(13+k)
260     continue
    endif
    if (tresca1(4) .gt. trescapq(1)) then
        trescapq(1) = tresca1(4)
        tresmepq(1) = tresme1(4)
        do 270 k = 1, 7
            mat1(k) = propj(13+k)
            mat2(k) = proqi(13+k)
270     continue
    endif
!
    if (tresca2(1) .gt. trescapq(1)) then
        tres = .false.
        if(typ .eq. 'PQ') then
            instbid(1) = instpmax
            instbid(2) = instqmin
            instbid(3) = instpmin
            instbid(4) = instqmax
        endif
        trescapq(1) = tresca2(1)
        tresmepq(1) = tresme2(1)
        do 280 k = 1, 7
            mat1(k) = propi(13+k)
            mat2(k) = proqi(13+k)
280     continue
    endif 
    if (tresca2(2) .gt. trescapq(1)) then
        tres = .false.
        if(typ .eq. 'PQ') then
            instbid(1) = instpmax
            instbid(2) = instqmin
            instbid(3) = instpmin
            instbid(4) = instqmax
        endif
        trescapq(1) = tresca2(2)
        tresmepq(1) = tresme2(2)
        do 290 k = 1, 7
            mat1(k) = propj(13+k)
            mat2(k) = proqj(13+k)
290     continue
    endif
    if (tresca2(3) .gt. trescapq(1)) then
        tres = .false.
        if(typ .eq. 'PQ') then
            instbid(1) = instpmax
            instbid(2) = instqmin
            instbid(3) = instpmin
            instbid(4) = instqmax
        endif
        trescapq(1) = tresca2(3)
        tresmepq(1) = tresme2(3)
        do 300 k = 1, 7
            mat1(k) = propi(13+k)
            mat2(k) = proqj(13+k)
300     continue
    endif
    if (tresca2(4) .gt. trescapq(1)) then
        tres = .false.
        if(typ .eq. 'PQ') then
            instbid(1) = instpmax
            instbid(2) = instqmin
            instbid(3) = instpmin
            instbid(4) = instqmax
        endif
        trescapq(1) = tresca2(4)
        tresmepq(1) = tresme2(4)
        do 310 k = 1, 7
            mat1(k) = propj(13+k)
            mat2(k) = proqi(13+k)
310     continue
    endif
!
    sppq(1)=trescapq(1)+sp2pq(1)
!
    if (tres) then
        do 320 kk = 1, 4
            if(tresca2(kk) .gt. trescapq(2)) then
                trescapq(2)=tresca2(kk)
                tresmepq(2)=tresme2(kk)
            endif
320     continue
    else
        do 330 kk = 1, 4
            if(tresca1(kk) .gt. trescapq(2)) then
                trescapq(2)=tresca1(kk)
                tresmepq(2)=tresme1(kk)
            endif
330     continue
    endif
!
    sppq(2)=trescapq(2)+sp2pq(2)
!
    if(sppq(1) .gt. sp1(1)) then
        sp1(1)  = sppq(1)
        sp1(2)  = sppq(2)
        spme(1) = tresmepq(1)+sp2pq(1)
        spme(2) = tresmepq(2)+sp2pq(2)
        instsp(1) = instbid(1)
        instsp(2) = instbid(2)
        instsp(3) = instbid(3)
        instsp(4) = instbid(4)
    endif
!
888 continue
!
    call jedema()
end subroutine
