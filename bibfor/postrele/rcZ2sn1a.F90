subroutine rcZ2sn1a(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                    instsn, sn, sp3, spmeca3)
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
    real(kind=8) :: proqj(20), instsn(2), sn, sp3, spmeca3
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
!     CALCUL DU SN avec la méthode TRESCA SIGNE
!
!     ------------------------------------------------------------------
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! OUT : sn    
!
    integer :: jseis, j, iret, jsigu, icmp, nbthep, nbprep, nbmecap, nbp
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), snpp, sn2pp, trescapp, sij(6)
    character(len=8) :: knumes
    character(len=2) :: typ, typpq, typqq
    real(kind=8) :: mij(12), sigu, pij, instpmin, instpmax, e0(2), e1(2)
    integer :: long, jtranp, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10
    real(kind=8) :: sntran(6), snt(6), tresca, snqq, sn2qq, trescaqq, sqtran(6)
    integer :: i11, i12, nbtheq, nbpreq, nbmecaq, nbq, jtranq, indicp
    real(kind=8) :: instqmin, instqmax, sqt(6), sn2pq, trescapq, sn2, sij1(6)
    real(kind=8) :: sij2(6), sij3(6), sij4(6), mij1(12), mij2(12), mij3(12)
    real(kind=8) :: mij4(12), pij1, pij2, pij3, pij4, smi(6), sma(6)
    real(kind=8) :: instbid(2), seis(6), st11(6), st12(6), st13(6), st14(6)
    real(kind=8) :: st21(6), st22(6), st23(6), st24(6), snpq, snther(6)
    integer :: indicq, jvalin, ind1, ind2
    real(kind=8) :: snpres(6), snmec(6), tresthpq, tresprpq, tresmecpq
    real(kind=8) :: k1, c1, k2, c2, k3, c3
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sn  = 0.d0
    sp3 = 0.d0
    spmeca3 = 0.d0
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
            seisfx(j) = 2*mse(1)*zr(jsigu-1+78+j)
            seisfy(j) = 2*mse(2)*zr(jsigu-1+78+1*6+j)
            seisfz(j) = 2*mse(3)*zr(jsigu-1+78+2*6+j)
            seismx(j) = 2*mse(4)*zr(jsigu-1+78+3*6+j)
            seismy(j) = 2*mse(5)*zr(jsigu-1+78+4*6+j)
            seismz(j) = 2*mse(6)*zr(jsigu-1+78+5*6+j)
            seisfx2(j)= 2*mse(7)*zr(jsigu-1+78+6*6+j)
            seisfy2(j)= 2*mse(8)*zr(jsigu-1+78+7*6+j)
            seisfz2(j)= 2*mse(9)*zr(jsigu-1+78+8*6+j)
            seismx2(j)= 2*mse(10)*zr(jsigu-1+78+9*6+j)
            seismy2(j)= 2*mse(11)*zr(jsigu-1+78+10*6+j)
            seismz2(j)= 2*mse(12)*zr(jsigu-1+78+11*6+j)
 30     continue
    endif
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE SITUATION SEULE
!                          CALCUL DE SN(P,P)
!--------------------------------------------------------------------
    typ   = '00'
    snpp  = 0.d0
    sn2pp = 0.d0
    trescapp = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) call rcZ2s2('SN', propi, propj, seismeze200, mse, sn2pp)
!
!-- on calcule la partie b3200_unitaire
    do 40 j = 1, 6
        sij(j) = 0.d0
40  continue
    if (iret .ne. 0) then
        do 50 j = 1, 6
            do 60 icmp = 1, 12
                mij(icmp) = propi(1+icmp)-propj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sij(j) = sij(j)+mij(icmp)*sigu
60          continue
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+78+72+j)
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
        indicp = jtranp + 6*2
        do 70 j = 1, 6
            sntran(j) = zr(indicp+6+j-1) -zr(indicp+j-1)
70      continue
        instpmin = zr(jtranp+84)
        instpmax = zr(jtranp+85)
    else
        do 80 j = 1, 6
            sntran(j) = 0.d0
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
                                      snt(j) =  sntran(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
113                               continue
                                  call rctres(snt,tresca)
                                  trescapp = max(trescapp, tresca)
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
                snt(j)  = sntran(j)+e0(i0)*sij(j)
        115 continue
            call rctres(snt,tresca)
            trescapp = max(trescapp, tresca)
114      continue
    endif
!
    snpp    = trescapp+sn2pp
    sn  = snpp
    instsn(1) = instpmin
    instsn(2) = instpmax
!
    if (numsip .eq. numsiq) goto 888
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE COMBINAISON DE
!                          SITUATIONS P ET Q
!--------------------------------------------------------------------
!--------------------- CALCUL DE sn(Q,Q)-----------------------------
    snqq  = 0.d0
    sn2qq = 0.d0
    trescaqq = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) call rcZ2s2('SN', proqi, proqj, seismeze200, mse, sn2qq)
!
!-- on calcule la partie b3200_unitaire
    do 120 j = 1, 6
        sij(j) = 0.d0
120 continue
    if (iret .ne. 0) then
        do 130 j = 1, 6
            do 140 icmp = 1, 12
                mij(icmp) = proqi(1+icmp)-proqj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sij(j) = sij(j)+mij(icmp)*sigu
140         continue
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+78+72+j)
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
        typqq='QQ'
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranq)
        indicq = jtranq + 6*2 
        do 150 j = 1, 6
            sqtran(j) = zr(indicq+6+j-1) -zr(indicq+j-1)
150     continue
        instqmin = zr(jtranq+84)
        instqmax = zr(jtranq+85)
    else
        typqq='00'
        do 160 j = 1, 6
            sqtran(j) = 0.d0
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
183                               continue
                                  call rctres(sqt,tresca)
                                  trescaqq = max(tresca, trescaqq)
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
        185 continue
            call rctres(sqt,tresca)
            trescaqq = max(tresca, trescaqq)
184      continue
    endif
!
    snqq = trescaqq+sn2qq
!
! -- Comparaison de SN(P,P) et SN(Q,Q)
    if (snqq .gt. snpp) then
        typ=typqq
        sn  = snqq
        instsn(1)= instqmin
        instsn(2)= instqmax
    endif
!
!--------------------- CALCUL DE SN(P,Q)-----------------------------
    sn2pq = 0.d0
    snpq = 0.d0
    trescapq = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) then
        call rcZ2s2('SN', propi, proqi, seismeze200, mse, sn2)
        sn2pq=max(sn2,sn2pq)
        call rcZ2s2('SN', propj, proqj, seismeze200, mse, sn2)
        sn2pq=max(sn2,sn2pq)
        call rcZ2s2('SN', propi, proqj, seismeze200, mse, sn2)
        sn2pq=max(sn2,sn2pq)
        call rcZ2s2('SN', propj, proqi, seismeze200, mse, sn2)
        sn2pq=max(sn2,sn2pq)
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
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sij1(j) = sij1(j) + mij1(icmp)*sigu
                sij2(j) = sij2(j) + mij2(icmp)*sigu
                sij3(j) = sij3(j) + mij3(icmp)*sigu
                sij4(j) = sij4(j) + mij4(icmp)*sigu
210         continue
            pij1 = propi(1)-proqi(1)
            pij2 = propj(1)-proqj(1)
            pij3 = propi(1)-proqj(1)
            pij4 = propj(1)-proqi(1)
            sigu = zr(jsigu-1+72+78+j)
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
            typpq='PQ'
            do 211 j = 1, 6
                smi(j)   = zr(indicp+j-1)    - zr(indicq+6+j-1)
                sma(j)   = zr(indicp+6+j-1)  - zr(indicq+j-1)
211         continue
        else
            typpq='QQ'
            do 212 j = 1, 6
                smi(j)   = 0.d0
                sma(j)   = zr(indicq+6+j-1) - zr(indicq+j-1)
212         continue
            instbid(1) = instqmin
            instbid(2) = instqmax
        endif
    else
        if (nbp .ne. 0) then
            typpq='PP'
            do 213 j = 1, 6
                smi(j)   = 0.d0
                sma(j)   = zr(indicp+6+j-1) - zr(indicp+j-1)
213         continue
            instbid(1) = instpmin
            instbid(2) = instpmax
        else
            typpq='00'
            do 214 j = 1, 6
                smi(j) = 0.d0
                sma(j) = 0.d0
214         continue
            instbid(1) = -1.0
            instbid(2) = -1.0
        endif
    endif
!
!-- on maximise sur les possibilités de signes
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
233                               continue
!
                                  call rctres(st11,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='PQ'
                                      endif  
                                  endif
                                  call rctres(st12,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='PQ'
                                      endif   
                                  endif
                                  call rctres(st13,tresca)
                                   if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='PQ'
                                      endif  
                                  endif
                                  call rctres(st14,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='PQ'
                                      endif  
                                  endif
                                  call rctres(st21,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='QP'
                                      endif 
                                  endif
                                  call rctres(st22,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='QP'
                                      endif  
                                  endif
                                  call rctres(st23,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='QP'
                                      endif  
                                  endif
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                                          typpq='QP'
                                      endif 
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
235         continue
            call rctres(st11,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='PQ'
                endif  
            endif
            call rctres(st12,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='PQ'
                endif  
            endif
            call rctres(st13,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='PQ'
                endif 
            endif
            call rctres(st14,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='PQ'
                endif  
            endif
!
            call rctres(st21,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='QP'
                endif  
            endif
            call rctres(st22,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='QP'
                endif 
            endif
            call rctres(st23,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='QP'
                endif
            endif
            call rctres(st24,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                if(typpq .eq. 'PQ' .or. typpq .eq. 'QP') then
                    typpq='QP'
                endif  
            endif
!
234     continue
    endif
!
    if(typpq .eq. 'PQ') then
       instbid(1) = instpmin
       instbid(2) = instqmax
    else
       instbid(1) = instqmin
       instbid(2) = instpmax
    endif
!
    snpq=trescapq+sn2pq
!
    if(snpq .gt. sn) then
        sn  = snpq
        instsn(1) = instbid(1)
        instsn(2) = instbid(2)
        typ =typpq
    endif
!
888 continue
!
!-----------------------------------------------------------
!   CALCUL DE LA PARTIE SP3 (QUI DEPEND DES INSTANTS DE SN)
!-----------------------------------------------------------
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
