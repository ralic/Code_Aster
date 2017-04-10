subroutine rcZ2sn1a(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                    instsn, sn, sp3, spmeca3, snet, trescapr, tresthpq)
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
    real(kind=8) :: mse(12), propi(20), propj(20), proqi(20), trescapr
    real(kind=8) :: proqj(20), instsn(2), sn, sp3, spmeca3, snet, tresthpq
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN  : ZE200       : EST-ON en ZE200a ou ZE200b ?
! IN  : LIEU        : ORIGINE DU SEGMENT OU EXTREMITE
! IN  : NUMSIP      : NUMERO SITUATION P
! IN  : NUMSIQ      : NUMERO SITUATION Q
! IN  : SEISMEB32   : EST-ON en SEISME+B3200_T
! IN  : SEISMEUNIT  : EST-ON en SEISME+B3200_UNIT
! IN  : SEISMEZE200 : EST-ON en SEISME+ZE200a (ou ZE200b)
! IN  : MSE         : MOMENTS DUS AU SEISME
! IN  : PROPI       : PROPRIETES DE L'ETAT A DE LA SITUATION P 
!                        (PRESSION, MOMENTS et MATERIAU)
! IN  : PROPJ       : PROPRIETES DE L'ETAT B DE LA SITUATION P 
!                        (PRESSION, MOMENTS et MATERIAU)
! IN  : PROQI       : PROPRIETES DE L'ETAT A DE LA SITUATION Q 
!                        (PRESSION, MOMENTS et MATERIAU)
! IN  : PROQJ       : PROPRIETES DE L'ETAT B DE LA SITUATION Q 
!                        (PRESSION, MOMENTS et MATERIAU)
! OUT : INSTSN      : INSTANTS DE CALCUL DE SN
! OUT : SN 
! OUT : SP3 
! OUT : SPMECA3   
!
    integer :: jseis, j, iretp, jsigu, icmp, nbthep, nbprep, nbmecap, nbp
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), spp, s2pp, trescapp, trescappet, sa(6)
    character(len=8) :: knumes
    character(len=2) :: typpq
    real(kind=8) :: mij(12), sigu, pij, instpmin, instpmax, e0(2)
    integer :: long, jtranp, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10
    real(kind=8) :: sb(6), sbet(6), stet(6), st(6), tresca, sqq, s2qq, trescaqq
    integer :: i11, i12, nbtheq, nbpreq, nbmecaq, nbq, jtranq, indicp
    real(kind=8) :: instqmin, instqmax, s2pq, trescapq, s2, smom11(6)
    real(kind=8) :: smom12(6), smom13(6), smom14(6), mij11(12), mij12(12), mij13(12)
    real(kind=8) :: mij14(12), pij1, pij2, pij3, pij4, sb1(6), sb2(6)
    real(kind=8) :: seis(6), st11(6), st12(6), st13(6), st14(6)
    real(kind=8) :: st21(6), st22(6), st23(6), st24(6), spq, snther(6)
    integer :: indicq, jvalin, ind1, ind2, iret2p, jtempa, jtempb
    real(kind=8) :: snpres(6), snmec(6), tresprpq, tresmecpq
    real(kind=8) :: k1, c1, k2, c2, k3, c3, sc(6), tempa, tempb, A1(12)
    real(kind=8) :: B1(12), tempmin, tempmax, momaxp(12), mominp(12)
    real(kind=8) ::  momaxq(12), mominq(12), sc1(6), sc2(6)
    integer :: jtemp, iretq, iret2q
    real(kind=8) :: spres1(6), spres2(6), spres3(6), smom21(6), smom22(6)
    real(kind=8) :: spres4(6), mij21(12), mij22(12), siprmoy(6)
    real(kind=8) :: siprmoy11(6), siprmoy21(6), siprmoy31(6), siprmoy41(6)
    real(kind=8) :: siprmoy12(6), siprmoy22(6), siprmoy32(6), siprmoy42(6)
    real(kind=8) :: siprmoyt(6), siprmoyt1(6), siprmoyt2(6)
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sn  = 0.d0
    sp3 = 0.d0
    spmeca3 = 0.d0
    snet = 0.d0
    trescapr = 0.d0
    tresthpq = 0.d0
!
! TROIS CONTRIBUTIONS POSSIBLES POUR SN
! SA  : UNITAIRE
!       SOUS SITUATION : CHAR_ETAT_A, CHAR_ETAT_B, PRES_A, PRES_B 
!       (RESU_MECA_UNIT et CHAR_MECA doivent être renseignés)
! SB  : TRANSITOIRE
!       SOUS SITUATION : NUME_RESU_THER NUME_RESU_MECA, NUME_RESU_PRES 
!       (RESU_MECA et RESU_PRES doivent être renseignés)
! SC  : INTERPOLATION DES MOMENTS
!       SOUS SITUATION : TEMP_A, TEMP_B, CHAR_ETAT_A, CHAR_ETAT_B, TABL_TEMP
!       (RESU_MECA_UNIT et CHAR_MECA doivent être renseignés) 
!
!------------------------
!     SI SEISME B3200
!------------------------
!-- type instantané
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
        do 20 j = 1, 6
            seisfx(j) = 2*zr(jseis+j-1)
            seisfy(j) = 2*zr(jseis+1*6+j-1)
            seisfz(j) = 2*zr(jseis+2*6+j-1)
            seismx(j) = 2*zr(jseis+3*6+j-1)
            seismy(j) = 2*zr(jseis+4*6+j-1)
            seismz(j) = 2*zr(jseis+5*6+j-1)
            seisfx2(j)= 0.d0
            seisfy2(j)= 0.d0
            seisfz2(j)= 0.d0
            seismx2(j)= 0.d0
            seismy2(j)= 0.d0
            seismz2(j)= 0.d0
20      continue
    endif
!
!-- type unitaire
    iretp  = 0
    iret2p  = 0
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iretp)
        if (iretp .ne. 0) then
            call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
            call jeexin(jexnom('&&RC3200.TEMP_ETAT_A', knumes), iret2p)
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
30      continue
    endif
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE SITUATION SEULE
!                          CALCUL DE SN(P,P)
!--------------------------------------------------------------------
    ind1=1
    ind2=1
    spp  = 0.d0
    s2pp = 0.d0
    trescapp = -1.d0
    trescappet = -1.d0
!
!-- partie ze200
    if (ze200) call rcZ2s2('SN', propi, propj, seismeze200, mse, s2pp)
!
!-- SA partie b3200_unitaire
    do 40 j = 1, 6
        sa(j) = 0.d0
40  continue
    if (iretp .ne. 0 .and. iret2p .eq. 0) then
        do 50 j = 1, 6
            do 60 icmp = 1, 12
                mij(icmp) = propi(1+icmp)-propj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sa(j) = sa(j)+mij(icmp)*sigu
60          continue
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+78+72+j)
            sa(j) = sa(j)+pij*sigu
50      continue
    endif
!
!-- SB partie transitoire
    do 65 j = 1, 6
        sb(j) = 0.d0
        st(j) = 0.d0
        sbet(j) = 0.d0
        stet(j) = 0.d0
        siprmoyt(j) = 0.d0
65  continue
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
    nbp = max(nbthep,nbprep, nbmecap)
    if (nbp .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranp)
        ind1 = jtranp
        ind2 = jtranp
        indicp = jtranp + 6*2
        do 70 j = 1, 6
            sb(j) = zr(indicp+6+j-1) -zr(indicp+j-1)
            sbet(j) = (zr(indicp+6+j-1)-zr(indicp+86+j-1)) -(zr(indicp+j-1)-zr(indicp+80+j-1))
            siprmoyt(j) = zr(jtranp+30+j-1) -zr(jtranp+24+j-1)
70      continue
        instpmin = zr(jtranp+84)
        instpmax = zr(jtranp+85)
    else
        ind1 = 1
        ind2 = 1
        instpmin = -1.0
        instpmax = -1.0
    endif
!
!-- SC partie unitaire avec interpolation des moments
    do 80 j = 1, 6
        sc(j) = 0.d0
        sc1(j) = 0.d0
        sc2(j) = 0.d0
        if (iret2p .ne.0) then
            pij=propi(1)-propj(1)
            sigu = zr(jsigu-1+78+72+j)
            sc1(j) = pij*sigu
        endif 
80  continue
!
    if (iret2p .ne. 0) then
        call jelira(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'L', jtempa)
        tempa = zr(jtempa)
        call jelira(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'L', jtempb)
        tempb = zr(jtempb)
        if (tempa .lt. tempb) then
            do 81 icmp = 1, 12
                    A1(icmp) = (propi(icmp+1)-propj(icmp+1))/(tempa-tempb)
                    B1(icmp) = (propj(icmp+1)*tempa-propi(icmp+1)*tempb)/(tempa-tempb)
81          continue
        else
            do 82 icmp = 1, 12
                    A1(icmp) = (propj(icmp+1)-propi(icmp+1))/(tempb-tempa)
                    B1(icmp) = (propi(icmp+1)*tempb-propj(icmp+1)*tempa)/(tempb-tempa)
82          continue
        endif
        if (nbp .ne. 0) then
            tempmin = zr(jtranp+88)
            tempmax = zr(jtranp+89) 
            do 83 icmp = 1, 12
                mominp(icmp) = A1(icmp)*tempmin+B1(icmp)
                momaxp(icmp) = A1(icmp)*tempmax+B1(icmp)
83          continue
            do 84 j = 1, 6
              do 85 icmp = 1, 12
                mij(icmp)=momaxp(icmp)-mominp(icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sc2(j) = sc2(j) + mij(icmp)*sigu
85            continue
84          continue
        else
            call jelira(jexnom('&&RC3200.TEMPCST', knumes), 'LONUTI', long)
            call jeveuo(jexnom('&&RC3200.TEMPCST', knumes), 'L', jtemp) 
            do 86 icmp = 1, 12
                    mominp(icmp) = A1(icmp)*zr(jtemp)+B1(icmp)
                    momaxp(icmp) = A1(icmp)*zr(jtemp)+B1(icmp)
86          continue 
            do 87 j = 1, 6
              do 88 icmp = 1, 12
                mij(icmp)=mominp(icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sc2(j) = sc2(j) + mij(icmp)*sigu
88            continue
87          continue           
        endif
    endif
!
!-- on maximise sur les différentes possibilités de signes
    do 90 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e0(i0) = i1
90  continue
!
    if (seismeb32 .or. seismeunit) then
          do 100 i0 = 1, 2
            do 101 i1 = 1, 2
              do 102 i2 = 1, 2
                do 103 i3 = 1, 2
                  do 104 i4 = 1, 2
                    do 105 i5 = 1, 2
                      do 106 i6 = 1, 2
                        do 107 i7 = 1, 2
                          do 108 i8 = 1, 2
                            do 109 i9 = 1, 2
                              do 110 i10 = 1, 2
                                do 111 i11 = 1, 2
                                  do 112 i12 = 1, 2
                                    do 113 j = 1, 6
                                      sc(j) =  sc1(j) + sc2(j)
                                      st(j) =  sb(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                               e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                               e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                               e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                               e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                               e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
                                      stet(j) =  sbet(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                               e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                               e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                               e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                               e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                               e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
113                                 continue
                                    call rctres(st,tresca)
                                    trescapp = max(trescapp, tresca)
                                    call rctres(stet,tresca)
                                    trescappet = max(trescappet, tresca)
112                               continue
111                             continue
110                           continue
109                         continue
108                       continue
107                     continue
106                   continue
105                 continue
104               continue
103             continue
102           continue
101         continue
100       continue
    else
          do 116 i0 = 1, 2
            do 117 j=1,6
                sc(j) =  sc1(j)+sc2(j)
                st(j)  = sb(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
                stet(j)  = sbet(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
117         continue
            call rctres(st,tresca)
            trescapp = max(trescapp, tresca)
            call rctres(stet,tresca)
            trescappet = max(trescappet, tresca)
116       continue
    endif
!
    spp    = trescapp+s2pp
    sn  = spp
    snet = trescappet+s2pp
    instsn(1) = instpmin
    instsn(2) = instpmax
!
!-- Pour le calcul du rochet thermique
    do 41 j = 1, 6
        siprmoy(j) = 0.d0
41  continue
    if (iretp .ne. 0) then
        do 51 j = 1, 6
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+156+72+j)
            siprmoy(j) = pij*sigu
51      continue
    endif
    call rctres(siprmoy,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoyt,tresca)
    trescapr = max(tresca, trescapr)
!
    if (numsip .eq. numsiq) goto 888
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE COMBINAISON DE
!                          SITUATIONS P ET Q
!--------------------------------------------------------------------
!--------------------- CALCUL DE SN(Q,Q)-----------------------------
    sqq  = 0.d0
    s2qq = 0.d0
    trescaqq = -1.d0
!
!-- partie ze200
    if (ze200) call rcZ2s2('SN', proqi, proqj, seismeze200, mse, s2qq)
!
!-- SA partie b3200_unitaire
    iretq  = 0
    iret2q  = 0
    knumes = 'S       '
    call codent(numsiq, 'D0', knumes(2:8))
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iretq)
        if (iretq .ne. 0) then
            if (iretp .eq. 0) call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
            call jeexin(jexnom('&&RC3200.TEMP_ETAT_A', knumes), iret2q)
        endif
    endif
!
    do 120 j = 1, 6
        sa(j) = 0.d0
120 continue
    if (iretq .ne. 0 .and. iret2q .eq. 0) then
        do 130 j = 1, 6
            do 140 icmp = 1, 12
                mij(icmp) = proqi(1+icmp)-proqj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sa(j) = sa(j)+mij(icmp)*sigu
140         continue
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+78+72+j)
            sa(j) = sa(j)+pij*sigu
130     continue
    endif
!
!-- SB partie transitoire
    do 150 j = 1, 6
        sb(j) = 0.d0
        siprmoyt(j) = 0.d0
150 continue
!
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbpreq)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecaq)
    nbq = max(nbtheq,nbpreq, nbmecaq)
    if (nbq .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranq)
        indicq = jtranq + 6*2
        do 160 j = 1, 6
            sb(j) = zr(indicq+6+j-1) -zr(indicq+j-1)
            siprmoyt(j) = zr(jtranq+30+j-1) -zr(jtranq+24+j-1)
160     continue
        instqmin = zr(jtranq+84)
        instqmax = zr(jtranq+85)
    else
        instqmin = -1.0
        instqmax = -1.0
    endif
!
!-- SC partie unitaire avec interpolation des moments
    do 170 j = 1, 6
        sc(j) = 0.d0
        sc1(j) = 0.d0
        sc2(j) = 0.d0
        if (iret2q .ne.0) then
            pij=proqi(1)-proqj(1)
            sigu = zr(jsigu-1+78+72+j)
            sc1(j) = pij*sigu
        endif 
170 continue
!
    if (iret2q .ne. 0) then
        call jelira(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'L', jtempa)
        tempa = zr(jtempa)
        call jelira(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'L', jtempb)
        tempb = zr(jtempb)
        if (tempa .lt. tempb) then
            do 171 icmp = 1, 12
                    A1(icmp) = (proqi(icmp+1)-proqj(icmp+1))/(tempa-tempb)
                    B1(icmp) = (proqj(icmp+1)*tempa-proqi(icmp+1)*tempb)/(tempa-tempb)
171         continue
        else
            do 172 icmp = 1, 12
                    A1(icmp) = (proqj(icmp+1)-proqi(icmp+1))/(tempb-tempa)
                    B1(icmp) = (proqi(icmp+1)*tempb-proqj(icmp+1)*tempa)/(tempb-tempa)
172         continue
        endif
        if (nbq .ne.0) then
            tempmin = zr(jtranq+88)
            tempmax = zr(jtranq+89) 
            do 173 icmp = 1, 12
                    mominq(icmp) = A1(icmp)*tempmin+B1(icmp)
                    momaxq(icmp) = A1(icmp)*tempmax+B1(icmp)
173         continue
            do 174 j = 1, 6
              do 175 icmp = 1, 12
                mij(icmp)=momaxq(icmp)-mominq(icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sc2(j) = sc2(j) + mij(icmp)*sigu
175           continue
174         continue
        else
            call jelira(jexnom('&&RC3200.TEMPCST', knumes), 'LONUTI', long)
            call jeveuo(jexnom('&&RC3200.TEMPCST', knumes), 'L', jtemp) 
            do 176 icmp = 1, 12
                    mominq(icmp) = A1(icmp)*zr(jtemp)+B1(icmp)
                    momaxq(icmp) = A1(icmp)*zr(jtemp)+B1(icmp)
176         continue 
            do 177 j = 1, 6
              do 178 icmp = 1, 12
                mij(icmp)=mominq(icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sc2(j) = sc2(j) + mij(icmp)*sigu
178           continue
177         continue           
        endif
    endif
!
!-- on maximise sur les différentes possibilités de signes
    if (seismeb32 .or. seismeunit) then
          do 180 i0 = 1, 2
            do 181 i1 = 1, 2
              do 182 i2 = 1, 2
                do 183 i3 = 1, 2
                  do 184 i4 = 1, 2
                    do 185 i5 = 1, 2
                      do 186 i6 = 1, 2
                        do 187 i7 = 1, 2
                          do 188 i8 = 1, 2
                            do 189 i9 = 1, 2
                              do 190 i10 = 1, 2
                                do 191 i11 = 1, 2
                                  do 192 i12 = 1, 2
                                    do 193 j = 1, 6
                                      sc(j) =  sc1(j) + sc2(j)
                                      st(j) =  sb(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                               e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                               e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                               e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                               e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                               e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
193                                 continue
                                    call rctres(st,tresca)
                                    trescaqq = max(trescaqq, tresca)
192                               continue
191                             continue
190                           continue
189                         continue
188                       continue
187                     continue
186                   continue
185                 continue
184               continue
183             continue
182           continue
181         continue
180       continue
    else
          do 194 i0 = 1, 2
            do 195 j=1,6
              sc(j) =  sc1(j) + sc2(j)
              st(j)  = sb(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
195         continue
            call rctres(st,tresca)
            trescaqq = max(trescaqq, tresca)
194       continue
    endif
!
    sqq = trescaqq+s2qq
!
! -- Comparaison de SN(P,P) et SN(Q,Q)
    if (sqq .gt. spp) then
        sn  = sqq
        instsn(1)= instqmin
        instsn(2)= instqmax
        if(nbq .ne. 0) then
            ind1=jtranq
            ind2=jtranq
        else
            ind1=1
            ind2=1
        endif
    endif
!
!
!-- Pour le calcul du rochet thermique
    do 42 j = 1, 6
        siprmoy(j) = 0.d0
42  continue
    if (iretq .ne. 0) then
        do 52 j = 1, 6
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+156+72+j)
            siprmoy(j) = pij*sigu
52      continue
    endif
    call rctres(siprmoy,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoyt,tresca)
    trescapr = max(tresca, trescapr)
!
!
!--------------------- CALCUL DE SN(P,Q)-----------------------------
    s2pq = 0.d0
    spq = 0.d0
    trescapq = 0.d0
!
!-- partie ze200
    if (ze200) then
        call rcZ2s2('SN', propi, proqi, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
        call rcZ2s2('SN', propj, proqj, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
        call rcZ2s2('SN', propi, proqj, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
        call rcZ2s2('SN', propj, proqi, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
    endif
!
!-- partie due à la pression
    do 197 j = 1, 6
        spres1(j) = 0.d0
        spres2(j) = 0.d0
        spres3(j) = 0.d0
        spres4(j) = 0.d0
197 continue
    if (iretp .ne. 0 .or. iretq .ne. 0) then
        do 198 j = 1, 6
            pij1 = propi(1)-proqi(1)
            pij2 = propj(1)-proqj(1)
            pij3 = propi(1)-proqj(1)
            pij4 = propj(1)-proqi(1)
            sigu = zr(jsigu-1+72+78+j)
            spres1(j) = pij1*sigu
            spres2(j) = pij2*sigu
            spres3(j) = pij3*sigu
            spres4(j) = pij4*sigu
198     continue
    endif
!
!-- partie due aux moments
    do 199 j = 1, 6
        smom11(j) = 0.d0
        smom12(j) = 0.d0
        smom13(j) = 0.d0
        smom14(j) = 0.d0
        smom21(j) = 0.d0
        smom22(j) = 0.d0
199 continue
!
    if(iretp .eq. 0 .and. iretq .eq. 0) goto 777 
!
    do 200 j = 1, 6
        do 201 icmp = 1, 12
            mij11(icmp) = 0.d0
            mij12(icmp) = 0.d0
            mij13(icmp) = 0.d0
            mij14(icmp) = 0.d0
            mij21(icmp) = 0.d0
            mij22(icmp) = 0.d0
            if (iretp .ne. 0) then
                if (iret2p .eq. 0) then
                    mij11(icmp) = mij11(icmp)+propi(1+icmp)
                    mij12(icmp) = mij12(icmp)+propj(1+icmp)
                    mij13(icmp) = mij13(icmp)+propi(1+icmp)
                    mij14(icmp) = mij14(icmp)+propj(1+icmp)
                else
                    mij21(icmp) = mij21(icmp)+mominp(icmp)
                    mij22(icmp) = mij22(icmp)+momaxp(icmp)
                endif
            endif
            if (iretq .ne. 0) then
                if (iret2q .eq. 0) then
                    mij11(icmp) = mij11(icmp)-proqi(1+icmp)
                    mij12(icmp) = mij12(icmp)-proqj(1+icmp)
                    mij13(icmp) = mij13(icmp)-proqj(1+icmp)
                    mij14(icmp) = mij14(icmp)-proqi(1+icmp)
                else
                    mij21(icmp) = mij21(icmp)-momaxq(icmp)
                    mij22(icmp) = mij22(icmp)-mominq(icmp)
                endif
            endif
            sigu = zr(jsigu-1+78+6*(icmp-1)+j)
            smom11(j) = smom11(j) + mij11(icmp)*sigu
            smom12(j) = smom12(j) + mij12(icmp)*sigu
            smom13(j) = smom13(j) + mij13(icmp)*sigu
            smom14(j) = smom14(j) + mij14(icmp)*sigu
            smom21(j) = smom21(j) + mij21(icmp)*sigu
            smom22(j) = smom22(j) + mij22(icmp)*sigu
201     continue
200 continue
!
777 continue
!
!-- SB partie transitoire
    if (nbq .ne. 0) then
        if (nbp .ne. 0) then
            do 211 j = 1, 6
                sb1(j)   = zr(indicp+j-1)    - zr(indicq+6+j-1)
                sb2(j)   = zr(indicp+6+j-1)  - zr(indicq+j-1)
                siprmoyt1(j) =  zr(jtranq+30+j-1) -zr(jtranp+24+j-1)
                siprmoyt2(j) =  zr(jtranp+30+j-1) -zr(jtranq+24+j-1)
211         continue
        else
            do 212 j = 1, 6
                sb1(j)   = 0.d0    - zr(indicq+6+j-1)
                sb2(j)   = 0.d0  - zr(indicq+j-1)
                siprmoyt1(j) =  zr(jtranq+30+j-1) -0.d0
                siprmoyt2(j) =  0.d0 -zr(jtranq+24+j-1)
212         continue
        endif
    else
        if (nbp .ne. 0) then
            do 213 j = 1, 6
                sb1(j)   = zr(indicp+j-1)    - 0.d0
                sb2(j)   = zr(indicp+6+j-1)  - 0.d0
                siprmoyt1(j) =  0.d0 -zr(jtranp+24+j-1)
                siprmoyt2(j) =  zr(jtranp+30+j-1) -0.d0
213         continue
        else
            do 214 j = 1, 6
                sb1(j) = 0.d0
                sb2(j) = 0.d0
214         continue
        endif
    endif
!
!-- on maximise sur les possibilités de signes
    if (seismeb32 .or. seismeunit) then
        do 220 i0 = 1, 2
          do 221 i1 = 1, 2
            do 222 i2 = 1, 2
              do 223 i3 = 1, 2
                do 224 i4 = 1, 2
                  do 225 i5 = 1, 2
                    do 226 i6 = 1, 2
                      do 227 i7 = 1, 2
                        do 228 i8 = 1, 2
                          do 229 i9 = 1, 2
                            do 230 i10 = 1, 2
                              do 231 i11 = 1, 2
                                do 232 i12 = 1, 2
                                  do 233 j = 1, 6
                                      seis(j)= e0(i1)*seisfx(j)+e0(i2)*seisfy(j)+&
                                               e0(i3)*seisfz(j)+e0(i4)*seismx(j)+&
                                               e0(i5)*seismy(j)+e0(i6)*seismz(j)+&
                                               e0(i7)*seisfx2(j)+e0(i8)*seisfy2(j)+&
                                               e0(i9)*seisfz2(j)+e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
                                      st11(j) = e0(i0)*(smom11(j)+smom21(j)+spres1(j))+&
                                                sb1(j)+seis(j)
                                      st12(j) = e0(i0)*(smom12(j)+smom21(j)+spres2(j))+&
                                                sb1(j)+seis(j)
                                      st13(j) = e0(i0)*(smom13(j)+smom21(j)+spres3(j))+&
                                                sb1(j)+seis(j)
                                      st14(j) = e0(i0)*(smom14(j)+smom21(j)+spres4(j))+&
                                                sb1(j)+seis(j)
                                      st21(j) = e0(i0)*(smom11(j)+smom22(j)+spres1(j))+&
                                                sb2(j)+seis(j)
                                      st22(j) = e0(i0)*(smom12(j)+smom22(j)+spres2(j))+&
                                                sb2(j)+seis(j)
                                      st23(j) = e0(i0)*(smom13(j)+smom22(j)+spres3(j))+&
                                                sb2(j)+seis(j)
                                      st24(j) = e0(i0)*(smom14(j)+smom22(j)+spres4(j))+&
                                                sb2(j)+seis(j)
233                               continue
!
                                  call rctres(st11,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='PQ'
                                  endif
                                  call rctres(st12,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='PQ'  
                                  endif
                                  call rctres(st13,tresca)
                                   if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='PQ'
                                  endif
                                  call rctres(st14,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='PQ' 
                                  endif
                                  call rctres(st21,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='QP'
                                  endif
                                  call rctres(st22,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='QP'
                                  endif
                                  call rctres(st23,tresca)
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='QP'
                                  endif
                                  if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      typpq='QP'
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
                st11(j)=sb1(j)+e0(i0)*(smom11(j)+smom21(j)+spres1(j))
                st12(j)=sb1(j)+e0(i0)*(smom12(j)+smom21(j)+spres2(j))
                st13(j)=sb1(j)+e0(i0)*(smom13(j)+smom21(j)+spres3(j))
                st14(j)=sb1(j)+e0(i0)*(smom14(j)+smom21(j)+spres4(j))
                st21(j)=sb2(j)+e0(i0)*(smom11(j)+smom22(j)+spres1(j))
                st22(j)=sb2(j)+e0(i0)*(smom12(j)+smom22(j)+spres2(j))
                st23(j)=sb2(j)+e0(i0)*(smom13(j)+smom22(j)+spres3(j))
                st24(j)=sb2(j)+e0(i0)*(smom14(j)+smom22(j)+spres4(j))
235         continue
            call rctres(st11,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='PQ'
            endif
            call rctres(st12,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='PQ'  
            endif
            call rctres(st13,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='PQ'
            endif
            call rctres(st14,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='PQ'
            endif
!
            call rctres(st21,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='QP'  
            endif
            call rctres(st22,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='QP'  
            endif
            call rctres(st23,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='QP'  
            endif
            call rctres(st24,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                typpq='QP'   
            endif
!
234     continue
    endif
!
    spq=trescapq+s2pq
!
    if(spq .gt. sn) then
        sn  = spq
        if(typpq .eq. 'PQ') then
          instsn(1) = instpmin
          instsn(2) = instqmax
          if (instpmin .lt. 0) then
              ind1 = 1
          else
              ind1=jtranp
          endif
          if (instqmax .lt. 0) then
              ind2 = 1
          else
              ind2=jtranq
          endif
        else
          instsn(1) = instqmin
          instsn(2) = instpmax
          if (instqmin .lt. 0) then
              ind1 = 1
          else
              ind1=jtranq
          endif
          if (instpmax .lt. 0) then
              ind2 = 1
          else
              ind2=jtranp
          endif
        endif
    endif
!
!-- Pour le calcul du rochet thermique
    do 43 j = 1, 6
        siprmoy11(j) = 0.d0
        siprmoy21(j) = 0.d0
        siprmoy31(j) = 0.d0
        siprmoy41(j) = 0.d0
        siprmoy12(j) = 0.d0
        siprmoy22(j) = 0.d0
        siprmoy32(j) = 0.d0
        siprmoy42(j) = 0.d0
43  continue
    if (iretp .ne. 0 .or. iretq .ne. 0) then
        do 53 j = 1, 6
            sigu = zr(jsigu-1+156+72+j)
            siprmoy11(j) = pij1*sigu-siprmoyt1(j)
            siprmoy21(j) = pij2*sigu-siprmoyt1(j)
            siprmoy31(j) = pij3*sigu-siprmoyt1(j)
            siprmoy41(j) = pij4*sigu-siprmoyt1(j)
            siprmoy12(j) = pij1*sigu-siprmoyt2(j)
            siprmoy22(j) = pij2*sigu-siprmoyt2(j)
            siprmoy32(j) = pij3*sigu-siprmoyt2(j)
            siprmoy42(j) = pij4*sigu-siprmoyt2(j)
53      continue
    endif
    call rctres(siprmoy11,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy21,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy31,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy41,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy12,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy22,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy32,tresca)
    trescapr = max(tresca, trescapr)
    call rctres(siprmoy42,tresca)
    trescapr = max(tresca, trescapr)
!
888 continue
!
!-----------------------------------------------------------
!   CALCUL DE LA PARTIE SP3 (QUI DEPEND DES INSTANTS DE SN)
!-----------------------------------------------------------
!
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
    k1 = zr(jvalin)
    c1 = zr(jvalin+1)
    k2 = zr(jvalin+2)
    c2 = zr(jvalin+3) 
    k3 = zr(jvalin+4)
    c3 = zr(jvalin+5)
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
    call jedema()
end subroutine
