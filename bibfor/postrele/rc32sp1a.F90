subroutine rc32sp1a(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                    instsp, sp, spme, mat1, mat2)
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
    real(kind=8) :: proqj(20), instsp(4), sp(2), spme(2)
    real(kind=8) :: mat1(7), mat2(7)
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
!     CALCUL DU SP avec la méthode TRESCA SIGNE
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
! OUT : INSTSP      : INSTANTS DE CALCUL DE SP1 et SP2
! OUT : SN 
! OUT : SP3 
! OUT : SPMECA3    
!
    integer :: k, jseis, j, iretp, jsigu, icmp, nbthep, nbprep, nbmecap, nbp
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), spp, s2pp, trescapp, tresmepp
    character(len=8) :: knumes
    character(len=2) :: typpq
    real(kind=8) :: sa(6), mij(12), sigu, pij, instpmin, instpmax, e0(2)
    integer :: long, jtranp, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10
    real(kind=8) :: sb(6), spm(6), st(6), spmec(6), tresca
    integer :: i11, i12, nbtheq, nbpreq, nbmecaq, nbq, jtranq, kk, iret2p
    real(kind=8) :: sqq, s2qq, trescaqq, tresmeqq
    real(kind=8) :: instqmin, instqmax, s2pq(2)
    real(kind=8) ::  trescapq(2), tresmepq(2), s2
    real(kind=8) :: mij11(12), mij12(12), mij13(12), mij14(12), mij21(12)
    real(kind=8) :: pij1, pij2, pij3, pij4, sb1(6), sb2(6), smemi(6)
    real(kind=8) :: smema(6), instbid(4), tresca1(4), tresca2(4), tresme1(4)
    real(kind=8) :: tresme2(4), seis(6), st11(6), st12(6), st13(6), st14(6)
    real(kind=8) :: st21(6), st22(6), st23(6), st24(6), smec11(6), smec12(6)
    real(kind=8) :: smec13(6), smec14(6), smec21(6), smec22(6), smec23(6)
    real(kind=8) :: smec24(6), spq(2), sc(6), tempa, tempb, A1(12), B1(12)
    aster_logical :: tres
    integer :: jtempa, jtempb, jtemp, iretq, iret2q
    real(kind=8) :: tempmin, tempmax, mominp(12), momaxp(12), mominq(12)
    real(kind=8) :: momaxq(12)
    real(kind=8) :: spres1(6), spres2(6), spres3(6), spres4(6)
    real(kind=8) :: smom11(6), smom12(6), smom13(6), smom14(6), smom21(6) 
    real(kind=8) :: smom22(6), mij22(12)
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sp(1)  = 0.d0
    sp(2)  = 0.d0
    spme(1) = 0.d0
    spme(2) = 0.d0
    do 10 k = 1, 7
        mat1(k) = propi(13+k)
        mat2(k) = propj(13+k)
10  continue
!
! TROIS CONTRIBUTIONS POSSIBLES POUR SP
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
!     SI SEISME 
!------------------------
!-- B3200 et séisme type instantané
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
!-- B3200 et séisme type unitaire
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
    spp  = 0.d0
    s2pp = 0.d0
    trescapp = 0.d0
    tresmepp = 0.d0
!
!-- partie ze200
    if (ze200) call rcZ2s2('SP', propi, propj, seismeze200, mse, s2pp)
!
!-- SA partie b3200_unitaire
    do 40 j = 1, 6
        sa(j) = 0.d0
40  continue
    if (iretp .ne. 0 .and. iret2p .eq. 0) then
        do 50 j = 1, 6
            do 60 icmp = 1, 12
                mij(icmp) = propi(1+icmp)-propj(1+icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sa(j) = sa(j)+mij(icmp)*sigu
60          continue
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+72+j)
            sa(j) = sa(j)+pij*sigu
50      continue
    endif
!
!-- SB la partie transitoire
    do 65 j = 1, 6
            sb(j) = 0.d0
            spm(j) = 0.d0
65  continue
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbthep)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbprep)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecap)
    nbp = max(nbthep,nbprep, nbmecap)
    if (nbp .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranp)
        do 70 j = 1, 6
            sb(j) = zr(jtranp+6+j-1) -zr(jtranp+j-1)
            spm(j)   = zr(jtranp+78+j-1)-zr(jtranp+72+j-1)
70      continue
        instpmin = zr(jtranp+86)
        instpmax = zr(jtranp+87)
    else
        instpmin = -1.0
        instpmax = -1.0
    endif
!
!-- SC la partie unitaire avec interpolation des moments
    do 80 j = 1, 6
        sc(j) = 0.d0
        if (iret2p .ne.0) then
            pij=propi(1)-propj(1)
            sigu = zr(jsigu-1+72+j)
            sc(j) = sc(j) + pij*sigu
        endif      
80  continue
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
            tempmin = zr(jtranp+90)
            tempmax = zr(jtranp+91)
            do 83 icmp = 1, 12
                mominp(icmp) = A1(icmp)*tempmin+B1(icmp)
                momaxp(icmp) = A1(icmp)*tempmax+B1(icmp)
83          continue
            do 84 j = 1, 6
              do 85 icmp = 1, 12
                mij(icmp)=momaxp(icmp)-mominp(icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sc(j) = sc(j) + mij(icmp)*sigu
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
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sc(j) = sc(j) + mij(icmp)*sigu
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
                                    st(j) =  sb(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                             e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                             e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                             e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                             e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                             e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                             e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
                                    spmec(j) =  spm(j)+e0(i0)*sa(j)+e0(i0)*sc(j) +&
                                                e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                                e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                                e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                                e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                                e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                                e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
113                             continue
                                call rctres(st,tresca)
                                if (tresca .gt. trescapp) then
                                    trescapp = tresca
                                    call rctres(spmec,tresmepp)
                                endif
112                           continue
111                         continue
110                       continue
109                     continue
108                   continue
107                 continue
106               continue
105             continue
104           continue
103         continue
102       continue
101     continue
100   continue
    else
      do 114 i0 = 1, 2
        do 115 j=1,6
            st(j)  = sb(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
            spmec(j)= spm(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
115     continue
        call rctres(st,tresca)
        if (tresca .gt. trescapp) then
            trescapp = tresca
            call rctres(spmec,tresmepp)
        endif
114   continue
    endif
!
    spp    = trescapp+s2pp
    sp(1)  = spp
    spme(1) = tresmepp+s2pp
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
    sqq  = 0.d0
    s2qq = 0.d0
    trescaqq = 0.d0
    tresmeqq = 0.d0
!
!-- partie ze200
    if (ze200) call rcZ2s2('SP', proqi, proqj, seismeze200, mse, s2qq)
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
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sa(j) = sa(j)+mij(icmp)*sigu
140         continue
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+72+j)
            sa(j) = sa(j)+pij*sigu
130     continue
    endif
!
!-- SB partie transitoire
    do 150 j = 1, 6
        sb(j) = 0.d0
        spm(j) = 0.d0
150 continue
    call jelira(jexnom('&&RC3200.SITU_THER', knumes), 'LONUTI', nbtheq)
    call jelira(jexnom('&&RC3200.SITU_PRES', knumes), 'LONUTI', nbpreq)
    call jelira(jexnom('&&RC3200.SITU_MECA', knumes), 'LONUTI', nbmecaq)
    nbq = max(nbtheq,nbpreq, nbmecaq)
    if (nbq .ne. 0) then
        call jelira(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TRANSIT.'//lieu, knumes), 'L', jtranq)
        do 160 j = 1, 6
            sb(j) = zr(jtranq+6+j-1) -zr(jtranq+j-1)
            spm(j)   = zr(jtranq+78+j-1)-zr(jtranq+72+j-1)
160     continue
        instqmin = zr(jtranq+86)
        instqmax = zr(jtranq+87)
    else
        instqmin = -1.0
        instqmax = -1.0
    endif
!
!-- SC la partie unitaire avec interpolation des moments
    do 170 j = 1, 6
        sc(j) = 0.d0
        if (iret2q .ne.0) then
            pij=proqi(1)-proqj(1)
            sigu = zr(jsigu-1+72+j)
            sc(j) = sc(j) + pij*sigu
        endif 
170  continue
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
            tempmin = zr(jtranq+90)
            tempmax = zr(jtranq+91) 
            do 173 icmp = 1, 12
                    mominq(icmp) = A1(icmp)*tempmin+B1(icmp)
                    momaxq(icmp) = A1(icmp)*tempmax+B1(icmp)
173         continue
            do 174 j = 1, 6
              do 175 icmp = 1, 12
                mij(icmp)=momaxq(icmp)-mominq(icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sc(j) = sc(j) + mij(icmp)*sigu
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
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sc(j) = sc(j) + mij(icmp)*sigu
178           continue
177         continue           
        endif
    endif
!
!-- on maximise sur les possibilités de signes
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
                                    st(j) =  sb(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                             e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                             e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                             e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                             e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                             e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                             e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
                                    spmec(j) =  spm(j)         +e0(i0)*sa(j)     +&
                                                e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                                e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                                e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                                e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                                e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                                e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
193                             continue
                                call rctres(st,tresca)
                                if (tresca .gt. trescaqq) then
                                    trescaqq = tresca
                                    call rctres(spmec,tresmeqq)
                                endif
192                           continue
191                         continue
190                       continue
189                     continue
188                   continue
187                 continue
186               continue
185             continue
184           continue
183         continue
182       continue
181     continue
180   continue
    else
      do 194 i0 = 1, 2
        do 195 j=1,6
            st(j)  = sb(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
            spmec(j)= spm(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
195     continue
        call rctres(st,tresca)
        if (tresca .gt. trescaqq) then
            trescaqq = tresca
            call rctres(spmec,tresmeqq)
        endif
194   continue
    endif
!
    sqq = trescaqq+s2qq
!
! -- Comparaison de SP(P,P) et SP(Q,Q)
    if (sqq .gt. spp) then
        sp(1)  = sqq
        sp(2)  = spp
        spme(1) = tresmeqq+s2qq
        spme(2) = tresmepp+s2pp
        instsp(1)= instqmin
        instsp(2)= instqmax
        instsp(3)= instpmin
        instsp(4)= instpmax
        do 196 k = 1, 7
            mat1(k) = proqi(13+k)
            mat2(k) = proqj(13+k)
196     continue
    else
        sp(2)  = sqq
        spme(2) = tresmeqq+s2qq
        instsp(3)= instqmin
        instsp(4)= instqmax
    endif
!
!--------------------- CALCUL DE SP(P,Q)-----------------------------
    s2pq(1) = 0.d0
    s2pq(2) = 0.d0
    spq(1) = 0.d0
    spq(2) = 0.d0
    trescapq(1) = 0.d0
    trescapq(2) = 0.d0
    tresmepq(1) = 0.d0
    tresmepq(2) = 0.d0
!
!-- partie ze200
    if (ze200) then
        call rcZ2s2('SP', propi, proqi, seismeze200, mse, s2pq(1))
        call rcZ2s2('SP', propj, proqj, seismeze200, mse, s2)
        if (s2 .gt. s2pq(1)) then
            s2pq(2) = s2pq(1)
            s2pq(1) = s2
        endif
        call rcZ2s2('SP', propi, proqj, seismeze200, mse, s2)
        if (s2 .gt. s2pq(1)) then
            s2pq(1) = s2
            call rcZ2s2('SP', propj, proqi, seismeze200, mse, s2pq(2))
        endif
        call rcZ2s2('SP', propj, proqi, seismeze200, mse, s2)
        if (s2 .gt. s2pq(1)) then
            s2pq(1) = s2
            call rcZ2s2('SP', propi, proqj, seismeze200, mse, s2pq(2))
        endif
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
            sigu = zr(jsigu-1+72+j)
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
            sigu = zr(jsigu-1+6*(icmp-1)+j)
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
!-- SB la partie transitoire
    if (nbq .ne. 0) then
        if (nbp .ne. 0) then
            do 211 j = 1, 6
                sb1(j)   = zr(jtranp+j-1)    - zr(jtranq+6+j-1)
                sb2(j)   = zr(jtranp+6+j-1)  - zr(jtranq+j-1)
                smemi(j) = zr(jtranp+72+j-1) - zr(jtranq+78+j-1)
                smema(j) = zr(jtranp+78+j-1) - zr(jtranq+72+j-1)
211         continue
        else
            do 212 j = 1, 6
                sb1(j)   = 0.d0    - zr(jtranq+6+j-1)
                sb2(j)   = 0.d0  - zr(jtranq+j-1)
                smemi(j) = 0.d0 - zr(jtranq+78+j-1)
                smema(j) = 0.d0 - zr(jtranq+72+j-1)
212         continue
        endif
    else
        if (nbp .ne. 0) then
            do 213 j = 1, 6
                sb1(j)   = zr(jtranp+j-1)    - 0.d0
                sb2(j)   = zr(jtranp+6+j-1)  - 0.d0
                smemi(j) = zr(jtranp+72+j-1) - 0.d0
                smema(j) = zr(jtranp+78+j-1) - 0.d0
213         continue
        else
            do 214 j = 1, 6
                sb1(j) = 0.d0
                sb2(j) = 0.d0
                smemi(j) = 0.d0
                smema(j) = 0.d0
214         continue
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
                                      smec11(j) = e0(i0)*(smom11(j)+smom21(j)+spres1(j))+&
                                                smemi(j)+seis(j)
                                      smec12(j) = e0(i0)*(smom12(j)+smom21(j)+spres2(j))+&
                                                smemi(j)+seis(j)
                                      smec13(j) = e0(i0)*(smom13(j)+smom21(j)+spres3(j))+&
                                                smemi(j)+seis(j)
                                      smec14(j) = e0(i0)*(smom14(j)+smom21(j)+spres4(j))+&
                                                smemi(j)+seis(j)
                                      smec21(j) = e0(i0)*(smom11(j)+smom22(j)+spres1(j))+&
                                                smema(j)+seis(j)
                                      smec22(j) = e0(i0)*(smom12(j)+smom22(j)+spres2(j))+&
                                                smema(j)+seis(j)
                                      smec23(j) = e0(i0)*(smom13(j)+smom22(j)+spres3(j))+&
                                                smema(j)+seis(j)
                                      smec24(j) = e0(i0)*(smom14(j)+smom22(j)+spres4(j))+&
                                                smema(j)+seis(j)
233                               continue
!
                                  call rctres(st11,tresca)
                                  if (tresca .gt. tresca1(1)) then
                                      tresca1(1) = tresca
                                      call rctres(smec11,tresme1(1))
                                      typpq='PQ'
                                  endif
                                  call rctres(st12,tresca)
                                  if (tresca .gt. tresca1(2)) then
                                      tresca1(2) = tresca
                                      call rctres(smec12,tresme1(2))
                                      typpq='PQ'
                                  endif
                                  call rctres(st13,tresca)
                                  if (tresca .gt. tresca1(3)) then
                                      tresca1(3) = tresca
                                      call rctres(smec13,tresme1(3))
                                      typpq='PQ'
                                  endif
                                  call rctres(st14,tresca)
                                  if (tresca .gt. tresca1(4)) then
                                      tresca1(4) = tresca
                                      call rctres(smec14,tresme1(4))
                                      typpq='PQ'
                                  endif
!
                                  call rctres(st21,tresca)
                                  if (tresca .gt. tresca2(1)) then
                                      tresca2(1) = tresca
                                      call rctres(smec21,tresme2(1))
                                      typpq='QP'
                                  endif
                                  call rctres(st22,tresca)
                                  if (tresca .gt. tresca2(2)) then
                                      tresca2(2) = tresca
                                      call rctres(smec22,tresme2(2))
                                      typpq='QP'
                                  endif
                                  call rctres(st23,tresca)
                                  if (tresca .gt. tresca2(3)) then
                                      tresca2(3) = tresca
                                      call rctres(smec23,tresme2(3))
                                      typpq='QP'
                                  endif
                                  call rctres(st24,tresca)
                                  if (tresca .gt. tresca2(4)) then
                                      tresca2(4) = tresca
                                      call rctres(smec24,tresme2(4))
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
                smec11(j)=smemi(j)+e0(i0)*(smom11(j)+smom21(j)+spres1(j))
                smec12(j)=smemi(j)+e0(i0)*(smom12(j)+smom21(j)+spres2(j))
                smec13(j)=smemi(j)+e0(i0)*(smom13(j)+smom21(j)+spres3(j))
                smec14(j)=smemi(j)+e0(i0)*(smom14(j)+smom21(j)+spres4(j))
                smec21(j)=smema(j)+e0(i0)*(smom11(j)+smom22(j)+spres1(j))
                smec22(j)=smema(j)+e0(i0)*(smom12(j)+smom22(j)+spres2(j))
                smec23(j)=smema(j)+e0(i0)*(smom13(j)+smom22(j)+spres3(j))
                smec24(j)=smema(j)+e0(i0)*(smom14(j)+smom22(j)+spres4(j))
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
   instbid(1) = instpmin
   instbid(2) = instqmax
   instbid(3) = instpmax
   instbid(4) = instqmin
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
        trescapq(1) = tresca2(1)
        tresmepq(1) = tresme2(1)
        do 280 k = 1, 7
            mat1(k) = propi(13+k)
            mat2(k) = proqi(13+k)
280     continue
    endif 
    if (tresca2(2) .gt. trescapq(1)) then
        tres = .false.
        trescapq(1) = tresca2(2)
        tresmepq(1) = tresme2(2)
        do 290 k = 1, 7
            mat1(k) = propj(13+k)
            mat2(k) = proqj(13+k)
290     continue
    endif
    if (tresca2(3) .gt. trescapq(1)) then
        tres = .false.
        trescapq(1) = tresca2(3)
        tresmepq(1) = tresme2(3)
        do 300 k = 1, 7
            mat1(k) = propi(13+k)
            mat2(k) = proqj(13+k)
300     continue
    endif
    if (tresca2(4) .gt. trescapq(1)) then
        tres = .false.
        trescapq(1) = tresca2(4)
        tresmepq(1) = tresme2(4)
        do 310 k = 1, 7
            mat1(k) = propj(13+k)
            mat2(k) = proqi(13+k)
310     continue
    endif
!
    spq(1)=trescapq(1)+s2pq(1)
!
    if(spq(1) .gt. sp(1)) then
        sp(1)  = spq(1)
        spme(1) = tresmepq(1)+s2pq(1)
        if (tres) then
          do 320 kk = 1, 4
            if(tresca2(kk) .gt. trescapq(2)) then
                trescapq(2)=tresca2(kk)
                tresmepq(2)=tresme2(kk)
            endif
320       continue
        else
          instbid(1) = instpmax
          instbid(2) = instqmin
          instbid(3) = instpmin
          instbid(4) = instqmax
          do 330 kk = 1, 4
            if(tresca1(kk) .gt. trescapq(2)) then
                trescapq(2)=tresca1(kk)
                tresmepq(2)=tresme1(kk)
            endif
330       continue
        endif
        spq(2)=trescapq(2)+s2pq(2)
        sp(2)  = spq(2)
        spme(2) = tresmepq(2)+s2pq(2)
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
