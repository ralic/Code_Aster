subroutine rc32s0(option, mm, pr, mse, sigun,&
                  nbinst, sth, snp)
    implicit   none
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rctres.h"
    integer :: nbinst
    real(kind=8) :: mm(*), pr, mse(*), sigun(*), sth(6*nbinst), snp
    character(len=4) :: option
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
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     CALCUL DU SN MAX SUR LES INSTANTS ET LES 64 POSSIBILITES DE SIGNE
!     POUR LES CMP DE SEISME
!     64 POSSIBLITES DE SIGNE DE MSE
!
! IN  : OPTION : ='PMPB'  MAX(+-SIGM_M + SIGM_SEISME)
!              : ='SITU' OU 'COMB'  ON CALCULE UNE AMPLITUDE (SN,SP) :
!                        MAX(+-SIGM_M + 2 * SIGM_SEISME)
! IN  : MM     : EFFORTS ASSOCIEES A L'ETAT DE CONTRAINTE MECANIQUE
! IN  : PR     : PRESSION
! IN  : MSE    : EFFORTS DUS AU SEISME
! IN  : SIGUN  : CONTRAINTES UNITAIRES
! IN  : NBINST : 0 SI MECA PUR / >0 SI TRANSITOIRE THERMOMECA
! IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
! OUT : SNP    : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
!     ------------------------------------------------------------------
!
    integer :: i1, i2, i3, i4, i5, i6, i, icmps, icmp, jcorp
    integer :: i11, i21, i31, i41, i51, i61, fact
    real(kind=8) :: mtt(6), mtc(6), sij(6), sijt(6), snp1, sth1, tresca
    real(kind=8) :: sigt, sigc, sigp
    real(kind=8) :: e1(2), e2(2), e3(2), e4(2), e5(2), e6(2), e7(2)
! DEB ------------------------------------------------------------------
!
    snp = 0.d0
! ON DISTINGUE LE CAS CLASSIQUE DU CAS CORPS/TUBU POUR OPTIMISER
! LES PERFORMANCES
    call jeveuo('&&RC3200.CORPS', 'L ', jcorp)
!
    fact = 2
    if (option .eq. 'PMPB') fact = 1
    do 2 i = 1, 2
        i1 = 2*(i-2)+1
        e1(i) = i1 * fact
        e2(i) = i1 * fact
        e3(i) = i1 * fact
        e4(i) = i1 * fact
        e5(i) = i1 * fact
        e6(i) = i1 * fact
        e7(i) = i1
 2  end do
!
! --- CALCUL MECANIQUE :
!     ----------------
    if (nbinst .eq. 0) then
        do 70 i1 = 1, 2
            mtt(1) = mm(1) + mse(1)*e1(i1)
            do 60 i2 = 1, 2
                mtt(2) = mm(2) + mse(2)*e2(i2)
                do 50 i3 = 1, 2
                    mtt(3) = mm(3) + mse(3)*e3(i3)
                    do 40 i4 = 1, 2
                        mtt(4) = mm(4) + mse(4)*e4(i4)
                        do 30 i5 = 1, 2
                            mtt(5) = mm(5) + mse(5)*e5(i5)
                            do 20 i6 = 1, 2
                                mtt(6) = mm(6) + mse(6)*e6(i6)
! CAS CORPS/TUBULURE
                                if (zl(jcorp)) then
                                    do 71 i11 = 1, 2
                                        mtc(1) = mm(7) + mse(7)*e1( i11)
                                        do 61 i21 = 1, 2
                                            mtc(2) = mm(8) + mse(8)*e2( i21)
                                            do 51 i31 = 1, 2
                                                mtc(3) = mm(9) + mse(9)*e3( i31)
                                                do 41 i41 = 1, 2
                                                    mtc(4) = mm(10) + mse(10)*e4( i41)
                                                    do 31 i51 = 1, 2
                                                        mtc(5) = mm(11) + mse(11)*e5( i51)
                                                        do 21 i61 = 1, 2
                                                            mtc(6) = mm(12) + mse(12)*e6( i61)
!
                                                            do 10 icmps = 1, 6
                                                                sij(icmps) = 0.d0
                                                                do 12 icmp = 1, 6
                                                                    sigt = sigun(&
                                                                           6*(icmp-1 )+icmps)
                                                                    sigc = sigun(&
                                                                           6*(icmp+6-1 )+ icmps)
                                                                    sij(icmps) = sij(icmps) + mtt&
                                                                                 &( icmp)*sigt
                                                                    sij(icmps) = sij(icmps) + mtc&
                                                                                 &( icmp)*sigc
12                                                              continue
                                                                sigp = sigun(72+icmps)
                                                                sij(icmps) = sij(icmps&
                                                                             ) + pr* sigp
10                                                          continue
!
                                                            call rctres(sij, tresca)
                                                            snp1 = tresca
                                                            snp = max( snp , snp1)
!
21                                                      continue
31                                                  continue
41                                              continue
51                                          continue
61                                      continue
71                                  continue
!
                                else
                                    do 101 icmps = 1, 6
                                        sij(icmps) = 0.d0
                                        do 102 icmp = 1, 6
                                            sigt = sigun(6*(icmp-1)+icmps)
                                            sij(icmps) = sij(icmps) + mtt( icmp)*sigt
102                                      continue
                                        sigp = sigun(72+icmps)
                                        sij(icmps) = sij(icmps) + pr* sigp
101                                  continue
                                    call rctres(sij, tresca)
                                    snp1 = tresca
                                    snp = max( snp , snp1 )
!
                                endif
20                          continue
30                      continue
40                  continue
50              continue
60          continue
70      continue
!
! --- CALCUL THERMOMECANIQUE (DEPENDANT DU TEMPS)
!     -------------------------------------------
    else
        do 170 i1 = 1, 2
            mtt(1) = mm(1) + mse(1)*e1(i1)
            do 160 i2 = 1, 2
                mtt(2) = mm(2) + mse(2)*e2(i2)
                do 150 i3 = 1, 2
                    mtt(3) = mm(3) + mse(3)*e3(i3)
                    do 140 i4 = 1, 2
                        mtt(4) = mm(4) + mse(4)*e4(i4)
                        do 130 i5 = 1, 2
                            mtt(5) = mm(5) + mse(5)*e5(i5)
                            do 120 i6 = 1, 2
                                mtt(6) = mm(6) + mse(6)*e6(i6)
! CAS CORPS/TUBULURE
                                if (zl(jcorp)) then
                                    do 171 i11 = 1, 2
                                        mtc(1) = mm(7) + mse(7)*e1( i11)
                                        do 161 i21 = 1, 2
                                            mtc(2) = mm(8) + mse(8)*e2( i21)
                                            do 151 i31 = 1, 2
                                                mtc(3) = mm(9) + mse(9)*e3( i31)
                                                do 141 i41 = 1, 2
                                                    mtc(4) = mm(10) + mse(10)*e4( i41)
                                                    do 131 i51 = 1, 2
                                                        mtc(5) = mm(11) + mse(11)*e5( i51)
                                                        do 121 i61 = 1, 2
                                                            mtc(6) = mm(12) + mse(12)*e6( i61)
!
                                                            do 110 icmps = 1, 6
                                                                sij(icmps) = 0.d0
                                                                do 112 icmp = 1, 6
                                                                    sigt = sigun(&
                                                                           6*(icmp-1 )+icmps)
                                                                    sigc = sigun(&
                                                                           6*(icmp+6-1 )+ icmps)
                                                                    sij(icmps) = sij(icmps) + mtt&
                                                                                 &( icmp)*sigt
                                                                    sij(icmps) = sij(icmps) + mtc&
                                                                                 &( icmp)*sigc
112                                                              continue
                                                                sigp = sigun(72+icmps)
                                                                sij(icmps) = sij(icmps&
                                                                             ) + pr* sigp
110                                                          continue
                                                            do 202 i = 1, 2
                                                                do 204 icmp = 1, 6
                                                                    sth1 = sth(icmp)
                                                                    sijt(icmp) = sij(icmp)*e7(i) &
                                                                                 &+ sth1
204                                                              continue
                                                                call rctres(sijt, tresca)
                                                                snp1 = tresca
                                                                snp = max(snp , snp1)
202                                                          continue
121                                                      continue
131                                                  continue
141                                              continue
151                                          continue
161                                      continue
171                                  continue
!
                                else
                                    do 1101 icmps = 1, 6
                                        sij(icmps) = 0.d0
                                        do 1121 icmp = 1, 6
                                            sigt = sigun(6*(icmp-1)+icmps)
                                            sij(icmps) = sij(icmps) + mtt( icmp)*sigt
1121                                      continue
                                        sigp = sigun(72+icmps)
                                        sij(icmps) = sij(icmps) + pr* sigp
1101                                  continue
                                    do 2021 i = 1, 2
                                        do 2041 icmp = 1, 6
                                            sth1 = sth(icmp)
                                            sijt(icmp) = sij(icmp)*e7(i) + sth1
2041                                      continue
                                        call rctres(sijt, tresca)
                                        snp1 = tresca
                                        snp = max( snp , snp1 )
2021                                  continue
                                endif
!
120                          continue
130                      continue
140                  continue
150              continue
160          continue
170      continue
    endif
!
end subroutine
