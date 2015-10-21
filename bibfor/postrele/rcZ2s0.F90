subroutine rcZ2s0(option, mm, pr, mse,&
                  nbinst, sth, snp)
    implicit   none
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rctres.h"
    integer :: nbinst
    real(kind=8) :: mm(*), pr, mse(*), sth(6), snp
    character(len=4) :: option
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
!!!!!!!!!!!!!!!!!!!!A MODIFIER pour B3200_T!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200 e
!     CALCUL DU SN MAX SUR LES INSTANTS ET LES POSSIBILITES DE SIGNE
!     POUR LES CMP DE SEISME
!    'SITU' OU 'COMB'  ON CALCULE UNE AMPLITUDE (SN,SP) :
!                    MAX(+-SIGM_M + 2 * SIGM_SEISME)
! IN  : MM     : EFFORTS ASSOCIEES A L'ETAT DE CONTRAINTE MECANIQUE
! IN  : PR     : PRESSION
! IN  : MSE    : EFFORTS DUS AU SEISME
! IN  : NBINST : 0 SI MECA PUR / >0 SI TRANSITOIRE THERMOMECA
! IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
! OUT : SNP    : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
!     ------------------------------------------------------------------
!
    integer :: i1, i2, i3, i, icmp, jcorp, jvalin
    integer :: i11, i21, i31, fact
    real(kind=8) :: mtt(6), mtc(6), sij, snp1, tresca
    real(kind=8) :: factp, factm
    real(kind=8) :: e1(2), e2(2), e3(2), e4(2), e5(2), e6(2), e7(2)
! DEB ------------------------------------------------------------------
!
    snp = 0.d0
! ON DISTINGUE LE CAS CLASSIQUE DU CAS CORPS/TUBU POUR OPTIMISER
! LES PERFORMANCES
    call jeveuo('&&RC3200.CORPS', 'L ', jcorp)
! ON RECUPERE LES CARACTERISTIQUES DE LA TUYAUTERIE
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
!
    if (option .eq. 'SN') then 
        factp= zr(jvalin+1)
        factm= zr(jvalin+3)
    else if (option .eq. 'SP') then
        factp= zr(jvalin)*zr(jvalin+1)
        factm= zr(jvalin+2)*zr(jvalin+3)
    endif
!
    fact = 2
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
! CAS CORPS/TUBULURE
                    if (zl(jcorp)) then
                        do 71 i11 = 1, 2
                            mtc(1) = mm(4) + mse(4)*e1( i11)
                            do 61 i21 = 1, 2
                                mtc(2) = mm(5) + mse(5)*e2( i21)
                                do 51 i31 = 1, 2
                                    mtc(3) = mm(6) + mse(6)*e3( i31)
!
                                    sij = 0.d0
                                    do 12 icmp = 1, 3
                                        sij = sij + mtt(icmp)**2
                                        sij = sij + mtc(icmp)*2
12                                  continue
                                    sij = sqrt (sij)*factm*zr(jvalin+4)/(2*zr(jvalin+6))
                                    sij = sij + abs(pr)*factp*zr(jvalin+4)/(2*zr(jvalin+5))
                                    snp = max( snp , sij)
!
51                              continue
61                          continue
71                      continue
!
                    else
                        sij = 0.d0
                        do 102 icmp = 1, 3
                            sij = sij + mtt(icmp)**2
102                     continue
                        sij = sqrt (sij)*factm*zr(jvalin+4)/(2*zr(jvalin+6))
                        sij = sij + abs(pr)*factp*zr(jvalin+4)/(2*zr(jvalin+5))
                        snp = max( snp , sij)
!
                    endif
50              continue
60          continue
70      continue
!
! --- CALCUL AVEC TRANSITOIRE
!     ------------------------
    else
        do 170 i1 = 1, 2
            mtt(1) = mm(1) + mse(1)*e1(i1)
            do 160 i2 = 1, 2
                mtt(2) = mm(2) + mse(2)*e2(i2)
                do 150 i3 = 1, 2
                    mtt(3) = mm(3) + mse(3)*e3(i3)
! CAS CORPS/TUBULURE
                    if (zl(jcorp)) then
                        do 171 i11 = 1, 2
                            mtc(1) = mm(4) + mse(4)*e1( i11)
                            do 161 i21 = 1, 2
                                mtc(2) = mm(5) + mse(5)*e2( i21)
                                do 151 i31 = 1, 2
                                    mtc(3) = mm(6) + mse(6)*e3( i31)
!
                                    sij = 0.d0
                                    do 112 icmp = 1, 3
                                        sij = sij + mtt(icmp)**2
                                        sij = sij + mtc(icmp)*2
112                                 continue
                                    sij = sqrt (sij)*factm*zr(jvalin+4)/(2*zr(jvalin+6))
                                    sij = sij + abs(pr)*factp*zr(jvalin+4)/(2*zr(jvalin+5))
                                    call rctres(sth, tresca)
                                    snp1 = tresca +sij
                                    snp = max(snp , snp1)
151                             continue
161                         continue
171                     continue
!
                    else
                        sij = 0.d0
                        do 1121 icmp = 1, 3
                            sij = sij + mtt(icmp)**2
1121                    continue
                        sij = sqrt (sij)*factm*zr(jvalin+4)/(2*zr(jvalin+6))
                        sij = sij + abs(pr)*factp*zr(jvalin+4)/(2*zr(jvalin+5))
                        call rctres(sth, tresca)
                        snp1 = tresca+sij
                        snp = max( snp , snp1 )
                    endif
!
150              continue
160          continue
170      continue
    endif
!
end subroutine
