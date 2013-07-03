subroutine poeihm(nomte, option, modint, jgao, nno1,&
                  nno2, ncmp, nvim, vpg, vno)
    implicit     none
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/ppgan2.h"
    integer :: jgao, ncmp, nvim
    real(kind=8) :: vno(*), vpg(*)
    character(len=3) :: modint
    character(len=8) :: lielrf(10)
    character(len=16) :: option, nomte
!
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- ROUTINE DE POST-TRAITEMENT JOINT HM --------------------------
! =====================================================================
! IN NOMTE  : NOM DE L'ELEMENT
! IN OPTION : OPTION DE CALCUL
! IN MODINT : MODE D'INTEGRATION
! IN JGAO : MATRICE DE PASSAGE NOEUDS -> POINTS D'INTEGRATION
! IN NNO1 : NOMBRE DE NOEUDS BORD INF ET SUP
! IN NNO2 : NOMBRE DE NOEUDS SEGMENT MILIEU
! IN NCMP :
! IN NVIM :
! IN VPG  : CHAMPS AUX POINTS D'INTEGRATION
! =====================================================================
! OUT VNO : CHAMPS AUX NOEUDS DE L'ELEMENT
! =====================================================================
    integer :: i, j, ibid1, ibid2, ibid3, jgapg1, jgaso1, jgapg2, jgaso2
    integer :: ndim, nno, npg, ndim2, nno2, nnos2, npg2, nno3, nnos3
    integer :: nno1, nnos1
    integer :: nvmax, npgmax, nnosma, dimmax, nnomax, ntrou
    parameter (nvmax  = 60)
    parameter (npgmax = 8 )
    parameter (nnosma = 8 )
    parameter (dimmax = 31)
    parameter (nnomax = 20)
    real(kind=8) :: vpg1(npgmax*nvmax), vpg2(nnosma*nvmax)
    real(kind=8) :: spg1(npgmax*dimmax), spg2(nnosma*dimmax)
    real(kind=8) :: varpg1(nnomax*nvmax), varso1(nnomax*nvmax)
    real(kind=8) :: varpg2(nnomax*nvmax), varso2(nnomax*nvmax)
    real(kind=8) :: sefpg1(nnomax*dimmax), sefso1(nnomax*dimmax)
    real(kind=8) :: sefpg2(nnomax*dimmax), sefso2(nnomax*dimmax)
    real(kind=8) :: vno1(nnomax*dimmax)
    integer :: next(3), next2(3), nmil(2)
!
    data next  /1,2,5/
    data next2 /4,3,7/
    data nmil  /8,6/
! =====================================================================
    if (modint .ne. 'RED') then
        call ppgan2(jgao, 1, ncmp, vpg, vno1)
!
        do 110 i = 1, nno1
            do 120 j = 1, nvim
                vno((next(i)-1)*ncmp+j) = vno1((i-1)*ncmp+j)
                vno((next2(i)-1)*ncmp+j) = vno1((i-1)*ncmp+j)
120          continue
            do 130 j = nvim+1, ncmp
                vno((next(i)-1)*ncmp+j) = vno1((i-1)*ncmp+j)
                vno((next2(i)-1)*ncmp+j) = vno1((i-1)*ncmp+j)
130          continue
110      continue
        do 140 i = 1, nno2
            do 150 j = 1, nvim
                vno((nmil(i)-1)*ncmp+j) = vno1((i-1)*ncmp+j)
150          continue
            do 160 j = nvim+1, ncmp
                vno((nmil(i)-1)*ncmp+j) = vno1((i-1)*ncmp+j)
160          continue
140      continue
!
    else
!
! =====================================================================
! --- MATRICE DE PASSAGE POINTS DE GAUSS -> SOMMETS JGAPG ------------
! =====================================================================
        call elref2(nomte, 2, lielrf, ntrou)
!
!
        call elref4(lielrf(1), 'MASS', ndim, nno1, nnos1,&
                    npg, ibid1, ibid2, ibid3, jgapg1)
!
        call elref4(lielrf(2), 'MASS', ndim, nno2, nnos2,&
                    npg, ibid1, ibid2, ibid3, jgapg2)
! =====================================================================
! --- MATRICE DE PASSAGE SOMMETS -> SOMMETS : JGASO ------------------
! =====================================================================
        call elref4(lielrf(1), 'NOEU_S', ndim2, nno3, nnos3,&
                    npg2, ibid1, ibid2, ibid3, jgaso1)
!
        call elref4(lielrf(2), 'NOEU_S', ndim2, nno3, nnos3,&
                    npg2, ibid1, ibid2, ibid3, jgaso2)
!
        nno=2*nno1+nno2
! =====================================================================
! --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
! =====================================================================
        call assert(nno .le. nnomax)
        call assert(npg .le. npgmax)
        call assert(nnos1 .le. nnosma)
        if (option .eq. 'SIEF_ELNO  ') then
! =====================================================================
! --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
! =====================================================================
            call assert(ncmp .le. dimmax)
            do 100 i = 1, ncmp*npg
                spg1(i) = vpg(i)
100          continue
            do 200 i = 1, ncmp*npg2
                spg2(i) = vpg(ncmp*npg+i)
200          continue
            call ppgan2(jgapg1, 1, ncmp, spg1, sefpg1)
            call ppgan2(jgaso1, 1, ncmp, spg2, sefso1)
            do 10 i = 1, nno1
                do 20 j = 1, nvim
                    vno((next(i)-1)*ncmp+j) = sefpg1((i-1)*ncmp+j)
                    vno((next2(i)-1)*ncmp+j) = sefpg1((i-1)*ncmp+j)
20              continue
                do 30 j = nvim+1, ncmp
                    vno((next(i)-1)*ncmp+j) = sefso1((i-1)*ncmp+j)
                    vno((next2(i)-1)*ncmp+j) = sefso1((i-1)*ncmp+j)
30              continue
10          continue
            call ppgan2(jgapg2, 1, ncmp, spg1, sefpg2)
            call ppgan2(jgaso2, 1, ncmp, spg2, sefso2)
            do 40 i = 1, nno2
                do 50 j = 1, nvim
                    vno((nmil(i)-1)*ncmp+j) = sefpg1((i-1)*ncmp+j)
50              continue
                do 60 j = nvim+1, ncmp
                    vno((nmil(i)-1)*ncmp+j) = sefso1((i-1)*ncmp+j)
60              continue
40          continue
        endif
        if (option .eq. 'VARI_ELNO  ') then
! =====================================================================
! --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
! =====================================================================
            call assert(ncmp .le. nvmax)
            do 300 i = 1, ncmp*npg
                vpg1(i) = vpg(i)
300          continue
            do 400 i = 1, ncmp*npg2
                vpg2(i) = vpg(ncmp*npg+i)
400          continue
            call ppgan2(jgapg1, 1, ncmp, vpg1, varpg1)
            call ppgan2(jgaso1, 1, ncmp, vpg2, varso1)
            do 70 i = 1, nno1
                do 80 j = 1, nvim
                    vno((next(i)-1)*ncmp+j) = varpg1((i-1)*ncmp+j)
                    vno((next2(i)-1)*ncmp+j) = varpg1((i-1)*ncmp+j)
80              continue
                do 90 j = nvim+1, ncmp
                    vno((next(i)-1)*ncmp+j) = varso1((i-1)*ncmp+j)
                    vno((next2(i)-1)*ncmp+j) = varso1((i-1)*ncmp+j)
90              continue
70          continue
            call ppgan2(jgapg2, 1, ncmp, vpg1, varpg2)
            call ppgan2(jgaso2, 1, ncmp, vpg2, varso2)
            do 71 i = 1, nno2
                do 81 j = 1, nvim
                    vno((nmil(i)-1)*ncmp+j) = varpg1((i-1)*ncmp+j)
81              continue
                do 91 j = nvim+1, ncmp
                    vno((nmil(i)-1)*ncmp+j) = varso1((i-1)*ncmp+j)
91              continue
71          continue
        endif
    endif
! =====================================================================
end subroutine
