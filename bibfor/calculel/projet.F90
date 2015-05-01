subroutine projet(ndim, npg1, nno, vect, res)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!=======================================================================
!
! DEFINITION :
!     SORTIE AUX NOEUDS A PARTIR DES VALEURS AUX POINTS DE GAUSS
!
! FONCTIONNEMENT :
!        CALCUL DE L'ENTHALPIE AUX NOEUDS 'RES' A PARTIR DE
!             L'ENTHALPIE AUX POINTS D INTEGRATION 'VECT'
!
!=======================================================================
    implicit none
!
#include "asterfort/intrpo.h"
    real(kind=8) :: vloc(3, 27), vl(81), vh(50)
    integer :: j8(8), j27(27)
    real(kind=8) :: vect(50), res(50), mpt(4, 4), mpp(6, 6), mp(3, 3), mpq(4, 4)
!
!-----------------------------------------------------------------------
    integer :: i, ic, in, j, ndim, nno, npg1
!
    real(kind=8) :: a, b, c, cinq, de, s, trois
    real(kind=8) :: un, xab, zero
!-----------------------------------------------------------------------
    data zero,trois,cinq/0.d0,3.d0,5.d0/
    data de,un/.5d0,1.d0/
    data j8/1,5,7,3,2,6,8,4/
    data j27/1,19,25,7,3,21,27,9,10,22,16,4,13,2,11,20,23,26,17,8,5,&
     &     14,12,24,18,6,15/
    data vl/-1.d0,-1.d0,-1.d0,1.d0,-1.d0,-1.d0,1.d0,1.d0,-1.d0,-1.d0,&
     &     1.d0,-1.d0,-1.d0,-1.d0,1.d0,1.d0,-1.d0,1.d0,1.d0,1.d0,1.d0,&
     &     -1.d0,1.d0,1.d0,0.d0,-1.d0,-1.d0,1.d0,0.d0,-1.d0,0.d0,1.d0,&
     &     -1.d0,-1.d0,0.d0,-1.d0,-1.d0,-1.d0,0.d0,1.d0,-1.d0,0.d0,1.d0,&
     &     1.d0,0.d0,-1.d0,1.d0,0.d0,0.d0,-1.d0,1.d0,1.d0,0.d0,1.d0,&
     &     0.d0,1.d0,1.d0,-1.d0,0.d0,1.d0,0.d0,0.d0,-1.d0,0.d0,-1.d0,&
     &     0.d0,1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,-1.d0,0.d0,0.d0,0.d0,0.d0,&
     &     0.d0,0.d0,0.d0,1.d0/
    if (ndim .eq. 2) then
!
!     ========= TRIANGLES ======
!
        if (nno .eq. 3 .or. nno .eq. 6) then
            mp(1,1) = cinq/trois
            mp(1,2) = -un/trois
            mp(1,3) = mp(1,2)
            mp(2,1) = mp(1,2)
            mp(2,2) = mp(1,1)
            mp(2,3) = mp(1,2)
            mp(3,1) = mp(1,2)
            mp(3,2) = mp(1,2)
            mp(3,3) = mp(1,1)
!
            do 20 i = 1, 3
                s = 0.d0
                do 10 j = 1, 3
                    s = s + mp(i,j)*vect(j)
10              continue
                res(i) = s
20          continue
            if (nno .eq. 6) then
                res(4) = de* (res(1)+res(2))
                res(5) = de* (res(2)+res(3))
                res(6) = de* (res(3)+res(1))
            endif
        endif
!
!     ========= QUADRANGLES =====
!
        if (nno .eq. 4 .or. nno .eq. 8) then
            mpq(1,1) = un + sqrt(trois)*de
            mpq(1,2) = -de
            mpq(1,3) = un - sqrt(trois)*de
            mpq(1,4) = mpq(1,2)
            mpq(2,1) = mpq(1,2)
            mpq(2,2) = mpq(1,1)
            mpq(2,3) = mpq(1,2)
            mpq(2,4) = mpq(1,3)
            mpq(3,1) = mpq(1,3)
            mpq(3,2) = mpq(1,2)
            mpq(3,3) = mpq(1,1)
            mpq(3,4) = mpq(1,2)
            mpq(4,1) = mpq(1,2)
            mpq(4,2) = mpq(1,3)
            mpq(4,3) = mpq(1,2)
            mpq(4,4) = mpq(1,1)
!
            do 40 i = 1, 4
                s = 0.d0
                do 30 j = 1, 4
                    s = s + mpq(i,j)*vect(j)
30              continue
                res(i) = s
40          continue
            if (nno .eq. 8) then
                res(5) = de* (res(1)+res(2))
                res(6) = de* (res(2)+res(3))
                res(7) = de* (res(3)+res(4))
                res(8) = de* (res(4)+res(1))
            endif
        endif
!
    endif
!
    if (ndim .eq. 3) then
!
!     ========= HEXAEDRES ======
!
        if (nno .eq. 8 .or. nno .eq. 20 .or. nno .eq. 27) then
!
!-----CONSTRUCTION DE VLOC
!
            do 50 i = 1, 50
                res(i) = zero
50          continue
!-------- MISE A ZERO DE VLOC
            do 70 i = 1, 3
                do 60 j = 1, 27
                    vloc(i,j) = zero
60              continue
70          continue
!
            xab = sqrt(trois)
            if (nno .eq. 20 .or. nno .eq. 27) xab = sqrt(cinq/trois)
!
            do 90 i = 1, 3
                do 80 j = 1, nno
                    vloc(i,j) = xab*vl(i+ (j-1)*3)
80              continue
90          continue
!
!       CALCUL AUX NOEUDS
!
!-----BOUCLE SUR LE NOMBRE DE NOEUDS NNO
!
            do 110 in = 1, nno
                call intrpo(vloc(1, in), vloc(2, in), vloc(3, in), nno, vh)
                do 100 j = 1, npg1
                    ic = j27(j)
                    if (nno .eq. 8) ic = j8(j)
                    res(in) = res(in) + vh(j)*vect(ic)
100              continue
110          continue
!
        endif
!
!     ========= TETRAEDRES ======
!
        if (nno .eq. 4 .or. nno .eq. 10) then
            a = (5.d0-sqrt(5.d0))/20.d0
            b = (5.d0+3.d0*sqrt(5.d0))/20.d0
            c = a - b
            mpt(1,1) = a/c
            mpt(1,2) = mpt(1,1)
            mpt(1,3) = (a-1.d0)/c
            mpt(1,4) = mpt(1,1)
            mpt(2,1) = mpt(1,1)
            mpt(2,2) = mpt(1,3)
            mpt(2,3) = mpt(1,1)
            mpt(2,4) = mpt(1,1)
            mpt(3,1) = mpt(1,3)
            mpt(3,2) = mpt(1,1)
            mpt(3,3) = mpt(1,1)
            mpt(3,4) = mpt(1,1)
            mpt(4,1) = mpt(1,1)
            mpt(4,2) = mpt(1,1)
            mpt(4,3) = mpt(1,1)
            mpt(4,4) = mpt(1,3)
!
            do 130 i = 1, 4
                s = 0.d0
                do 120 j = 1, 4
                    s = s + mpt(i,j)*vect(j)
120              continue
                res(i) = s
130          continue
            if (nno .eq. 10) then
                res(5) = de* (res(1)+res(2))
                res(6) = de* (res(2)+res(3))
                res(7) = de* (res(3)+res(1))
                res(8) = de* (res(1)+res(4))
                res(9) = de* (res(2)+res(4))
                res(10) = de* (res(3)+res(4))
            endif
!
        endif
!
!     ========= PENTAEDRES =====
!
        if (nno .eq. 6 .or. nno .eq. 15) then
            a = (sqrt(3.d0)+1.d0)/2.d0
            mpp(1,1) = a
            mpp(1,2) = -a
            mpp(1,3) = a
            mpp(1,4) = 1.d0 - a
            mpp(1,5) = a - 1.d0
            mpp(1,6) = 1.d0 - a
            mpp(2,1) = a
            mpp(2,2) = a
            mpp(2,3) = -a
            mpp(2,4) = 1.d0 - a
            mpp(2,5) = 1.d0 - a
            mpp(2,6) = a - 1.d0
            mpp(3,1) = -a
            mpp(3,2) = a
            mpp(3,3) = a
            mpp(3,4) = a - 1.d0
            mpp(3,5) = 1.d0 - a
            mpp(3,6) = 1.d0 - a
            mpp(4,1) = 1.d0 - a
            mpp(4,2) = a - 1.d0
            mpp(4,3) = 1.d0 - a
            mpp(4,4) = a
            mpp(4,5) = -a
            mpp(4,6) = a
            mpp(5,1) = 1.d0 - a
            mpp(5,2) = 1.d0 - a
            mpp(5,3) = a - 1.d0
            mpp(5,4) = a
            mpp(5,5) = a
            mpp(5,6) = -a
            mpp(6,1) = a - 1.d0
            mpp(6,2) = 1.d0 - a
            mpp(6,3) = 1.d0 - a
            mpp(6,4) = -a
            mpp(6,5) = a
            mpp(6,6) = a
!
            do 150 i = 1, 6
                s = 0.d0
                do 140 j = 1, 6
                    s = s + mpp(i,j)*vect(j)
140              continue
                res(i) = s
150          continue
            if (nno .eq. 15) then
                res(7) = de* (res(1)+res(2))
                res(8) = de* (res(2)+res(3))
                res(9) = de* (res(3)+res(1))
                res(10) = de* (res(4)+res(1))
                res(11) = de* (res(5)+res(2))
                res(12) = de* (res(6)+res(3))
                res(13) = de* (res(5)+res(4))
                res(14) = de* (res(5)+res(6))
                res(15) = de* (res(6)+res(4))
            endif
        endif
!
    endif
!
end subroutine
