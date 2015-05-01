subroutine dstcis(dci, carat3, hft2, bca, an)
    implicit  none
#include "asterfort/mgauss.h"
    real(kind=8) :: dci(2, 2), carat3(*), hft2(2, 6), bca(2, 3), an(3, 9)
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
!     --------------------------------------------------------
!     MATRICES BCA(2,3) ET AN(3,9) DU CISAILLEMENT POUR LE DST
!     --------------------------------------------------------
!
    integer :: i, j, k, iret
    real(kind=8) :: l(3), c(3), s(3), x(3), y(3), det
    real(kind=8) :: ta(6, 3), db(2, 3), aa(3, 3), aai(3, 3), aw(3, 9)
!     ------------------------------------------------------------------
    c(1) = carat3(16)
    c(2) = carat3(17)
    c(3) = carat3(18)
    s(1) = carat3(19)
    s(2) = carat3(20)
    s(3) = carat3(21)
    l(1) = carat3(13)
    l(2) = carat3(14)
    l(3) = carat3(15)
    x(1) = carat3(1)
    x(2) = carat3(2)
    x(3) = carat3(3)
    y(1) = carat3(4)
    y(2) = carat3(5)
    y(3) = carat3(6)
!
    do 100 k = 1, 6
        do 101 j = 1, 3
            ta(k,j) = 0.d0
101      continue
100  end do
    ta(1,1) = - 8.d0 * c(1)
    ta(2,3) = - 8.d0 * c(3)
    ta(3,1) = - 4.d0 * c(1)
    ta(3,2) = 4.d0 * c(2)
    ta(3,3) = - 4.d0 * c(3)
    ta(4,1) = - 8.d0 * s(1)
    ta(5,3) = - 8.d0 * s(3)
    ta(6,1) = - 4.d0 * s(1)
    ta(6,2) = 4.d0 * s(2)
    ta(6,3) = - 4.d0 * s(3)
!     -------------- PRODUIT HFT2.TA -----------------------------------
    do 110 i = 1, 2
        do 111 j = 1, 3
            bca(i,j) = 0.d0
111      end do
110  end do
    do 120 j = 1, 3
        do 120 k = 1, 6
            bca(1,j) = bca(1,j) + hft2(1,k) * ta(k,j)
            bca(2,j) = bca(2,j) + hft2(2,k) * ta(k,j)
120      continue
!     -------------- PRODUIT DCI.BCA -----------------------------------
    do 130 j = 1, 3
        db(1,j) = dci(1,1) * bca(1,j) + dci(1,2) * bca(2,j)
        db(2,j) = dci(2,1) * bca(1,j) + dci(2,2) * bca(2,j)
130  end do
!     -------------- CALCUL DE AA --------------------------------------
    do 150 i = 1, 3
        do 140 j = 1, 3
            aa(i,j) = - (x(i) * db(1,j) + y(i) * db(2,j))
140      continue
        aa(i,i) = aa(i,i) + 2.d0/3.d0 * l(i)
150  end do
!     -------------- INVERSION DE AA -----------------------------------
    do 155 i = 1, 3
        do 157 j = 1, 3
            aai(i,j) = 0.d0
157      end do
155  end do
    do 156 i = 1, 3
        aai(i,i) = 1.d0
156  end do
    call mgauss('NFVP', aa, aai, 3, 3,&
                3, det, iret)
!
!     -------------- CALCUL DE AW --------------------------------------
    do 160 i = 1, 3
        do 161 j = 1, 9
            aw(i,j) = 0.d0
161      end do
160  end do
    aw(1,1) = 1.d0
    aw(1,2) = - x(1)/2.d0
    aw(1,3) = - y(1)/2.d0
    aw(1,4) = - 1.d0
    aw(1,5) = - x(1)/2.d0
    aw(1,6) = - y(1)/2.d0
    aw(2,4) = 1.d0
    aw(2,5) = - x(2)/2.d0
    aw(2,6) = - y(2)/2.d0
    aw(2,7) = - 1.d0
    aw(2,8) = - x(2)/2.d0
    aw(2,9) = - y(2)/2.d0
    aw(3,1) = - 1.d0
    aw(3,2) = - x(3)/2.d0
    aw(3,3) = - y(3)/2.d0
    aw(3,7) = 1.d0
    aw(3,8) = - x(3)/2.d0
    aw(3,9) = - y(3)/2.d0
!
!     -------------- PRODUIT AAI.AW ------------------------------------
    do 170 i = 1, 3
        do 171 j = 1, 9
            an(i,j) = 0.d0
171      end do
170  end do
    do 180 i = 1, 3
        do 180 k = 1, 3
            do 180 j = 1, 9
                an(i,j) = an(i,j) + aai(i,k) * aw(k,j)
180          continue
!
end subroutine
