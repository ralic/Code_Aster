subroutine dktlxy(codi, lcot, hlt2, depf, lambda)
    implicit  none
    real(kind=8) :: hlt2(4, 6), depf(9), codi(*), lcot(*), lambda(4)
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
!     ------------------------------------------------------------------
!     'LAMBDA' DE L'ELEMENT DE PLAQUE DKT
!     ------------------------------------------------------------------
    integer :: i, j, k, nno, nc
    real(kind=8) :: tkt(6, 9), bl(4, 9)
    real(kind=8) :: cl(3), sl(3), cs(3), cu(3), su(3)
!     ------------------------------------------------------------------
!
    nno = 3
    nc = 3
!
    do 100 i = 1, nc
        cl(i) = 6.d0 * codi( i) / lcot(i)
        sl(i) = 6.d0 * codi(nc+i) / lcot(i)
        cs(i) = 3.d0 * codi( i) * codi(nc+i)
        cu(i) = 3.d0 * codi( i) * codi( i)
        su(i) = 3.d0 * codi(nc+i) * codi(nc+i)
100  end do
!
    tkt(1,1) = - cl(1) - cl(1)
    tkt(1,2) = cu(1) + cu(1)
    tkt(1,3) = cs(1) + cs(1)
    tkt(1,4) = cl(1) + cl(1)
    tkt(1,5) = cu(1) + cu(1)
    tkt(1,6) = cs(1) + cs(1)
    tkt(1,7) = 0.d0
    tkt(1,8) = 0.d0
    tkt(1,9) = 0.d0
    tkt(2,1) = cl(3) + cl(3)
    tkt(2,2) = cu(3) + cu(3)
    tkt(2,3) = cs(3) + cs(3)
    tkt(2,4) = 0.d0
    tkt(2,5) = 0.d0
    tkt(2,6) = 0.d0
    tkt(2,7) = - cl(3) - cl(3)
    tkt(2,8) = cu(3) + cu(3)
    tkt(2,9) = cs(3) + cs(3)
    tkt(3,1) = cl(3) - cl(1)
    tkt(3,2) = cu(3) + cu(1)
    tkt(3,3) = cs(3) + cs(1)
    tkt(3,4) = cl(1) + cl(2)
    tkt(3,5) = cu(1) - cu(2)
    tkt(3,6) = cs(1) - cs(2)
    tkt(3,7) = - cl(3) - cl(2)
    tkt(3,8) = cu(3) - cu(2)
    tkt(3,9) = cs(3) - cs(2)
    tkt(4,1) = - sl(1) - sl(1)
    tkt(4,2) = cs(1) + cs(1)
    tkt(4,3) = su(1) + su(1)
    tkt(4,4) = sl(1) + sl(1)
    tkt(4,5) = cs(1) + cs(1)
    tkt(4,6) = su(1) + su(1)
    tkt(4,7) = 0.d0
    tkt(4,8) = 0.d0
    tkt(4,9) = 0.d0
    tkt(5,1) = sl(3) + sl(3)
    tkt(5,2) = cs(3) + cs(3)
    tkt(5,3) = su(3) + su(3)
    tkt(5,4) = 0.d0
    tkt(5,5) = 0.d0
    tkt(5,6) = 0.d0
    tkt(5,7) = - sl(3) - sl(3)
    tkt(5,8) = cs(3) + cs(3)
    tkt(5,9) = su(3) + su(3)
    tkt(6,1) = sl(3) - sl(1)
    tkt(6,2) = cs(3) + cs(1)
    tkt(6,3) = su(3) + su(1)
    tkt(6,4) = sl(1) + sl(2)
    tkt(6,5) = cs(1) - cs(2)
    tkt(6,6) = su(1) - su(2)
    tkt(6,7) = - sl(3) - sl(2)
    tkt(6,8) = cs(3) - cs(2)
    tkt(6,9) = su(3) - su(2)
!
!     ------ LAMDA = HLT2.TKT.DEPF ------------------------------------
    do 150 i = 1, 4
        do 140 j = 1, 3*nno
            bl(i,j) = 0.d0
            do 130 k = 1, 6
                bl(i,j) = bl(i,j) + hlt2(i,k) * tkt(k,j)
130          continue
140      continue
150  end do
    do 190 i = 1, 4
        lambda(i) = 0.d0
190  end do
    do 210 i = 1, 4
        do 200 j = 1, 3*nno
            lambda(i) = lambda(i) + bl(i,j) * depf(j)
200      continue
210  end do
!
end subroutine
