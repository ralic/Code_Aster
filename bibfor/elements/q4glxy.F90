subroutine q4glxy(hlt2, depf, lambda)
    implicit  none
    real(kind=8) :: hlt2(4, 6), depf(12), lambda(4)
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
!     ------------------------------------------------------------------
!     'LAMBDA' DE L'ELEMENT DE PLAQUE Q4G
!     ------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: tb(6, 12)
    real(kind=8) :: blb(4, 12)
!     ------------------------------------------------------------------
!
!       ---- CALCUL DE LA MATRICE TB -------------------------------
    do 200 k = 1, 6
        do 201 j = 1, 12
            tb(k,j) = 0.d0
201      continue
200  end do
    tb(3,2) = 0.25d0
    tb(3,5) = -0.25d0
    tb(3,8) = 0.25d0
    tb(3,11) = -0.25d0
    tb(6,3) = 0.25d0
    tb(6,6) = -0.25d0
    tb(6,9) = 0.25d0
    tb(6,12) = -0.25d0
!
!        -------------- BLB = HLT2.TB ---------------------------
    do 234 i = 1, 4
        do 236 j = 1, 12
            blb(i,j) = 0.d0
            do 238 k = 1, 6
                blb(i,j) = blb(i,j) + hlt2(i,k)*tb(k,j)
238          continue
236      continue
234  end do
!        -------- LAMBDA = BLB.DEPF -----------------------------
    do 240 i = 1, 4
        lambda(i) = 0.d0
240  end do
    do 242 i = 1, 4
        do 244 j = 1, 12
            lambda(i) = lambda(i) + blb(i,j)*depf(j)
244      continue
242  end do
!
end subroutine
