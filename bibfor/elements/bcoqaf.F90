subroutine bcoqaf(bm, bf, bc, nbno, bmat)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: bm(3, 1), bf(3, 1), bc(2, 1), bmat(8, 1)
!     ------------------------------------------------------------------
! --- AFFECTATION DES MATRICES B_MEMBRANE (NOTEE (BM)) ET B_FLEXION
! --- (NOTEE (BF)) DES ELEMENTS COQUES A FACETTES PLANES DST, DKT,
! --- DSQ, DKQ OU Q4G A LA MATRICE B COMPLETE (NOTEE BMAT).
!     ------------------------------------------------------------------
!     IN  BM(3,NBNO*2)  : MATRICE B_MEMBRANE
!     IN  BF(3,NBNO*3)  : MATRICE B_FLEXION
!     IN  BC(2,NBNO*3)  : MATRICE B_CISAILLEMENT
!     IN  NBNO          : NOMBRE DE NOEUDS DE L'ELEMENT
!     OUT BMAT(8,NBNO*6): MATRICE B COMPLETE : (BMAT) = !(BM)!
!                                                       !(BF)!
!                                                       !(BC)!
!                        !BM1 0   0    0    0    0 ....!
!                        !0   BM2 0    0    0    0 ....!
!                        !BM2 BM1 0    0    0    0 ....!
!                        !0   0   BF11 BF12 BF13 0 ....!
! PLUS EXACTEMENT BMAT = !0   0   BF21 BF22 BF23 0 ....!
!                        !0   0   BF31 BF32 BF33 0 ....!
!                        !0   0   BC11 BC12 BC13 0 ....!
!                        !0   0   BC21 BC22 BC23 0 ....!
!
!-----------------------------------------------------------------------
    integer :: i, j, k, nbno
!-----------------------------------------------------------------------
!
! --- AFFECTATION DE (BM) A (BMAT)
!     ----------------------------
    do 10 i = 1, nbno
        do 20 k = 1, 2
            do 30 j = 1, 3
                bmat(j,6*(i-1)+k) = bm(j,2*(i-1)+k)
30          continue
20      continue
10  end do
!
! --- AFFECTATION DE (BF) A (BMAT)
!     ----------------------------
    do 40 i = 1, nbno
        do 50 k = 1, 3
            do 60 j = 1, 3
                bmat(3+j,6*(i-1)+k+2) = bf(j,3*(i-1)+k)
60          continue
50      continue
40  end do
!
! --- AFFECTATION DE (BC) A (BMAT)
!     ----------------------------
    do 70 i = 1, nbno
        do 80 k = 1, 3
            do 90 j = 1, 2
                bmat(6+j,6*(i-1)+k+2) = bc(j,3*(i-1)+k)
90          continue
80      continue
70  end do
!
end subroutine
