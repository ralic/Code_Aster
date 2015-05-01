subroutine vpzhes(mat, k, l, neq, mxeq,&
                  d)
    implicit none
    integer :: k, l, neq, mxeq
    real(kind=8) :: mat(mxeq, neq), d(neq)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     MISE SOUS FORME DE HESSENBERG (FORME SUPERIEURE)
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE 342
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: m, i, j
    real(kind=8) :: f, g, h, scale, zero
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.d0
    do 45 m = k+1, l-1
        h = zero
        d(m) = zero
        scale = zero
!
!        --- MISE A L'ECHELLE DE LA COLONNE ---
        do 5 i = m, l
            scale = scale + abs(mat(i,m-1))
 5      continue
        if (scale .eq. zero) goto 45
        do 10 i = l, m, -1
            d(i) = mat(i,m-1) / scale
            h = h + d(i) * d(i)
10      continue
        g = -sign(sqrt(h),d(m))
        h = h - d(m) * g
        d(m) = d(m) - g
!
!        --- FORMATION DE  (I-(U*UT)/H) * MAT  ---
        do 25 j = m, neq
            f = zero
            do 15 i = l, m, -1
                f = f + d(i) * mat(i,j)
15          continue
            f = f / h
            do 20 i = m, l
                mat(i,j) = mat(i,j) - f * d(i)
20          continue
25      continue
!
!        --- FORMATION DE (I-(U*UT)/H)*MAT*(I-(U*UT)/H)  ---
        do 40 i = 1, l
            f = zero
            do 30 j = l, m, -1
                f = f + d(j) * mat(i,j)
30          continue
            f = f / h
            do 35 j = m, l
                mat(i,j) = mat(i,j) - f * d(j)
35          continue
40      continue
        d(m) = scale * d(m)
        mat(m,m-1) = scale * g
45  end do
end subroutine
