subroutine vpzech(d, z, low, high, mm,&
                  neq, iz)
    implicit none
    integer :: low, high, mm, neq, iz
    real(kind=8) :: d(1), z(iz, 1)
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
!     MISE A L'ECHELLE (NORMALISATION) DE LA COLONNE Z PAR LA BONNE
!     VALEUR DE "D"
!     ------------------------------------------------------------------
!     SERT A L'EQUILIBRAGE D'UNE MATRICE POUR LE CALCUL DE SES VALEURS
!     ET VECTEURS PROPRES.
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE 321 (ROUTINE BALBAK)
!     ------------------------------------------------------------------
    integer :: i, j, ii, jj
    real(kind=8) :: s
!     ------------------------------------------------------------------
    do 10 i = low, high
        s = d(i)
        do 5 j = 1, mm
            z(i,j) = z(i,j)*s
 5      continue
10  end do
!
!     --- REPERMUTER LES LIGNES SI CA A ETE FAIT DANS VPZBAL ---
    do 20 ii = low-1, 1, -1
        jj = nint(d(ii))
        if (ii .ne. jj) then
            do 25 j = 1, mm
                s = z(ii,j)
                z(ii,j) = z(jj,j)
                z(jj,j) = s
25          continue
        endif
20  end do
!
    do 30 ii = high+1, neq
        jj = nint(d(ii))
        if (ii .ne. jj) then
            do 35 j = 1, mm
                s = z(ii,j)
                z(ii,j) = z(jj,j)
                z(jj,j) = s
35          continue
        endif
30  end do
end subroutine
