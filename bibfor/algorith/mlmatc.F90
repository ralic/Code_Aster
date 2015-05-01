subroutine mlmatc(ni, nk, nj, a, b,&
                  c)
    implicit   none
    integer :: ni, nk, nj
    complex(kind=8) :: a(ni, *), b(nk, *), c(ni, *)
! ----------------------------------------------------------------------
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
!     CALCUL COMPLEXE MATRICIEL C = A * B
!     MATRICE ORDONNEES PAR COLONNES DESCENDANTES
! ----------------------------------------------------------------------
! IN  : A  : MATRICE A(NI,NK)
!     : B  : MATRICE B(NK,NJ)
!     : C  : MATRICE C(NI,NJ)
!     : NI , NJ ,NK : DIMENSIONS DES MATRICES
!     ------------------------------------------------------------------
    integer :: i, j, k
    complex(kind=8) :: xcres
!
    do 1 i = 1, ni
        do 2 j = 1, nj
            xcres = dcmplx(0.d0,0.d0)
            do 3 k = 1, nk
                xcres = xcres + a(i,k) * b(k,j)
 3          continue
            c(i,j) = xcres
 2      continue
 1  end do
!
end subroutine
