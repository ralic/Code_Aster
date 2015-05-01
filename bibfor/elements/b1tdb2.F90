subroutine b1tdb2(b1, b2, d, jacob, nbsig,&
                  nbinco, btdb)
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
!.======================================================================
    implicit none
!
!       B1TDB2  -- CALCUL DU PRODUIT B1T*D*B2 DONNANT LA MATRICE
!                  DE RIGIDITE ELEMENTAIRE EN FAISANT LE PRODUIT
!                  MATRICIEL MULTIPLIE PAR LE SCALAIRE JACOBIEN*POIDS
!
!   ARGUMENT        E/S  TYPE         ROLE
! B1(NBSIG,NBINCO)  IN     R        MATRICE (B1) CALCULEE AU POINT
! B2(NBSIG,NBINCO)  IN     R        MATRICE (B2) CALCULEE AU POINT
!                                   D'INTEGRATION COURANT ET RELIANT
!                                   LES DEFORMATIONS DU PREMIER ORDRE
!                                   AUX DEPLACEMENTS
! D(NBSIG,NBSIG)    IN     R        MATRICE DE HOOKE DANS LE REPERE
!                                   GLOBAL
! JACOB             IN     R        PRODUIT JACOBIEN*POIDS AU POINT
!                                   D'INTEGRATION COURANT
! NBSIG             IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
!                                   L'ELEMENT
! NBINCO            IN     I        NOMBRE D'INCONNUES SUR L'ELEMENT
! BTDB(NBINCO,NBINCO) OUT  R        MATRICE ELEMENTAIRE DE RIGIDITE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    real(kind=8) :: b1 ( nbsig , nbinco )
    real(kind=8) :: b2 ( nbsig , nbinco )
    real(kind=8) :: d(nbsig, nbsig), jacob, btdb(nbinco, nbinco)
! -----  VARIABLES LOCALES
    real(kind=8) :: tab1( 9 ), tab2( 9 )
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: i, j, j1, j2, nbinco, nbsig
    real(kind=8) :: s, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
!
    do 10 i = 1, nbinco
        do 20 j = 1, nbsig
            tab1(j) = jacob * b1 ( j , i )
20      end do
!
        do 30 j1 = 1, nbsig
            s = zero
            do 40 j2 = 1, nbsig
                s = s + tab1(j2)*d(j1,j2)
40          continue
            tab2(j1) = s
30      continue
!
        do 50 j1 = 1, nbinco
            s = zero
            do 60 j2 = 1, nbsig
                s = s + b2 ( j2 , j1 ) * tab2(j2)
60          continue
!
            btdb(i,j1) = btdb(i,j1) + s
!
50      continue
10  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
