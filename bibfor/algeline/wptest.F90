subroutine wptest(exclu, xh, xb, vp, neq,&
                  nmax)
    implicit none
!
    complex(kind=8) :: vp, xh(*), xb(*)
    integer :: neq, exclu(*)
    real(kind=8) :: nmax
!     -----------------------------------------------------------------
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
!     TEST SUR LE COUPLAGE HAUT-BAS DU PB GENERALISE ASSOCIE AU PB
!     QUADRATIQUE POUR CHOISIR LA VALEUR PROPRE QUADRATIQUE RACINE
!     D' UNE EQUATION DE SECOND DEGRE (CF WP2VEC)
!
!              NMAX := !! M*XB - VP*M*XH !!
!     -----------------------------------------------------------------
! IN  XH   : C : PARTIE HAUTE D' UN VECTEUR PROPRE DU PB QUAD
! IN  XB   : C : PARTIE BASSE D' UN VECTEUR PROPRE DU PB QUAD
! IN  VP   : C : VALEUR CANDIDATE A LA PROPRETE
! IN  NEQ  : I : TAILLE DES VECTEURS DU PB QUADRATIQUE
! OUT NMAX : R : NORME MAX DU COUPLAGE
!     -----------------------------------------------------------------
    integer :: i
    real(kind=8) :: a
!     -----------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nmax = -1.0d-50
!
!     --- TEST SUR LA PARTIE BASSE : COUPLAGE ----
!
!-DEB
!
!     /* CE QUI SUIT MARCHE POUR UNE MATRICE DE MASSE GENERALE */
!
!     CALL MRMULT('ZERO',LMASSE,XH,'C',V1,1)
!     CALL MRMULT('ZERO',LMASSE,XB,'C',V2,1)
!
!     DO 10, I = 1, NEQ, 1
!        A    = ABS(VP*V1(I)-V2(I))
!        NMAX = MAX(A,NMAX)
!10    CONTINUE
!
!-FIN
!
!     /* CE QUI SUIT MARCHE POUR UNE MATRICE DE MASSE      */
!     /* AVEC DECOMPOSITION BLOC CONFORME A LA DOC DE REF */
!     /* I.E : BLOCS ASSOCIES AUX DDL LAGR A ZERO         */
!
    do 20, i = 1, neq, 1
    a = abs(vp*xh(i)-xb(i))*exclu(i)
    nmax = max(a,nmax)
    20 end do
!
end subroutine
