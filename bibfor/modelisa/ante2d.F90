subroutine ante2d(itria, xbar, ksi1, ksi2)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!  DESCRIPTION : DETERMINATION DE L'ANTECEDENT DANS L'ELEMENT DE
!  -----------   REFERENCE D'UN POINT APPARTENANT A UN ELEMENT REEL
!                LES ELEMENTS CONSIDERES SONT DES ELEMENTS 2D TRIANGLES
!                OU QUADRANGLES
!
!                APPELANT : RECI2D
!
!  IN     : ITRIA  : INTEGER , SCALAIRE
!                    INDICATEUR DU SOUS-DOMAINE AUQUEL APPARTIENT LE
!                    POINT DE L'ELEMENT REEL :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES BARYCENTRIQUES DU POINT DE L'ELEMENT
!                    REEL (BARYCENTRE DES SOMMETS DU TRIANGLE 1-2-3
!                    OU 3-4-1)
!  OUT    : KSI1   : REAL*8 , SCALAIRE
!                    PREMIERE COORDONNEE DU POINT ANTECEDENT DANS LE
!                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
!  OUT    : KSI2   : REAL*8 , SCALAIRE
!                    SECONDE COORDONNEE DU POINT ANTECEDENT DANS LE
!                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: itria
    real(kind=8) :: xbar(*), ksi1, ksi2
!
! VARIABLES LOCALES
! -----------------
    integer :: i, isom(3), j
!
    real(kind=8) :: ksi1el(4), ksi2el(4)
    data          ksi1el / -1.0d0 , -1.0d0 ,  1.0d0 ,  1.0d0 /
    data          ksi2el /  1.0d0 , -1.0d0 , -1.0d0 ,  1.0d0 /
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   AFFECTATION DES NUMEROS DES SOMMETS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (itria .eq. 1) then
        isom(1) = 1
        isom(2) = 2
        isom(3) = 3
    else
        isom(1) = 3
        isom(2) = 4
        isom(3) = 1
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   CALCUL DES COORDONNEES DE L'ANTECEDENT DANS L'ELEMENT DE REFERENCE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    ksi1 = 0.0d0
    ksi2 = 0.0d0
    do 10 i = 1, 3
        j = isom(i)
        ksi1 = ksi1 + xbar(i) * ksi1el(j)
        ksi2 = ksi2 + xbar(i) * ksi2el(j)
10  end do
!
! --- FIN DE ANTE2D.
end subroutine
