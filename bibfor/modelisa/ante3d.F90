subroutine ante3d(nbsom, itetra, xbar, ksi1, ksi2,&
                  ksi3)
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
!                LES ELEMENTS CONSIDERES SONT DES ELEMENTS 3D TETRAEDRES
!                OU PYRAMIDES OU PENTAEDRES OU HEXAEDRES
!
!                APPELANT : RECI3D
!
!  IN     : NBSOM  : INTEGER , SCALAIRE
!                    NOMBRE DE SOMMETS DE L'ELEMENT REEL
!  IN     : ITETRA : INTEGER , SCALAIRE
!                    INDICATEUR DU SOUS-DOMAINE TETRAEDRE AUQUEL
!                    APPARTIENT LE POINT DE L'ELEMENT REEL
!                    SI ELEMENT REEL TETRAEDRE : ITETRA = 1
!                    SI ELEMENT REEL PYRAMIDE  : ITETRA = 1 OU 2
!                    SI ELEMENT REEL PENTAEDRE : ITETRA = 1 OU 2 OU 3
!                    SI ELEMENT REEL HEXAEDRE  : ITETRA = 1 OU 2 OU 3 OU
!                                                         4 OU 5 OU 6
!  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 4
!                    COORDONNEES BARYCENTRIQUES DU POINT DE L'ELEMENT
!                    REEL (BARYCENTRE DES SOMMETS DU SOUS-DOMAINE
!                    TETRAEDRE AUQUEL IL APPARTIENT)
!  OUT    : KSI1   : REAL*8 , SCALAIRE
!                    PREMIERE COORDONNEE DU POINT ANTECEDENT DANS LE
!                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
!  OUT    : KSI2   : REAL*8 , SCALAIRE
!                    DEUXIEME COORDONNEE DU POINT ANTECEDENT DANS LE
!                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
!  OUT    : KSI3   : REAL*8 , SCALAIRE
!                    TROISIEME COORDONNEE DU POINT ANTECEDENT DANS LE
!                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbsom, itetra
    real(kind=8) :: xbar(*), ksi1, ksi2, ksi3
!
! VARIABLES LOCALES
! -----------------
    integer :: i, isom(4), j
!
    real(kind=8) :: xtet(4), ytet(4), ztet(4), xpyr(5), ypyr(5), zpyr(5)
    real(kind=8) :: xpen(6), ypen(6), zpen(6), xhex(8), yhex(8), zhex(8)
!
    data          xtet /  0.0d0 ,  0.0d0 ,  0.0d0 ,  1.0d0 /
    data          ytet /  1.0d0 ,  0.0d0 ,  0.0d0 ,  0.0d0 /
    data          ztet /  0.0d0 ,  1.0d0 ,  0.0d0 ,  0.0d0 /
!
    data          xpyr /  1.0d0 ,  0.0d0 , -1.0d0 ,  0.0d0 ,  0.0d0 /
    data          ypyr /  0.0d0 ,  1.0d0 ,  0.0d0 , -1.0d0 ,  0.0d0 /
    data          zpyr /  0.0d0 ,  0.0d0 ,  0.0d0 ,  0.0d0 ,  1.0d0 /
!
    data          xpen / -1.0d0 , -1.0d0 , -1.0d0 ,  1.0d0 ,  1.0d0 ,&
     &                      1.0d0 /
    data          ypen /  1.0d0 ,  0.0d0 ,  0.0d0 ,  1.0d0 ,  0.0d0 ,&
     &                      0.0d0 /
    data          zpen /  0.0d0 ,  1.0d0 ,  0.0d0 ,  0.0d0 ,  1.0d0 ,&
     &                      0.0d0 /
!
    data          xhex / -1.0d0 ,  1.0d0 ,  1.0d0 , -1.0d0 , -1.0d0 ,&
     &                      1.0d0 ,  1.0d0 , -1.0d0 /
    data          yhex / -1.0d0 , -1.0d0 ,  1.0d0 ,  1.0d0 , -1.0d0 ,&
     &                     -1.0d0 ,  1.0d0 ,  1.0d0 /
    data          zhex / -1.0d0 , -1.0d0 , -1.0d0 , -1.0d0 ,  1.0d0 ,&
     &                      1.0d0 ,  1.0d0 ,  1.0d0 /
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    ksi1 = 0.0d0
    ksi2 = 0.0d0
    ksi3 = 0.0d0
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   CAS D'UN ELEMENT REEL TETRAEDRE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (nbsom .eq. 4) then
!
! 1.1    CALCUL DES COORDONNEES DE L'ANTECEDENT
! ---    DANS L'ELEMENT DE REFERENCE
!
        do 10 i = 1, 4
            ksi1 = ksi1 + xbar(i) * xtet(i)
            ksi2 = ksi2 + xbar(i) * ytet(i)
            ksi3 = ksi3 + xbar(i) * ztet(i)
10      continue
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   CAS D'UN ELEMENT REEL PYRAMIDE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    else if (nbsom.eq.5) then
!
! 2.1    AFFECTATION DES NUMEROS DES SOMMETS
! ---
        if (itetra .eq. 1) then
            isom(1) = 1
            isom(2) = 2
            isom(3) = 3
            isom(4) = 5
        else if (itetra.eq.2) then
            isom(1) = 3
            isom(2) = 4
            isom(3) = 1
            isom(4) = 5
        else
            isom(1) = 1
            isom(2) = 2
            isom(3) = 3
            isom(4) = 4
        endif
!
! 2.2    CALCUL DES COORDONNEES DE L'ANTECEDENT
! ---    DANS L'ELEMENT DE REFERENCE
!
        do 20 i = 1, 4
            j = isom(i)
            ksi1 = ksi1 + xbar(i) * xpyr(j)
            ksi2 = ksi2 + xbar(i) * ypyr(j)
            ksi3 = ksi3 + xbar(i) * zpyr(j)
20      continue
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   CAS D'UN ELEMENT REEL PENTAEDRE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    else if (nbsom.eq.6) then
!
! 3.1    AFFECTATION DES NUMEROS DES SOMMETS
! ---
        if (itetra .eq. 1) then
            isom(1) = 2
            isom(2) = 3
            isom(3) = 4
            isom(4) = 5
        else if (itetra.eq.2) then
            isom(1) = 3
            isom(2) = 4
            isom(3) = 5
            isom(4) = 6
        else if (itetra.eq.3) then
            isom(1) = 1
            isom(2) = 2
            isom(3) = 3
            isom(4) = 4
        else if (itetra.eq.4) then
            isom(1) = 4
            isom(2) = 1
            isom(3) = 2
            isom(4) = 5
        else if (itetra.eq.5) then
            isom(1) = 5
            isom(2) = 2
            isom(3) = 3
            isom(4) = 6
        else if (itetra.eq.6) then
            isom(1) = 3
            isom(2) = 1
            isom(3) = 4
            isom(4) = 6
        endif
!
! 3.2    CALCUL DES COORDONNEES DE L'ANTECEDENT
! ---    DANS L'ELEMENT DE REFERENCE
!
        do 30 i = 1, 4
            j = isom(i)
            ksi1 = ksi1 + xbar(i) * xpen(j)
            ksi2 = ksi2 + xbar(i) * ypen(j)
            ksi3 = ksi3 + xbar(i) * zpen(j)
30      continue
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   CAS D'UN ELEMENT REEL HEXAEDRE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    else
!
! 4.1    AFFECTATION DES NUMEROS DES SOMMETS
! ---
        if (itetra .eq. 1) then
            isom(1) = 6
            isom(2) = 3
            isom(3) = 8
            isom(4) = 1
        else if (itetra.eq.2) then
            isom(1) = 1
            isom(2) = 3
            isom(3) = 8
            isom(4) = 4
        else if (itetra.eq.3) then
            isom(1) = 6
            isom(2) = 8
            isom(3) = 1
            isom(4) = 5
        else if (itetra.eq.4) then
            isom(1) = 1
            isom(2) = 3
            isom(3) = 6
            isom(4) = 2
        else if (itetra.eq.5) then
            isom(1) = 6
            isom(2) = 8
            isom(3) = 3
            isom(4) = 7
        else if (itetra.eq.6) then
            isom(1) = 1
            isom(2) = 2
            isom(3) = 3
            isom(4) = 4
        else if (itetra.eq.7) then
            isom(1) = 3
            isom(2) = 4
            isom(3) = 8
            isom(4) = 7
        else if (itetra.eq.8) then
            isom(1) = 6
            isom(2) = 7
            isom(3) = 8
            isom(4) = 5
        else if (itetra.eq.9) then
            isom(1) = 6
            isom(2) = 5
            isom(3) = 1
            isom(4) = 2
        else if (itetra.eq.10) then
            isom(1) = 6
            isom(2) = 2
            isom(3) = 3
            isom(4) = 7
        else if (itetra.eq.11) then
            isom(1) = 1
            isom(2) = 5
            isom(3) = 8
            isom(4) = 4
        endif
!
! 4.2    CALCUL DES COORDONNEES DE L'ANTECEDENT
! ---    DANS L'ELEMENT DE REFERENCE
!
        do 40 i = 1, 4
            j = isom(i)
            ksi1 = ksi1 + xbar(i) * xhex(j)
            ksi2 = ksi2 + xbar(i) * yhex(j)
            ksi3 = ksi3 + xbar(i) * zhex(j)
40      continue
!
    endif
!
! --- FIN DE ANTE3D.
end subroutine
