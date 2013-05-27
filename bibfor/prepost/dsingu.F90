subroutine dsingu(ndim, nelem, nnoem, nsommx, nelcom,&
                  degre, icnc, numeli, xy, erreur,&
                  energi, mesu, alpha, nalpha)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dalp2d.h'
    include 'asterfort/dalp3d.h'
    integer :: ndim, nelem, nnoem, nsommx, nelcom, degre
    integer :: icnc(nsommx+2, nelem), numeli(nelcom+2, nnoem)
    real(kind=8) :: xy(3, nnoem), erreur(nelem), energi(nelem), mesu(nelem)
    real(kind=8) :: alpha(nelem)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!         CALCUL DES DEGRES ELEMENTAIRE DE LA SINGULARITE
!         OPTION : 'SING_ELEM'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NDIM                   : DIMENSION DU PROBLEME
! IN   NELEM                  : NOMBRE D ELEMENTS FINIS
! IN   NNOEM                  : NOMBRE DE NOEUDS
! IN   NSOMMX                 : NOMBRE DE SOMMETS MAX PAR EF
! IN   NELCOM                 : NOMBRE MAX D'EF PAR NOEUD
! IN   DEGRE                  : DEGRE DES EF 1 EF P1 2 POUR EF P2
!      3 SI EF SURFACIQUES EN 2D OU VOLUMIQUES EN 3D
!      0 SINON
! IN   ICNC(NSOMMX+2,NELEM)   : CONNECTIVITE EF=>NOEUDS CONNECTES
!      1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
!      2EME VALEUR = 1 SI EF UTILE 0 SINON
!      CONNECTIVITE  EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
!      EN 2D EF UTILE = QUAD OU TRIA
!      EN 3D EF UTILE = TETRA OU HEXA
! IN   NUMELI(NELCOM+2,NNOEM) : CONNECTIVITE INVERSE NOEUD=>EF CONNECTES
!      1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N
!      2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
!                    1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
!                    2 NOEUD SOMMET BORD + LIE A UN EF UTILE
!      CONNECTIVITE  NOEUD N
! IN   XY(3,NNOEM)           : COORDONNEES DES NOEUDS
! IN   ERREUR(NELEM)         : ERREUR SUR CHAQUE EF
! IN   ENERGI(NELEM)         : ENERGIE SUR CHAQUE EF
! IN   PREC                  : % DE L ERREUR TOTALE SOUHAITE POUR
!                     CALCULER LA NOUVELLE CARTE DE TAILLE DES EF
! IN   MESU(NELEM)           : SURFACE OU VOLUME DE CHAQUE EF
!
!      SORTIE :
!-------------
! OUT  ALPHA(NELEM)          : DEGRE DE LA SINGULARITE PAR ELEMENT
! OUT  NALPHA                : NOMBRE DE CPE PAR ELEMENT DIFFERENTS
!                              1 PAR DEFAUT SI PAS DE SINGULARITE
!
! ......................................................................
!
!
!
!
!
    integer :: nalpha
!
! CALCUL DU DEGRE DE LA SINGULARITE ALPHA(NELEM)
!
    if (ndim .eq. 2) then
        call dalp2d(nelem, nnoem, degre, nsommx, icnc,&
                    nelcom, numeli, xy, erreur, energi,&
                    mesu, alpha, nalpha)
    else if (ndim.eq.3) then
        call dalp3d(nelem, nnoem, degre, nsommx, icnc,&
                    nelcom, numeli, xy, erreur, energi,&
                    mesu, alpha, nalpha)
    else
        call assert(.false.)
    endif
!
end subroutine
