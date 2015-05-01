subroutine pmfdge(b, g, depl, alpha, dege)
    implicit none
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
! -----------------------------------------------------------
! ---  CALCUL DES DEFORMATIONS GENERALISEES
!         DE L'ELEMENT POUTRE EULER (A LA POSITION X)
! --- IN : MATRICE B POUR LA POSITION CONSIDEREE
! --- IN : MATRICE G POUR LA POSITION CONSIDEREE (MODE INCOMPATIBLE)
! --- IN : DEPLACEMENTS DANS LE REPERE LOCAL (6 DDL PAR NOEUD)
!          DEPL(12)
! --- IN : ALPHA VARIABLE MODE INCOMPATIBLE
! --- OUT : DEFORMATIONS GENERALISEES A LA POSITION OU B EST CALCULEE
!          DEGE(6)
!           1 : DEFORMATION AXIALE
!           2 ET 3 : DISTORSION TRANCHANTE NULLE POUR EULER BERNOULLI
!           4 : ANGLE UNITAIRE DE TORSION
!           5 : COURBURE AUTOUR DE Y
!           6 : COURBURE AUTOUR DE Z
! -----------------------------------------------------------
    real(kind=8) :: b(4), g, depl(12), alpha, dege(6)
    real(kind=8) :: zero
    parameter (zero=0.0d+0)
!
! --- DEF. GENERALISEES DE L'ELEMENT POUTRE EULER A LA POSITION DE B
    dege(1)=(depl(7)-depl(1))*b(1)+g*alpha
    dege(2)=zero
    dege(3)=zero
    dege(4)=(depl(10)-depl(4))*b(1)
    dege(5)=b(4)*depl(11)+b(3)*depl(5)+b(2)*(depl(9)-depl(3))
    dege(6)=b(4)*depl(12)+b(3)*depl(6)+b(2)*(depl(2)-depl(8))
!
end subroutine
