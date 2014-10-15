subroutine xmvco4(ino, ndim, nnol, sigma, lamb, pla,&
                  lact, jac, ffc, p, raug, vtmp)
!
!
    implicit none
#include "asterfort/matini.h"
#include "asterfort/prmave.h"
#include "asterfort/transp.h"
#include "asterfort/vecini.h"
    integer :: ino
    integer :: ndim, nnol
    integer :: pla(27), lact(8)
    real(kind=8) :: vtmp(400), sigma(6)
    real(kind=8) :: jac, lamb(3), raug
    real(kind=8) :: ffc(8), p(3, 3)
!
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
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES SECONDS MEMBRES DE COHESION
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  SIGMA  : VECTEUR CONTRAINTE EN REPERE LOCAL
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! I/O VTMP   : VECTEUR ELEMENTAIRE DE CONTACT/FROTTEMENT
!
!
!
!
    integer :: k, pli, ier
    real(kind=8) :: ptr(3, 3), sigglo(3)
!
! ---------------------------------------------------------------------
!
! DIRECTION DU SAUT DE DEPLACEMENT TANGENT
!
! on reecrit tout
! on suppose qu on a acces à LAMB(3):
!    valeur de lambda au point de gauss
! et qu on a accès à WSAUT(3)
!    valeur de w au point de Gauss
! remplissage L1l
    call matini(3, 3, 0.d0, ptr)
    call vecini(3, 0.d0, sigglo)
! remplissage L2w
! on transpose la matrice
    call transp(p, 3, ndim, ndim, ptr,&
                3)
! produit matrice vecteur
    call prmave(0, ptr, 3, ndim, ndim,&
                sigma, ndim, sigglo, ndim, ier)
! attention, cette fois on ne doit pas virer
! la boucle sur les noeuds,
! mais on remplit tout avec la contribution de J
! vecteur w*
    pli=pla(ino)
    do 11 k = 1, ndim
        vtmp(pli-1+ndim+k) = vtmp(pli-1+ndim+k) + sigma(k)* ffc(ino)*jac
11  continue
! vecteur lambda*
! attention a ne pas oublier le terme lambda/r
    pli=pla(ino)
    do 12 k = 1, ndim
        vtmp(pli-1+k) = vtmp(pli-1+k) + (sigma(k)-lamb(k))* ffc(ino)*jac/raug
12  continue
!
end subroutine
