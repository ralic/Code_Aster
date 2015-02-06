subroutine xmvco5(ndim, nno, nnol, pla, nd,&
                  tau1, tau2, mu, ddls, jac,&
                  ffc, ffp, nnos, ddlm, wsaut,&
                  saut, vtmp)
!
!
    implicit none
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/prmave.h"
#include "asterfort/transp.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_saut.h"
    integer :: ndim, nno, nnol, ddlm
    integer :: ddls, pla(27)
    integer :: nnos
    real(kind=8) :: vtmp(400)
    real(kind=8) :: ffp(27), jac
!
    real(kind=8) :: mu(3), wsaut(3), saut(3)
    real(kind=8) :: ffc(8)
    real(kind=8) :: nd(3), tau1(3), tau2(3)
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
! --- CALCUL DES SECONDS MEMBRES DE COHESION, LOI CZM_LIN_MIX
! --- PARTIE INDEPENDANTE DE LA LOI D'INTERFACE
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  NNOL   : NOMBRE DE NOEUDS LAGRANGE
! IN  PLA    : PLACE DES INCONNUS DE CONTACT DANS LA NUMEROTATION
! IN  ND     : DIRECTION NORMALE
! IN  TAU1   : DIRECTION TANGENTE 1
! IN  TAU2   : DIRECTION TANGENTE 2
! IN  MU     : VECTEUR DE MEME NOM (FORCES D'INTERFACE MOYENNES)
! IN  DDLS   : NOMBRE DE DDLS DES NOEUDS SOMMET
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  FFC    : FONCTIONS DE FORME DE CONTACT
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  NNOS   : NOMBRE DE NOEUDS SOMMET 
! IN  DDLM   : NOMBRE DE DDLS DES NOEUDS MILIEU
! IN  SAUT   : SAUT DE DEPLACEMENT (BASE FIXE)
! IN  WSAUT  : SAUT DE DEPLACEMENT MOYEN W (BASE LOCAL)
! I/O VTMP   : VECTEUR ELEMENTAIRE DE COHESION
!
!
!
!
    integer :: i, j, pli, iin, ier
    real(kind=8) :: p(3, 3), ptr(3, 3), mug(3), am(3), coefi
!
! ---------------------------------------------------------------------
! on va commencer par construire une matrice de passage
    call matini(3, 3, 0.d0, p)
    call matini(3, 3, 0.d0, ptr)
    call vecini(3, 0.d0, mug)
    call vecini(3, 0.d0, am)
    coefi=xcalc_saut(1,0,1)
!
    do 17 i = 1, ndim
        p(1,i) = nd(i)
17  continue
    do 18 i = 1, ndim
        p(2,i) = tau1(i)
18  continue
    if (ndim .eq. 3) then
        do 19 i = 1, ndim
            p(3,i) = tau2(i)
19      continue
    endif
! calcul du saut de deplacement am en base locale
    call prmave(0, p, 3, ndim, ndim,&
                saut, ndim, am, ndim, ier)
! calcul de la contrainte MUG en base globale
    call transp(p, 3, ndim, ndim, ptr,&
                3)
    call prmave(0, ptr, 3, ndim, ndim,&
                mu, ndim, mug, ndim, ier)
! on reecrit tout
! on suppose qu on a acces à LAMB(3):
!    valeur de lambda au point de gauss
! et qu on a accès à WSAUT(3)
!    valeur de w au point de Gauss
! remplissage L1mu
    do 1 i = 1, nno
        call indent(i, ddls, ddlm, nnos, iin)
        do 2 j = 1, ndim
            vtmp(iin+ndim+j) = vtmp(iin+ndim+j)+ coefi*mug(j)*ffp(i)* jac
 2      continue
 1  end do
! remplissage L1u
    do 3 i = 1, nnol
        pli=pla(i)
        do 4 j = 1, ndim
            vtmp(pli+2*ndim-1+j) = vtmp(pli+2*ndim-1+j) - am(j)*ffc(i)*jac
 4      continue
 3  end do
! remplissage L1w
    do 5 i = 1, nnol
        pli=pla(i)
        do 6 j = 1, ndim
            vtmp(pli+2*ndim-1+j) = vtmp(pli+2*ndim-1+j) - wsaut(j)*ffc(i)*jac
 6      continue
 5  end do
! remplissage L2mu
    do 12 i = 1, nnol
        pli=pla(i)
        do 13 j = 1, ndim
            vtmp(pli-1+ndim+j) = vtmp(pli-1+ndim+j) - mu(j)*ffc(i)* jac
13      continue
12  end do
! le remplissage de L2w a été enlevé
!
end subroutine
