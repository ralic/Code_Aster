subroutine xmmsa4(ndim, nno, nnos, ffp, nddl,&
                  nvec, v1, v2, v3, nfh,&
                  singu, rr, ddls, ddlm, saut)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/indent.h"
#include "asterfort/vecini.h"
    integer :: ndim, nno, nnos
    integer :: nfh, ddls, ddlm
    integer :: singu, nvec, nddl
    real(kind=8) :: saut(3), rr, ffp(27)
    real(kind=8) :: v1(nddl), v2(*), v3(*)
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! CALCUL DU SAUT
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  NNOS   : NOMBRE DE NOEUDS SOMMET DE L'ELEMENT DE REF PARENT
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  NDDL   : NOMBRE TOTAL DE DDL DE L ELEMENT
! IN  NVEC   : NOMBRE VECTEURS DEPLACEMENT
! IN  VEC1   : PREMIER VECTEUR
! IN  VEC2   : DEUXIEME VECTEUR
! IN  VEC3   : TROISIEME VECTEUR
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! I/O SAUT   : SAUT
!
!
!
!
    integer :: i, j, in
!
! ----------------------------------------------------------------------
!
    call vecini(3, 0.d0, saut)
    ASSERT(nvec.gt.0)
    do 161 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 162 j = 1, nfh*ndim
            saut(j) = saut(j) - 2.d0*ffp(i)*v1(in+ndim+j)
            if (nvec .ge. 2) saut(j) = saut(j) - 2.d0*ffp(i)*v2(in+ndim+ j)
            if (nvec .ge. 3) saut(j) = saut(j) - 2.d0*ffp(i)*v3(in+ndim+ j)
162      continue
        do 163 j = 1, singu*ndim
            saut(j) = saut(j)-2.d0*ffp(i)*rr*v1(in+ndim*(1+nfh)+j)
            if (nvec .ge. 2) saut(j) = saut(j)-2.d0*ffp(i)*rr*v2(in+ndim* (1+nfh)+j)
            if (nvec .ge. 3) saut(j) = saut(j)-2.d0*ffp(i)*rr*v3(in+ndim* (1+nfh)+j)
163      continue
161  end do
!
end subroutine
