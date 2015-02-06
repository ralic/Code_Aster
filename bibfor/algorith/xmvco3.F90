subroutine xmvco3(sigref, depref, ndim, nno, nnol,&
                  nnos, pla, lact, nfh, ddls,&
                  ddlm, nfiss, ifiss, jheafa, ifa,&
                  ncomph, jheavn, ncompn, jac, ffc, ffp,&
                  singu, rr, vtmp)
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/xcalc_saut.h"
    integer :: ndim, nno, nnol
    integer :: nfh, ddls, pla(27), lact(8)
    integer :: singu, jheavn, ncompn
    real(kind=8) :: vtmp(400)
    real(kind=8) :: ffp(27), jac, depref, sigref
    real(kind=8) :: rr, ffc(8)
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
    integer :: i, j, k, pli, nli, ddlm, ifa, ifh, ifiss
    integer :: in, jheafa, ncomph, nfiss, nnos
    real(kind=8) :: ffi, coefi
    aster_logical :: lmultc
!
! ---------------------------------------------------------------------
!
    coefi = xcalc_saut(1,0,1)
    lmultc = nfiss.gt.1
    do 10 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 11 ifh = 1, nfh
            if (lmultc) then
                coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                                       zi(jheafa-1+ncomph*(ifiss-1)+2*ifa-1), &
                                       zi(jheafa-1+ncomph*(ifiss-1)+2*ifa))
            endif
            do 12 j = 1, ndim
                vtmp(in+ndim*ifh+j) = vtmp(in+ndim*ifh+j) + abs(coefi* ffp(i)*sigref*jac)
 12         continue
 11     continue
        do 13 j = 1, singu*ndim
            vtmp(in+ndim*(1+nfh)+j) = vtmp( in+ndim*(1+nfh)+j) + abs(2.d0*ffp(i)*rr*sigref*jac )
 13     continue
 10 continue
!
! SECOND MEMBRE DE L EQUATION D INTERFACE: EXPRESSION DIRECTE
! ATTENTION INVERSION DE CONVENTIONS
!
    do 20 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        nli=lact(i)
        do 21 k = 1, ndim
! SI LAGRANGE ACTIF ON MET LA FORCE NODALE DE REF
            if (nli .ne. 0) then
                vtmp(pli-1+k) = vtmp(pli-1+k) + abs(depref*ffi*jac)
! SINON ON MET SIGREF
            else if (nli.eq.0) then
                vtmp(pli-1+k) = vtmp(pli-1+k) + abs(sigref)
            endif
 21     continue
 20 continue
!
end subroutine
