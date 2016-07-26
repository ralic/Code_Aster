subroutine xmvco2(ndim, nno, nnol, nnos, lamb,&
                  am, delta, pla, lact, nfh,&
                  ddls, ddlm, nfiss, ifiss, jheafa,&
                  ifa, ncomph, jheavn, ncompn, jac, ffc,&
                  ffp, singu, r, fk, vtmp,&
                  p)
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/prmave.h"
#include "asterfort/transp.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
    integer :: ndim, nno, nnol, jheavn, ncompn
    integer :: nfh, ddls, pla(27), lact(8)
    integer :: singu
    real(kind=8) :: vtmp(400), delta(6)
    real(kind=8) :: ffp(27), jac
    real(kind=8) :: ffc(8)
    real(kind=8) :: fk(27,3,3)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: i, j, k, pli, nli, ddlm, ier, ifa, ifh, ifiss
    integer :: in, jheafa, ncomph, nfiss, nnos, hea_fa(2), alp
    real(kind=8) :: ffi, am(3), coefi, hfix(3), h(3), lamb(3), r
    real(kind=8) :: p(3, 3), ptr(3, 3)
    aster_logical :: lmultc
!
! ---------------------------------------------------------------------
!
! INITIALISATIONS
!
    lmultc = nfiss.gt.1
    call vecini(3, 0.d0, h)
    call vecini(3, 0.d0, hfix)
    call matini(3, 3, 0.d0, ptr)
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    endif
!
! CALCUL DE H = R*DELTA - FORCE COHESIVE AUGMENTEE EN BASE COVARIANTE
! RAPPEL : AM INVERSE PAR RAPPORT AUX CONVENTIONS X-FEM
!
    do 1 i = 1, ndim
        h(i) = - lamb(i) - r*am(i) + r*delta(i)
  1 end do
!
! CONVERSION DE H EN BASE FIXE : {HFIX} = [P]T {H}
! RAPPEL : P MATRICE DE PASSAGE BASE FIXE --> BASE COVARIANTE
!
    call transp(p, 3, ndim, ndim, ptr,&
                3)
    call prmave(0, ptr, 3, ndim, ndim,&
                h, ndim, hfix, ndim, ier)
!
! ON STOCKE DANS LE VECTEUR SECOND MEMBRE ELEMENTAIRE DE L EQUILIBRE
! ! IL Y A DEJA UN MOINS DU AUX CONVENTIONS POUR LE SAUT
!
    coefi = xcalc_saut(1,0,1)
    do 10 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 11 ifh = 1, nfh
            if (lmultc) then
                coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                                   zi(jheafa-1+ncomph*(ifiss-1)+2*ifa-1), &
                                   zi(jheafa-1+ncomph*(ifiss-1)+2*ifa),&
                                   zi(jheavn-1+ncompn*(i-1)+ncompn))
            else
                coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                                   hea_fa(1), &
                                   hea_fa(2),&
                                   zi(jheavn-1+ncompn*(i-1)+ncompn))
            endif
            do 12 j = 1, ndim
                vtmp(in+ndim*ifh+j) = vtmp(in+ndim*ifh+j) - coefi*ffp( i)*hfix(j)*jac
 12         continue
 11     continue
        do 13 alp = 1, singu*ndim
            do j = 1, ndim
                vtmp(in+ndim*(1+nfh)+alp) = vtmp( in+ndim*(1+nfh)+alp) - 2.d0&
                                             *fk(i,alp,j)*hfix(j)*jac
            enddo
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
        if (nli .eq. 0) goto 20
        do 21 k = 1, ndim
            vtmp(pli-1+k) = vtmp(pli-1+k) + (am(k)-delta(k))*ffi*jac
 21     continue
 20 continue
!
end subroutine
