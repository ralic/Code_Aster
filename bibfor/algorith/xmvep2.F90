subroutine xmvep2(ndim, nno, nnos, nnol, pla,&
                  ffc, ffp, reac, jac, nfh,&
                  saut, singu, fk, nd, cpenco,&
                  ddls, ddlm, jheavn, ncompn, nfiss, ifiss,&
                  jheafa, ncomph, ifa, vtmp)
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
    integer :: ndim, nno, nnos, nnol
    integer :: pla(27), nfh
    integer :: singu, ddls, ddlm, nfiss, ifiss, jheafa, ncomph, ifa, jheavn, ncompn
    real(kind=8) :: vtmp(400), cpenco, saut(3), nd(3)
    real(kind=8) :: ffc(8), ffp(27), jac, reac
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
! --- CALCUL DU VECTEUR LN1 & LN2
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  NNOS   : NOMBRE DE NOEUDS SOMMET DE L'ELEMENT DE REF PARENT
! IN  NNOL   : NOMBRE DE NOEUDS PORTEURS DE DDLC
! IN  NNOF   : NOMBRE DE NOEUDS DE LA FACETTE DE CONTACT
! IN  PLA    : PLACE DES LAMBDAS DANS LA MATRICE
! IN  IPGF   : NUMÉRO DU POINTS DE GAUSS
! IN  IVFF   : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
! IN  FFC    : FONCTIONS DE FORME DE L'ELEMENT DE CONTACT
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  IDEPD  :
! IN  IDEPM  :
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NOEUD  : INDICATEUR FORMULATION (T=NOEUDS , F=ARETE)
! IN  TAU1   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  TAU2   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  IFA    : INDICE DE LA FACETTE COURANTE
! IN  CFACE  : CONNECTIVITÉ DES NOEUDS DES FACETTES
! IN  LACT   : LISTE DES LAGRANGES ACTIFS
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! IN  CSTAFR : COEFFICIENTS DE STABILISATION DU FROTTEMENT
! IN  CPENFR : COEFFICIENTS DE PENALISATION DU FROTTEMENT
! OUT VTMP   : VECTEUR SECOND MEMBRE DE CONTACT
!
!
!
    integer :: i, j, in, pli, ifh, hea_fa(2)
    integer :: alpi
    real(kind=8) :: ffi, dn, coefi
    aster_logical :: lmultc
!
! ----------------------------------------------------------------------
!
    coefi = xcalc_saut(1,0,1)
    lmultc = nfiss.gt.1
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    endif
    dn = 0.d0
    do 143 j = 1, ndim
        dn = dn + saut(j)*nd(j)
143 end do
!
! --- TERME LN1
!
    do 153 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 156 ifh = 1, nfh
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
            do 154 j = 1, ndim
                vtmp(in+ndim*ifh+j) = vtmp(in+ndim*ifh+j) + reac* coefi*ffp(i)*nd(j)*jac
154         continue
156     continue
        do 155 j = 1, singu*ndim
          do alpi = 1, ndim
            vtmp(in+ndim*(1+nfh)+alpi) = vtmp( in+ndim*(1+nfh)+alpi) + &
                                        reac*2.d0*fk(i,alpi,j)*nd(j )*jac
          enddo
155     continue
153 end do
!
! --- TERME LN2
!
    do 160 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        vtmp(pli) = vtmp(pli) - dn * ffi * jac
        vtmp(pli) = vtmp(pli) - reac*ffi*jac/cpenco
160 continue
end subroutine
