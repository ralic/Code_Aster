subroutine xmvep2(ndim, nno, nnos, nnol, pla,&
                  ffc, ffp, reac, jac, nfh,&
                  saut, singu, nd, rr, cpenco,&
                  ddls, ddlm, jfisno, nfiss, ifiss,&
                  jheafa, ncomph, ifa, vtmp)
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
    integer :: ndim, nno, nnos, nnol
    integer :: pla(27), nfh
    integer :: singu, ddls, ddlm, jfisno, nfiss, ifiss, jheafa, ncomph, ifa
    real(kind=8) :: vtmp(400), cpenco, saut(3), nd(3)
    real(kind=8) :: ffc(8), ffp(27), jac, reac, rr
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
! IN  RR     : DISTANCE AU FOND DE FISSURE
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
    integer :: i, j, in, pli, ifh, coefi
    real(kind=8) :: ffi, dn
    logical :: lmultc
!
! ----------------------------------------------------------------------
!
    coefi = 2
    lmultc = nfiss.gt.1
    dn = 0.d0
    do 143 j = 1, ndim
        dn = dn + saut(j)*nd(j)
143  end do
!
! --- TERME LN1
!
    do 153 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 156 ifh = 1, nfh
            if (lmultc) then
                coefi = zi(&
                        jheafa-1+ncomph*(&
                        nfiss*(ifiss-1) +zi( jfisno-1+nfh*(i-1)+ifh)-1)+2*ifa) - zi(jheafa-1+ nco&
                        &mph*(nfiss*(ifiss-1) +zi(jfisno-1+nfh*(i-1)+ifh)-1&
                        ) +2*ifa-1&
                        )
            endif
            do 154 j = 1, ndim
                vtmp(in+ndim*ifh+j) = vtmp(in+ndim*ifh+j) + reac* coefi*ffp(i)*nd(j)*jac
154          continue
156      continue
        do 155 j = 1, singu*ndim
            vtmp(in+ndim*(1+nfh)+j) = vtmp( in+ndim*(1+nfh)+j) + reac*coefi*ffp(i)*rr*nd(j )*jac
155      continue
153  end do
!
! --- TERME LN2
!
    do 160 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        vtmp(pli) = vtmp(pli) - dn * ffi * jac
        vtmp(pli) = vtmp(pli) - reac*ffi*jac/cpenco
160  continue
end subroutine
