subroutine xmmaa3(ndim, nno, nnos, nnol, pla,&
                  ffc, ffp, jac, nfh, nd,&
                  cstaco, singu, rr, ddls, ddlm,&
                  jheavn, ncompn, nfiss, ifiss, jheafa, ncomph,&
                  ifa, mmat)
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_saut.h"
    integer :: ndim, nno, nnos, nnol
    integer :: nfh, ddls, ddlm
    integer :: singu, pla(27), nfiss, ifiss, jheafa, ncomph, ifa, jheavn, ncompn
    real(kind=8) :: mmat(216, 216), nd(3)
    real(kind=8) :: ffc(8), ffp(27), jac
    real(kind=8) :: cstaco, rr
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES MATRICES A, AT, AU - CAS DU CONTACT
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
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NOEUD  : INDICATEUR FORMULATION (T=NOEUDS , F=ARETE)
! IN  ND     : NORMALE À LA FACETTE ORIENTÉE DE ESCL -> MAIT
!                 AU POINT DE GAUSS
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
!
    integer :: i, j, k, l, jn, in, ifh, jfh
    integer :: pli, hea_fa(2)
    real(kind=8) :: ffi, coefi, coefj
    aster_logical :: lmultc
!
! ----------------------------------------------------------------------
!
    coefi = xcalc_saut(1,0,1)
    coefj = xcalc_saut(1,0,1)
    lmultc = nfiss.gt.1
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    endif
! I.1 CALCUL DE A
    do 130 i = 1, nnol
!
        pli=pla(i)
        ffi=ffc(i)
!
        do 131 j = 1, nno
            call indent(j, ddls, ddlm, nnos, jn)
            do 134 jfh = 1, nfh
                if (lmultc) then
                    coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+jfh),&
                                       zi(jheafa-1+ncomph*(ifiss-1)+2*ifa-1), &
                                       zi(jheafa-1+ncomph*(ifiss-1)+2*ifa),&
                                       zi(jheavn-1+ncompn*(j-1)+ncompn))
                else
                    coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+jfh),&
                                       hea_fa(1), &
                                       hea_fa(2),&
                                       zi(jheavn-1+ncompn*(j-1)+ncompn))
                endif
                do 132 l = 1, ndim
                    mmat(pli,jn+ndim*jfh+l) = mmat(pli,jn+ndim*jfh+l) + coefj * ffi * ffp(j) * nd&
                                              &(l) * jac
!
! TERME SYMETRIQUE
!
                    mmat(jn+ndim*jfh+l,pli) = mmat(jn+ndim*jfh+l,pli) + coefj * ffi * ffp(j) * nd&
                                              &(l) * jac
132             continue
!
134         continue
            do 133 l = 1, singu*ndim
                mmat(pli,jn+ndim*(1+nfh)+l) = mmat(&
                                              pli,&
                                              jn+ndim*(1+nfh) +l) + coefj * ffi * ffp(j) * rr * n&
                                              &d(l&
                                              ) * jac
!
                mmat(jn+ndim*(1+nfh)+l,pli)= mmat(jn+ndim*(1+nfh)+l,&
                pli) + coefj * ffi * ffp(j) * rr * nd(l) * jac
133         continue
!
131     continue
!
130 end do
!
!     I.2. CALCUL DE A_U
!
    do 140 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
        do 141 j = 1, nno
            call indent(j, ddls, ddlm, nnos, jn)
            do 148 ifh = 1, nfh
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
                do 149 jfh = 1, nfh
                    if (lmultc) then
                        coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+jfh),&
                                           zi(jheafa-1+ncomph*(ifiss-1)+2*ifa-1), &
                                           zi(jheafa-1+ncomph*(ifiss-1)+2*ifa),&
                                           zi(jheavn-1+ncompn*(j-1)+ncompn))
                    else
                        coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+jfh),&
                                           hea_fa(1), &
                                           hea_fa(2),&
                                           zi(jheavn-1+ncompn*(j-1)+ncompn))
                    endif
                    do 142 k = 1, ndim
                        do 143 l = 1, ndim
                            mmat(in+ndim*ifh+k,jn+ndim*jfh+l) =&
                            mmat(in+ndim*ifh+k,jn+ndim*jfh+l) +&
                            coefi*coefj*cstaco*ffp(i)*ffp(j)*nd(k)*nd(&
                            l)*jac
143                     continue
!
                        do 144 l = 1, singu*ndim
                            mmat(in+ndim+k,jn+ndim*(1+nfh)+l) =&
                            mmat(in+ndim+k,jn+ndim*(1+nfh)+l) +&
                            coefi*2.d0*cstaco*ffp(i)*ffp(j)*rr*nd(k)*nd(l)*&
                            jac
144                     continue
142                 continue
149             continue
148         continue
!
            do 145 k = 1, singu*ndim
                do 146 l = 1, nfh*ndim
                    mmat(in+ndim*(1+nfh)+k,jn+ndim+l) = mmat(&
                                                        in+ndim*(1+nfh)+k,&
                                                   jn+ndim+l) + coefj*2.d0*cstaco*ffp(i)*ffp(j)* r&
                                                        &r*nd(k)*nd(l&
                                                        )*jac
146             continue
!
                do 147 l = 1, singu*ndim
                    mmat(in+ndim*(1+nfh)+k,jn+ndim*(1+nfh)+l) =&
                    mmat(in+ndim*(1+nfh)+k,jn+ndim*(1+nfh)+l) +&
                    4.d0*cstaco*ffp(i)*ffp(j)*rr*rr*nd(k)*nd(l)&
                    *jac
147             continue
145         continue
!
141     continue
140 continue
!
end subroutine
