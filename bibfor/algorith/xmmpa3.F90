subroutine xmmpa3(ndim, nno, nnos, nnol, pla,&
                  ffc, ffp, jac, nfh, nd,&
                  cpenco, singu, rr, ddls, ddlm,&
                  jfisno, nfiss, ifiss, jheafa, ncomph,&
                  ifa, mmat)
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
    integer :: ndim, nno, nnos, nnol
    integer :: nfh, ddls, ddlm
    integer :: singu, pla(27), jfisno, nfiss, ifiss, jheafa, ncomph, ifa
    real(kind=8) :: mmat(216, 216), nd(3)
    real(kind=8) :: ffc(8), ffp(27), jac
    real(kind=8) :: cpenco, rr
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
! IN  CPENCO : COEFFICIENT DE PENALISATION DU CONTACT
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
    integer :: i, j, l, jn, jfh, coefj
    integer :: pli, plj
    real(kind=8) :: ffi, ffj
    logical :: lmultc
!
! ----------------------------------------------------------------------
!
    coefj = 2
    lmultc = nfiss.gt.1
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
                    coefj = zi(&
                            jheafa-1+ncomph*(&
                            nfiss*(ifiss-1) +zi(jfisno-1+nfh*(j-1)+jfh)-1)+2*ifa) - zi(jheafa- 1+&
                            &ncomph*(nfiss*(ifiss-1) +zi(jfisno-1+nfh*(j-1)+ jfh)-1&
                            )+2*ifa-1&
                            )
                endif
                do 132 l = 1, ndim
                    mmat(pli,jn+ndim*jfh+l) = mmat(pli,jn+ndim*jfh+l) + coefj * ffi * ffp(j) * nd&
                                              &(l) * jac
!
! LBB : ON PREND AUSSI LE TERME EN PENALISATION
!
                    mmat(jn+ndim*jfh+l,pli) = mmat(jn+ndim*jfh+l,pli) + coefj * ffi * ffp(j) * nd&
                                              &(l) * jac
132              continue
!
134          continue
            do 133 l = 1, singu*ndim
                mmat(pli,jn+ndim*(1+nfh)+l) = mmat(&
                                              pli,&
                                              jn+ndim*(1+nfh) +l) + coefj * ffi * ffp(j) * rr * n&
                                              &d(l&
                                              ) * jac
!
                mmat(jn+ndim*(1+nfh)+l,pli)= mmat(jn+ndim*(1+nfh)+l,&
                pli) + coefj * ffi * ffp(j) * rr * nd(l) * jac
133          continue
!
131      continue
!
130  end do
!
!     CALCUL DE C
!
    do 220 i = 1, nnol
!
        pli=pla(i)
        ffi=ffc(i)
!
        do 221 j = 1, nnol
!
            plj=pla(j)
            ffj=ffc(j)
!
            mmat(pli,plj) = mmat(pli,plj) - ffj * ffi * jac / cpenco
!
221      continue
220  end do
!
end subroutine
