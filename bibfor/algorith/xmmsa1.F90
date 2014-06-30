subroutine xmmsa1(algofr, ndim, nno, nnos, nnol,&
                  pla, ffc, ffp, idepd, idepm,&
                  nfh, nd, tau1, tau2, singu,&
                  rr, lact, ddls, ddlm, coeffr,&
                  coeffp, p, adher, knp, ptknp,&
                  ik)
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/promat.h"
#include "asterfort/vecini.h"
#include "asterfort/xadher.h"
#include "asterfort/xmafr1.h"
    integer :: algofr, ndim, nno, nnos, nnol
    integer :: nfh, ddls, ddlm
    integer :: singu, pla(27), lact(8), idepd, idepm
    real(kind=8) :: rr, coeffr, coeffp, p(3, 3), ik(3, 3)
    real(kind=8) :: ffc(8), ffp(27), tau1(3), tau2(3), ptknp(3, 3)
    real(kind=8) :: knp(3, 3), nd(3)
    logical(kind=1) :: adher
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
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES INCREMENTS - DÉPLACEMENTS&
! --- SEMI-MULTIPLICATEUR DE FROTTEMENT
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
! IN  COEFFR  :
! IN  COEFFR : COEFFICIENTS DE STABILISATION DU FROTTEMENT
! IN  COEFFP : COEFFICIENTS DE PENALISATION DU FROTTEMENT
! IN  LPENAF : INDICATEUR DE PENALISATION DU FROTTEMENT
! IN  P      :
! OUT ADHER  :
! OUT KNP    : PRODUIT KN.P
! OUT PTKNP  : MATRICE PT.KN.P
! OUT IK     :
!
!
!
!
    integer :: i, j, nli, ino, in, pli
    real(kind=8) :: ffi, lamb1(3), r3(3), vitang(3), kn(3, 3), saut(3)
!
! ----------------------------------------------------------------------
!
! --- INITIALISATION
    call vecini(3, 0.d0, saut)
    call vecini(3, 0.d0, lamb1)
    call matini(3, 3, 0.d0, ptknp)
    call matini(3, 3, 0.d0, p)
    call matini(3, 3, 0.d0, knp)
    call matini(3, 3, 0.d0, kn)
!
    call xmafr1(ndim, nd, p)
!
    do 154 ino = 1, nno
        call indent(ino, ddls, ddlm, nnos, in)
!
        do 155 j = 1, nfh*ndim
            saut(j) = saut(j) - 2.d0 * ffp(ino) * zr(idepd-1+in+ndim+ j)
155      continue
        do 156 j = 1, singu*ndim
            saut(j) = saut(j) - 2.d0 * ffp(ino) * rr * zr(idepd-1+in+ ndim*(1+nfh)+j)
156      continue
154  end do
!
    do 158 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        nli=lact(i)
        if (nli .eq. 0) goto 158
!
        do 159 j = 1, ndim
            lamb1(j)=lamb1(j) + ffi * tau1(j) * (zr(idepd-1+pli+1)+zr(&
            idepm-1+pli+1))
!
            if (ndim .eq. 3) lamb1(j)=lamb1(j) + ffi * tau2(j) * (zr(idepd-1+pli+2) + zr(idepm-1+&
                             &pli+2))
159      continue
158  end do
!
!
! --- TEST DE L'ADHERENCE ET CALCUL DES MATRICES DE FROTTEMENT UTILES
!
    call xadher(p, saut, lamb1, coeffr, coeffp,&
                algofr, vitang, r3, kn, ptknp,&
                ik, adher)
!
    call promat(kn, 3, ndim, ndim, p,&
                3, ndim, ndim, knp)
!
end subroutine
