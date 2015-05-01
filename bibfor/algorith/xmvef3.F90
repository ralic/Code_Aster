subroutine xmvef3(ndim, nnol, pla, ffc, reac12,&
                  pb, jac, seuil, tau1, tau2,&
                  lact, cstafr, mu, vtmp)
!
    implicit none
#include "blas/ddot.h"
    integer :: ndim, nnol, pla(27)
    integer :: lact(8)
    real(kind=8) :: vtmp(400), tau1(3), tau2(3)
    real(kind=8) :: ffc(8), jac, pb(3), reac12(3)
    real(kind=8) :: cstafr, seuil, mu
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
! --- CALCUL DU VECTEUR LN3
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
! IN  RHOTK  :
! IN  CSTAFR : COEF AUGMENTATION OU PENALISATION
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
!
    integer :: i, k
    integer :: pli, nli
    real(kind=8) :: ffi, metr(2), rpb(3)
!
! ----------------------------------------------------------------------
!
! --- CALCUL DE REAC12-PBOUL
    do 180 i = 1, ndim
        rpb(i)=reac12(i)-pb(i)
180  end do
!
    do 194 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        nli=lact(i)
        if (nli .eq. 0) goto 194
!
        metr(1)=ddot(ndim,tau1(1),1,rpb,1)
        if (ndim .eq. 3) metr(2)=ddot(ndim,tau2(1),1,rpb,1)
        do 195 k = 1, ndim-1
            vtmp(pli+k) = vtmp(pli+k) + mu*seuil/cstafr * metr(k)*ffi* jac
195      continue
194  continue
!
end subroutine
