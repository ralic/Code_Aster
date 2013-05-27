subroutine xmmab5(ndim, nnol, pla, ffc, jac,&
                  coeffr, seuil, tau1, tau2, mu,&
                  ik, lact, mmat)
!
    implicit none
    include 'asterfort/xmafr2.h'
    integer :: ndim, nnol
    integer :: pla(27), lact(8)
    real(kind=8) :: mmat(216, 216)
    real(kind=8) :: ffc(8), jac, tau1(3), tau2(3)
    real(kind=8) :: seuil, mu, coeffr, ik(3, 3)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
!
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DE F - CAS GLISSANT OU PENALISATION (NON SEULE)
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNOL   : NOMBRE DE NOEUDS PORTEURS DE DDLC
! IN  NNOF   : NOMBRE DE NOEUDS DE LA FACETTE DE CONTACT
! IN  PLA    : PLACE DES LAMBDAS DANS LA MATRICE
! IN  IPGF   : NUMÉRO DU POINTS DE GAUSS
! IN  IVFF   : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
! IN  FFC    : FONCTIONS DE FORME DE L'ELEMENT DE CONTACT
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  COEFFR :
! IN  NOEUD  : INDICATEUR FORMULATION (T=NOEUDS , F=ARETE)
! IN  SEUIL  : SEUIL
! IN  TAU1   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  TAU2   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  MU     : COEFFICIENT DE COULOMB
! IN  IK     : MATRICE I-KN
! IN  IFA    : INDICE DE LA FACETTE COURANTE
! IN  CFACE  : CONNECTIVITÉ DES NOEUDS DES FACETTES
! IN  LACT   : LISTE DES LAGRANGES ACTIFS
! IN  LPENAF : INDICATEUR DE PENALISATION DU FROTTEMENT
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
!
!
    integer :: i, j, k, l, nli, nlj
    integer :: pli, plj
    real(kind=8) :: ffi, ffj, taikta(2, 2)
!
! ----------------------------------------------------------------------
!
    do 180 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        nli=lact(i)
        if (nli .eq. 0) goto 180
        do 181 j = 1, nnol
            plj=pla(j)
            ffj=ffc(j)
            nlj=lact(j)
            if (nlj .eq. 0) goto 181
!
!         CALCUL DE TAIKTA = TAUT.(ID-KN).TAU
            call xmafr2(tau1, tau2, ik, taikta)
!
            do 182 k = 1, ndim-1
                do 183 l = 1, ndim-1
                    mmat(pli+k,plj+l) = mmat(pli+k,plj+l) + (mu*seuil/ coeffr)* ffi*ffj*taikta(k,&
                                        &l)*jac
183              continue
182          continue
181      continue
180  end do
!
end subroutine
