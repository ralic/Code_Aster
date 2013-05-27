subroutine xmmaa4(nnol, pla, ffc, jac, cstaco,&
                  mmat)
!
    implicit none
    include 'jeveux.h'
    integer :: nnol, pla(27)
    real(kind=8) :: mmat(216, 216)
    real(kind=8) :: ffc(8), jac, cstaco
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
!
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DE LA MATRICE C - CAS SANS CONTACT
!
! ----------------------------------------------------------------------
!
! IN  NNOL   : NOMBRE DE NOEUDS PORTEURS DE DDLC
! IN  NNOF   : NOMBRE DE NOEUDS DE LA FACETTE DE CONTACT
! IN  PLA    : PLACE DES LAMBDAS DANS LA MATRICE
! IN  IPGF   : NUMÉRO DU POINTS DE GAUSS
! IN  IVFF   : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
! IN  FFC    : FONCTIONS DE FORME DE L'ELEMENT DE CONTACT
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  NOEUD  : INDICATEUR FORMULATION (T=NOEUDS , F=ARETE)
! IN  CSTACO : COEFFICIENTS DE STABILISATION OU PENALISATION DU CONTACT
! I/O  MMAT  : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
    integer :: i, j, pli, plj
    real(kind=8) :: ffi, ffj
!
! ----------------------------------------------------------------------
!
    do 120 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
!
        do 121 j = 1, nnol
            plj=pla(j)
            ffj=ffc(j)
!
            mmat(pli,plj) = mmat(pli,plj) - ffj * ffi * jac / cstaco
121      continue
120  end do
!
end subroutine
