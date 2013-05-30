subroutine xmmbp3(ndim, nno, nnos, nnol, pla,&
                  ffc, ffp, jac, knp, nfh,&
                  seuil, tau1, tau2, mu, singu,&
                  rr, lact, ddls, ddlm, mmat)
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/indent.h'
    include 'asterfort/matini.h'
    integer :: ndim, nno, nnos, nnol
    integer :: nfh, ddls, ddlm
    integer :: singu, pla(27), lact(8)
    real(kind=8) :: mmat(216, 216)
    real(kind=8) :: ffc(8), ffp(27), jac, tau1(3), tau2(3)
    real(kind=8) :: rr, seuil, knp(3, 3), mu
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
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DE B, BT
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
! IN  KNP    : PRODUIT KN.P
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NOEUD  : INDICATEUR FORMULATION (T=NOEUDS , F=ARETE)
! IN  SEUIL  : SEUIL
! IN  TAU1   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  TAU2   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  MU     : COEFFICIENT DE COULOMB
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  IFA    : INDICE DE LA FACETTE COURANTE
! IN  CFACE  : CONNECTIVITÉ DES NOEUDS DES FACETTES
! IN  LACT   : LISTE DES LAGRANGES ACTIFS
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! IN  LPENAF : INDICATEUR DE PENALISATION DU FROTTEMENT
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
    integer :: i, j, k, l, jn, nli, pli
    real(kind=8) :: ffi, tauknp(2, 3)
!
! ----------------------------------------------------------------------
!
!     INITIALISATION
    call matini(2, 3, 0.d0, tauknp)
!
!     II.3.1. CALCUL DE B ET DE BT
!
    do 160 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        nli=lact(i)
        if (nli .eq. 0) goto 160
!
!     CALCUL DE TAU.KN.P
        do 161 j = 1, ndim
            tauknp(1,j) = 0.d0
            do 162 k = 1, ndim
                tauknp(1,j) = tauknp(1,j) + tau1(k) * knp(k,j)
162          continue
161      end do
!
        if (ndim .eq. 3) then
            do 163 j = 1, ndim
                tauknp(2,j) = 0.d0
                do 164 k = 1, ndim
                    tauknp(2,j) = tauknp(2,j) + tau2(k) * knp(k,j)
164              continue
163          continue
        endif
!
        do 165 j = 1, nno
            call indent(j, ddls, ddlm, nnos, jn)
            do 166 k = 1, ndim-1
                do 167 l = 1, nfh*ndim
                    mmat(pli+k,jn+ndim+l) = mmat(pli+k,jn+ndim+l) + 2.d0*mu*seuil*ffi*ffp(j)*tauk&
                                            &np(k,l)*jac
!
167              continue
!
                do 168 l = 1, singu*ndim
!
                    mmat(pli+k,jn+ndim*(1+nfh)+l) = mmat(&
                                                    pli+k,&
                                                    jn+ ndim*(1+nfh)+l) + 2.d0*rr*mu*seuil*ffi*ff&
                                                    &p(j)* tauknp(k,&
                                                    l&
                                                    )*jac
!
168              continue
166          continue
165      end do
160  end do
!
end subroutine
