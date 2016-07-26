subroutine xmmab4(ndim, nno, nnos, ffp, jac,&
                  ptknp, nfh, seuil, mu, singu,&
                  fk, coefbu, ddls, ddlm, mmat)
!
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/xcalc_saut.h"
    integer :: ndim, nno, nnos
    integer :: nfh, ddls, ddlm
    integer :: singu
    real(kind=8) :: mmat(216, 216), ptknp(3, 3)
    real(kind=8) :: ffp(27), jac
    real(kind=8) :: seuil, mu, coefbu
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
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DE B_U
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  NNOS   : NOMBRE DE NOEUDS SOMMET DE L'ELEMENT DE REF PARENT
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  PTKNP  : MATRICE PT.KN.P
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  SEUIL  : SEUIL
! IN  MU     : COEFFICIENT DE COULOMB
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  COEFBU :
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
!
!
    real(kind=8) :: coefj, coefi
    integer :: alpi, alpj
    integer :: i, j, k, l, jn, in
!
! ----------------------------------------------------------------------
!
    coefi=xcalc_saut(1,0,1)
    coefj=xcalc_saut(1,0,1)
!
    do 170 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
!
        do 171 j = 1, nno
            call indent(j, ddls, ddlm, nnos, jn)
!
            do 172 k = 1, nfh*ndim
                do 173 l = 1, nfh*ndim
!
                   mmat(in+ndim+k,jn+ndim+l) = mmat(in+ndim+k,jn+ ndim+l) - coefi*coefj*mu*seuil*&
                                               coefbu*ffp(i)*ffp(j)* ptknp(k,l)*jac
173              continue
!
                do 174 l = 1, singu*ndim
                  do alpj = 1, ndim
                    mmat(in+ndim+k,jn+ndim*(1+nfh)+alpj) = mmat(&
                                                        in+ndim+ k,&
                                                   jn+ndim*(1+nfh)+alpj) - coefi*2.d0*mu*seuil*coe&
                                                        &fbu* ffp(i)*fk(j,alpj,l)* ptknp(k,&
                                                        l&
                                                        )*jac
                   enddo
174              continue
172          continue
!
            do 175 k = 1, singu*ndim
              do alpi = 1, ndim
                do 176 l = 1, nfh*ndim
!
                    mmat(in+ndim*(1+nfh)+alpi,jn+ndim+l) = mmat(&
                                                        in+ndim*(1+nfh)+alpi,&
                                                   jn+ndim+l) - coefj*2.d0*mu*seuil*coefbu*&
                                                        fk(i,alpi,k)*ffp(j)* ptknp(k,&
                                                        l&
                                                        )*jac
!
176             continue
                do 177 l = 1, singu*ndim
                  do alpj = 1, ndim
                    mmat(in+ndim*(1+nfh)+alpi,jn+ndim*(1+nfh)+alpj) =&
                    mmat(in+ndim*(1+nfh)+alpi,jn+ndim*(1+nfh)+alpj) -&
                    4.d0*mu*seuil*coefbu*fk(i,alpi,k)*fk(j,alpj,l)* ptknp(k,&
                    l)*jac
                   enddo
177              continue
              enddo
175          continue
!
171      continue
170  end do
!
end subroutine
