subroutine xmvef2(ndim, nno, nnos, ffp, jac,&
                  seuil, reac12, singu, nfh, rr,&
                  coeffp, coeffr, mu, algofr, nd,&
                  ddls, ddlm, idepl, pb, vtmp)
!
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/vecini.h"
#include "asterfort/xadher.h"
#include "asterfort/xmafr1.h"
    integer :: ndim, nno, nnos, ddls, ddlm, nfh, singu, idepl
    integer :: algofr
    real(kind=8) :: vtmp(400), rr, nd(3)
    real(kind=8) :: ffp(27), jac, pb(3), reac12(3)
    real(kind=8) :: coeffp, coeffr, seuil, mu
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
! --- CALCUL DU VECTEUR LN1
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
! IN  MALIN  : INDICATEUR FORMULATION (T=NOEUDS , F=ARETE)
! IN  TAU1   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  TAU2   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  IFA    : INDICE DE LA FACETTE COURANTE
! IN  CFACE  : CONNECTIVITÉ DES NOEUDS DES FACETTES
! IN  LACT   : LISTE DES LAGRANGES ACTIFS
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! IN  COEFFR : COEFFICIENT D AUGMENTATION DU FROTTEMENT
! IN  COEFFP : COEFFICIENT DE PENALISATION DU FROTTEMENT
! IN  ALGOFR : ALGO DE FROTTEMENT (1:LAG, 2:PENALISATION)
! IN  P      :
! OUT ADHER  :
! OUT KNP    : PRODUIT KN.P
! OUT PTKNP  : MATRICE PT.KN.P
! OUT IK     :
!
!
!
!
    integer :: i, j, k, in, ino
    real(kind=8) :: ptpb(3), p(3, 3), vitang(3), saut(3), rbid(3, 3)
    real(kind=8) :: r2bid(3, 3)
    real(kind=8) :: r3bid(3, 3)
    logical(kind=1) :: adher
!
! ----------------------------------------------------------------------
!
!     P : OPÉRATEUR DE PROJECTION
    call xmafr1(ndim, nd, p)
!
!     PBOUL SELON L'ÉTAT D'ADHERENCE DU PG (AVEC DEPDEL)
    call vecini(3, 0.d0, saut)
    do 175 ino = 1, nno
        call indent(ino, ddls, ddlm, nnos, in)
        do 176 j = 1, nfh*ndim
            saut(j) = saut(j) - 2.d0 * ffp(ino) * zr(idepl-1+in+ndim+ j)
176      continue
        do 177 j = 1, singu*ndim
            saut(j) = saut(j) - 2.d0 * ffp(ino) * rr * zr(idepl-1+in+ ndim*(1+nfh)+j)
!
177      continue
175  end do
!
    call xadher(p, saut, reac12, coeffr, coeffp,&
                algofr, vitang, pb, rbid, r2bid,&
                r3bid, adher)
!
    if (adher) then
!               CALCUL DE PT.REAC12
        do 188 i = 1, ndim
            ptpb(i)=0.d0
            if (algofr .eq. 2) then
                do 190 k = 1, ndim
                    ptpb(i)=ptpb(i)+p(k,i)*coeffp*vitang(k)
190              continue
            else
                do 189 k = 1, ndim
                    ptpb(i)=ptpb(i)+p(k,i)*(reac12(k)+coeffr*vitang(k)&
                    )
189              continue
            endif
188      continue
    else
!     CALCUL DE PT.PBOUL
        do 182 i = 1, ndim
            ptpb(i)=0.d0
            do 183 k = 1, ndim
                ptpb(i)=ptpb(i) + p(k,i)*pb(k)
183          continue
182      continue
    endif
!
    do 185 i = 1, nno
        call indent(i, ddls, ddlm, nnos, in)
!
        do 186 j = 1, nfh*ndim
            vtmp(in+ndim+j) = vtmp(in+ndim+j) + 2.d0*mu*seuil* ptpb(j) *ffp(i)*jac
186      continue
        do 187 j = 1, singu*ndim
            vtmp(in+ndim*(1+nfh)+j) = vtmp(in+ndim*(1+nfh)+j) + 2.d0*rr*mu*seuil* ptpb(j)*ffp(i&
                                      )*jac
187      continue
185  end do
!
end subroutine
