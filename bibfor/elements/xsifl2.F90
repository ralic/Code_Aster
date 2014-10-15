subroutine xsifl2(basloc, coeff, coeff3, ddld, ddlm,&
                  ddls, dfdi, ff, idepl, igthet,&
                  ithet, jac, mult, ndim, nnop,&
                  nnos, tau1, tau2, nd, xg)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/assert.h"
#include "blas/ddot.h"
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/normev.h"
#include "asterfort/prmave.h"
#include "asterfort/provec.h"
#include "asterfort/transp.h"
#include "asterfort/vecini.h"
#include "jeveux.h"
!
! Calcul de facteurs d'intensité des contraintes équivalents
!   avec XFEM + éléments cohésifs (option CALC_K_G_COHE)
!
! In basloc => base covariante liée au front,
!              aux noeuds du maillage parent
! In coeff, coeff3 => coefficients pour application de la
!                     formule d'Irwin
! In ddld => nombre de ddl de déplacement par noeud
! In ddlm => nombre de ddl par noeud milieu
! In ddls => nombre de ddl par noeud sommet
! In dfdi => dérivées des fonctions de forme
! In ff   => fonctions de forme
! In idepl => champ de déplacement
! Out igthet => champ resultat (KI, KII etc...)
! In  ithet => champ theta (extension virtuelle de fissure)
! In  jac => produit du jacobien et du poids
! In ndim => dimension
! In nnop => nombre de noeuds
! In nnos => nombre de noeuds sommet
! In nd => normale à la facette
! In tau1 => tangente 1
! In xg => coordonnées du point de Gauss

!
!
    integer :: ndim, nnop
    real(kind=8) :: am(3), basloc(9*nnop), coeff, coeff3
    integer :: ddld, ddlm, ddls
    real(kind=8) :: dfdi(nnop, ndim)
    real(kind=8) :: e1(3), e2(3), e3(3), ff(27), g, g1, g2, g3
    real(kind=8) :: grde1, grde2, grde3, grdep(3, 3)
    real(kind=8) :: gs2, gs3, cmp_hp
    integer :: i, idepl, ier, igthet, ii, ino, ithet
    integer :: j, l
    real(kind=8) :: jac, jm, k1, k2, k3, lamb(3), lamb1, lamb2
    real(kind=8) :: lamb3, lambl(3), mult
    integer :: nnos
    real(kind=8) :: norme, pm(3, 3), ptr(3, 3), theta(3)
    real(kind=8) :: tau1(3), tau2(3), nd(3), temp(3), xg(3)
    real(kind=8) :: ptp(3), vec(3), sens
!     BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
!     DIMENSIONNEMENT A 3 ET NON NDIM POUR POUVOIR UTILISER NORMEV.F
    call matini(3, 3, 0.d0, pm)
    do 117 i = 1, ndim
        pm(1,i) = nd(i)
117  continue
    do 118 i = 1, ndim
        pm(2,i) = tau1(i)
118  continue
    if (ndim .eq. 3) then
        do 119 i = 1, ndim
            pm(3,i) = tau2(i)
119      continue
    endif
    call transp(pm, 3, ndim, ndim, ptr,&
                3)
    call vecini(3, 0.d0, e1)
    call vecini(3, 0.d0, e2)
    call vecini(3, 0.d0, ptp)
    call vecini(3, 0.d0, vec)
    do 132 ino = 1, nnop
        do 110 i = 1, ndim
            ptp(i) = ptp(i) + basloc(3*ndim*(ino-1)+i) * ff(ino)
            e2(i) = e2(i) + basloc(3*ndim*(ino-1)+i+ndim) * ff(ino)
            e1(i) = e1(i) + basloc(3*ndim*(ino-1)+i+2*ndim) * ff(ino)
110      continue
132  end do
!
! E1 == NORMALE (N)
! E2 == TANGENTE DIRECTION DE FISSURATION (M)
!
!     -----------------------------------
!     2) CALCUL DE THETA ET DE DIV(THETA)
!     -----------------------------------
    call vecini(3, 0.d0, theta)
!
    do 390 i = 1, ndim
        do 301 ino = 1, nnop
            theta(i) = theta(i) + ff(ino) * zr(ithet-1+ndim*(ino-1)+i)
301      continue
!
!       ON REMPLACE E2 PAR THETA: REDRESSEMENT EN BORD DE FISSURE
        e2(i) = theta(i)
390  end do
!
!    ON REORTHOGONALISE THETA
!
    call provec(e1, theta, temp)
    call normev(temp, norme)
    call provec(temp, e1, theta)
    call normev(theta, norme)
!
!   NORMALISATION DE LA BASE
    call normev(e1, norme)
    call normev(e2, norme)
    call provec(e1, e2, e3)
    call normev(e3, norme)
!
!   OPTION 1 : ON REORTHOGONALISE EN CORRIGEANT N (DECOMMENTER)
!    call provec(e2, e3, e1)
!    call normev(e1, norme)
!
!   OPTION 2 : ON REORTHOGONALISE EN CORRIGEANT M (DECOMMENTER)
!    call provec(e3, e1, e2)
!    call normev(e2, norme)
!
!   OPTION 3 : DIRECTION TANGENTE = DIRECTION AU FOND
    do i=1,ndim
        vec(i) = xg(i) - ptp(i)
    end do
!   Seule modification par rapport à avant :
!   on va prendre la projection dans le plan
    cmp_hp = ddot(3,vec,1,e3,1)
    do i=1,ndim
        vec(i) = vec(i) - cmp_hp*e3(i)
    end do
    call normev(vec,norme)
    sens = ddot(3,vec,1,e2,1)
    if(norme.ne.0.d0) then
        if(sens.lt.0.d0) then
            do i=1,ndim
               e2(i) = -vec(i)
            end do
        else
            do i=1,ndim
               e2(i) = vec(i)
            end do
        endif
    endif
!
!   ON ORTHOGONALISE
    call provec(e2, e3, e1)
    call normev(e1, norme)
!
! AJOUT CALCUL DE LA CONTRAINTE COHÉSIVE
!
    call vecini(3, 0.d0, lambl)
    do 270 ino = 1, nnop
        call indent(ino, ddls, ddlm, nnos, ii)
        do 280 j = 1, ndim
            lambl(j) = lambl(j) + zr(idepl-1+ii+ddld+j)*ff(ino)
280      continue
270  end do
    call prmave(0, ptr, 3, ndim, ndim,&
                lambl, ndim, lamb, ndim, ier)
!
!     ---------------------------------------------
!     3) CALCUL DU DEPLACEMENT
!     ---------------------------------------------
    call matini(ndim, ndim, 0.d0, grdep)
    do 290 ino = 1, nnop
        call vecini(ndim, 0.d0, am)
        call indent(ino, ddls, ddlm, nnos, ii)
        do 291 j = 1, ndim
            do 292 l = 1, ndim
                am(j) = am(j) + ptr(j,l)*zr(idepl-1+ddld+ndim+ii+l)
292          continue
291      continue
!
        do 281 j = 1, ndim
            do 282 l = 1, ndim
                grdep(j,l) = grdep(j,l) + dfdi(ino,l)*am(j)
282          continue
281      continue
290  end do
!
!
!     -----------------------------------
!     4) CALCUL EFFECTIF DE G, K1, K2, K3
!     -----------------------------------
    g = 0.d0
    k1 = 0.d0
    k2 = 0.d0
    k3 = 0.d0
    g1 = 0.d0
    g2 = 0.d0
    g3 = 0.d0
    grde1 = 0.d0
    grde2 = 0.d0
    grde3 = 0.d0
    do 530 j = 1, ndim
        do 540 l = 1, ndim
            grde1 = grde1 + e1(j)*grdep(j,l)*theta(l)
            grde2 = grde2 + e2(j)*grdep(j,l)*theta(l)
            grde3 = grde3 + e3(j)*grdep(j,l)*theta(l)
            g = g - lamb(j)*grdep(j,l)*theta(l)
540      continue
530  end do
    lamb1 = ddot(3,lamb,1,e1,1)
    lamb2 = ddot(3,lamb,1,e2,1)
    lamb3 = ddot(3,lamb,1,e3,1)
    g1 = -lamb1*grde1
    g2 = -lamb2*grde2
    g3 = -lamb3*grde3
    gs2 = (lamb2*abs(grde2)-grde2*abs(lamb2))/2.d0
    gs3 = (lamb3*abs(grde3)-grde3*abs(lamb3))/2.d0
!
    jm = jac*mult*0.5d0
!
    if (ndim .eq. 3) then
        zr(igthet-1+1)=zr(igthet-1+1)+g*jac*mult
        zr(igthet-1+2)=zr(igthet-1+2)+g1*jac*mult
        zr(igthet-1+3)=zr(igthet-1+3)+gs2*jac*mult
        zr(igthet-1+4)=zr(igthet-1+4)+gs3*jac*mult
        zr(igthet-1+5)=zr(igthet-1+5)+g1*jac*mult*coeff
        zr(igthet-1+6)=zr(igthet-1+6)+g2*jac*mult*coeff
        zr(igthet-1+7)=zr(igthet-1+7)+g3*jac*mult*coeff3
    else if (ndim.eq.2) then
!
! PAS PROGRAMME POUR L INSTANT
!
        ASSERT(.false.)
!
    endif
end subroutine
