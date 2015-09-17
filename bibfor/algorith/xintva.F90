subroutine xintva(name, dekker, ptxx, ndime, intinf, intsup)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/provec.h"
#include "asterfort/xnormv.h"
#include "blas/ddot.h"
    integer :: ndime
    character(len=6) :: name
    real(kind=8) :: ptxx(*), intinf, intsup, dekker(4*ndime)
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
!         RECHERCHE DE L'INTERVALE DE RECHERCHE POUR LA RESOLUTION
!                   DU PROBLEME SCALAIRE DANS XMIFIS
!
!     ENTREE
!       N       : INDICE DES NOEUDS SOMMETS DE LA FACE TRIA DANS
!                 LAQUELLE ON EFFECTUE LA RECHERCHE
!       PTXX    : PONT DE DEPART ET DIRECTION DE RECHERCHE
!     SORTIE
!       INTINF  : BORNE INFERIEURE DE L'INTERVALLE DE RECHERCHE
!       INTSUP  : BORNE SUPERIEURE DE L'INTERVALLE DE RECHERCHE
!     --------------------------------------------------------------
!
    real(kind=8) :: a(ndime), b(ndime), c(ndime), ab(ndime), bc(ndime)
    real(kind=8) :: ca(ndime), ptini(ndime), k(ndime), det, alpha, kappa
    real(kind=8) :: ka(ndime), kb(ndime), kc(ndime), b1, c1, c2, ad(ndime)
    real(kind=8) :: norm_ab, norm_bc , norm_ca, eps, tole, d(ndime), bd(ndime)
    real(kind=8) :: norm_n, n(ndime), norm_ad, norm_bd
    integer :: j, cpt
    parameter (eps = 1.d-12)
    parameter (tole = 5.d-7)
!
! ------------------------------------------------------------------
!
    call jemarq()
!
    intinf=0.d0
    intsup =0.d0
    cpt = 0
!   RECUPERATION DES COORDONNEES DE REFERENCE DES NOEUDS SOMMETS DU SOUS ELEMENT
    do j = 1, ndime
       a(j)=dekker(j)
       b(j)=dekker(ndime+j)
       c(j)=dekker(2*ndime+j)
       d(j)=dekker(3*ndime+j)
    end do
!
!   RECUPERATION DES ARETES DU SOUS ELEMENT
    do j = 1, ndime
       ab(j)=b(j)-a(j)
       bc(j)=c(j)-b(j)
       ca(j)=a(j)-c(j)
       ad(j)=d(j)-a(j)
       bd(j)=d(j)-b(j)
    end do
!
    call xnormv(ndime, ab, norm_ab)
    call xnormv(ndime, bc, norm_bc)
    call xnormv(ndime, ca, norm_ca)
    call xnormv(ndime, ad, norm_ad)
    call xnormv(ndime, bd, norm_bd)
!
!   RECUPERACTION DU POINT DE DEPART ET DE LA DIRECTION DE RECHERCHE
    do j = 1, ndime
       ptini(j)=ptxx(ndime+j)
       k(j)=ptxx(j)
    end do
!
    if (name.eq.'XMIFIS') then
!   ON CHERCHE LES INTERSECTIONS DE LA DIRECTION DE RECHERCHE AVEC LES ARETES DE LA FACE TRIA
!
!   INTERSECTION SUR L'ARETE AB (ON PROJETTE LE SYSTEME D'EQUATION DANS LE REPERE DE LA FACE)
       b1 = ddot(ndime,k,1,ab,1)
       do j = 1, ndime
          ka(j)=a(j)-ptini(j)
       end do
       c1 = ddot(ndime,k,1,ka,1)
       c2 = ddot(ndime,ka,1,ab,1)
       det =  -1.d0+b1**2
       if (abs(det).gt.eps) then
          kappa = (-c1+c2*b1)/det
          alpha = (c2-b1*c1)/det
          alpha = alpha/norm_ab
          if (alpha.le.(1.d0+tole).and.alpha.ge.-tole) then
             if (kappa.le.0.d0) intinf = kappa
             if (kappa.ge.0.d0) intsup = kappa
             cpt = cpt+1
          endif
       endif
!
!   INTERSECTION SUR L'ARETE BC
       b1 = ddot(ndime,k,1,bc,1)
       do j = 1, ndime
          kb(j)=b(j)-ptini(j)
       end do
       c1 = ddot(ndime,k,1,kb,1)
       c2 = ddot(ndime,kb,1,bc,1)
       det = -1.d0+b1**2
       if (abs(det).gt.eps) then
          kappa = (-c1+c2*b1)/det
          alpha = (c2-b1*c1)/det
          alpha = alpha/norm_bc
          if (alpha.le.(1.d0+tole).and.alpha.ge.-tole) then
             if (kappa.le.0.d0) intinf = kappa
             if (kappa.ge.0.d0) intsup = kappa
             cpt = cpt+1
          endif
       endif
!
!   INTERSECTION SUR L'ARETE CA
       b1 = ddot(ndime,k,1,ca,1)
       do j = 1, ndime
          kc(j)=c(j)-ptini(j)
       end do
       c1 = ddot(ndime,k,1,kc,1)
       c2 = ddot(ndime,kc,1,ca,1)
       det =  -1.d0+b1**2
       if (abs(det).gt.eps) then
          kappa = (-c1+c2*b1)/det
          alpha = (c2-b1*c1)/det
          alpha = alpha/norm_ca
          if (alpha.le.(1.d0+tole).and.alpha.ge.-tole) then
             if (kappa.le.0.d0) intinf = kappa
             if (kappa.ge.0.d0) intsup = kappa
             cpt = cpt+1
          endif
       endif
    elseif (name.eq.'XCENFI') then
!   ON CHERCHE LES INTERSECTIONS DE LA DIRECTION DE RECHERCHE AVEC LES FACES DU TETRA
        intinf = -10.d0
        intsup = 10.d0
!   INTERSECTION SUR LA FACE ABC
!      RECHERCHE D'UN VECTEUR UNITAIRE NORMAL A LA FACE
       call provec(ab, bc, n)
       call xnormv(ndime, n, norm_n)
!      SI LA DIRECTION DE RECHERCHE EST PARALLELE A LA FACE ON SORT
       if (abs(ddot(ndime,n,1,k,1)).ge.tole) then
          do j = 1, ndime
             ka(j)=a(j)-ptini(j)
          end do
          alpha = ddot(ndime,ka,1,n,1)/ddot(ndime,n,1,k,1)
          if (alpha.gt.intinf .and. alpha.lt.0.d0) then
             intinf = alpha
             cpt = cpt+1
          elseif (alpha.lt.intsup .and. alpha.gt.0.d0) then
             intsup = alpha
             cpt = cpt+1
          endif
       endif
!
!   INTERSECTION SUR LA FACE ADB
!      RECHERCHE D'UN VECTEUR UNITAIRE NORMAL A LA FACE
       call provec(ab, ad, n)
       call xnormv(ndime, n, norm_n)
!      SI LA DIRECTION DE RECHERCHE EST PARALLELE A LA FACE ON SORT
       if (abs(ddot(ndime,n,1,k,1)).ge.tole) then
          do j = 1, ndime
             ka(j)=a(j)-ptini(j)
          end do
          alpha = ddot(ndime,ka,1,n,1)/ddot(ndime,n,1,k,1)
          if (alpha.gt.intinf .and. alpha.lt.0.d0) then
             intinf = alpha
             cpt = cpt+1
          elseif (alpha.lt.intsup .and. alpha.gt.0.d0) then
             intsup = alpha
             cpt = cpt+1
          endif
       endif
!
!   INTERSECTION SUR LA FACE ACD
!      RECHERCHE D'UN VECTEUR UNITAIRE NORMAL A LA FACE
       call provec(ca, ad, n)
       call xnormv(ndime, n, norm_n)
!      SI LA DIRECTION DE RECHERCHE EST PARALLELE A LA FACE ON SORT
       if (abs(ddot(ndime,n,1,k,1)).ge.tole) then
          do j = 1, ndime
             ka(j)=a(j)-ptini(j)
          end do
          alpha = ddot(ndime,ka,1,n,1)/ddot(ndime,n,1,k,1)
          if (alpha.gt.intinf .and. alpha.lt.0.d0) then
             intinf = alpha
             cpt = cpt+1
          elseif (alpha.lt.intsup .and. alpha.gt.0.d0) then
             intsup = alpha
             cpt = cpt+1
          endif
       endif
!
!   INTERSECTION SUR LA FACE BCD
!      RECHERCHE D'UN VECTEUR UNITAIRE NORMAL A LA FACE
       call provec(bd, bc, n)
       call xnormv(ndime, n, norm_n)
!      SI LA DIRECTION DE RECHERCHE EST PARALLELE A LA FACE ON SORT
       if (abs(ddot(ndime,n,1,k,1)).ge.tole) then
          do j = 1, ndime
             kb(j)=b(j)-ptini(j)
          end do
          alpha = ddot(ndime,kb,1,n,1)/ddot(ndime,n,1,k,1)
          if (alpha.gt.intinf .and. alpha.lt.0.d0) then
             intinf = alpha
             cpt = cpt+1
          elseif (alpha.lt.intsup .and. alpha.gt.0.d0) then
             intsup = alpha
             cpt = cpt+1
          endif
       endif
    endif
!
    ASSERT(cpt.ge.2)
!
    call jedema()
end subroutine
