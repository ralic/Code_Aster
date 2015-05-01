subroutine xintva(elrefp, n, ptxx, ndime, intinf, intsup)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/provec.h"
#include "asterfort/xelrex.h"
#include "asterfort/xnormv.h"
#include "blas/ddot.h"
    integer :: n(3), ndime
    real(kind=8) :: ptxx(*), intinf, intsup
    character(len=8) :: elrefp
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
    real(kind=8) :: x(81), a(ndime), b(ndime), c(ndime), ab(ndime), bc(ndime)
    real(kind=8) :: ca(ndime), ptini(ndime), k(ndime), det, alpha, kappa
    real(kind=8) :: ka(ndime), kb(ndime), kc(ndime), b1, c1, c2
    real(kind=8) :: norm_ab, norm_bc , norm_ca, eps, tole
    integer :: nno, ia , ib , ic, j, cpt
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
!   RECUPERATION DES COORDONNEES DE REFERENCE DES NOEUDS SOMMETS DE LA FACE
    call xelrex(elrefp, nno, x)
    ib=n(1)
    ic=n(2)
    ia=n(3)
    do j = 1, ndime
       a(j)=x(ndime*(ia-1)+j)
       b(j)=x(ndime*(ib-1)+j)
       c(j)=x(ndime*(ic-1)+j)
    end do
!
!   RECUPERATION DES ARETES DE LA FACE TRIA
    do j = 1, ndime
       ab(j)=x(ndime*(ib-1)+j)-x(ndime*(ia-1)+j)
       bc(j)=x(ndime*(ic-1)+j)-x(ndime*(ib-1)+j)
       ca(j)=x(ndime*(ia-1)+j)-x(ndime*(ic-1)+j)
    end do
!
    call xnormv(ndime, ab, norm_ab)
    call xnormv(ndime, bc, norm_bc)
    call xnormv(ndime, ca, norm_ca)
!
!   RECUPERACTION DU POINT DE DEPART ET DE LA DIRECTION DE RECHERCHE
    do j = 1, ndime
       ptini(j)=ptxx(ndime+j)
       k(j)=ptxx(j)
    end do
!
!   INTERSECTION SUR LA FACE AB (ON PROJETTE LE SYSTEME D'EQUATION DANS LE REPERE DE LA FACE)
    b1 = ddot(ndime,k,1,ab,1)
    do j = 1, ndime
       ka(j)=a(j)-ptini(j)
    end do
    c1 = ddot(ndime,k,1,ka,1)
    c2 = ddot(ndime,ka,1,ab,1)
    det =  -1.d0+b1**2
!    write(6,*)'KOR: det1=',det
    if (abs(det).gt.eps) then
       kappa = (-c1+c2*b1)/det
       alpha = (c2-b1*c1)/det
       alpha = alpha/norm_ab
!    write(6,*)'KOR: alpha1=',alpha
       if (alpha.le.(1.d0+tole).and.alpha.ge.-tole) then
          if (kappa.le.0.d0) intinf = kappa
          if (kappa.ge.0.d0) intsup = kappa
          cpt = cpt+1
       endif
    endif
!
!   INTERSECTION SUR LA FACE BC
    b1 = ddot(ndime,k,1,bc,1)
    do j = 1, ndime
       kb(j)=b(j)-ptini(j)
    end do
    c1 = ddot(ndime,k,1,kb,1)
    c2 = ddot(ndime,kb,1,bc,1)
    det = -1.d0+b1**2
!    write(6,*)'KOR: det2=',det
    if (abs(det).gt.eps) then
       kappa = (-c1+c2*b1)/det
       alpha = (c2-b1*c1)/det
       alpha = alpha/norm_bc
!    write(6,*)'KOR: alpha2=',alpha
       if (alpha.le.(1.d0+tole).and.alpha.ge.-tole) then
          if (kappa.le.0.d0) intinf = kappa
          if (kappa.ge.0.d0) intsup = kappa
          cpt = cpt+1
       endif
    endif
!
!   INTERSECTION SUR LA FACE CA
    b1 = ddot(ndime,k,1,ca,1)
    do j = 1, ndime
       kc(j)=c(j)-ptini(j)
    end do
    c1 = ddot(ndime,k,1,kc,1)
    c2 = ddot(ndime,kc,1,ca,1)
    det =  -1.d0+b1**2
!    write(6,*)'KOR: det3=',det
    if (abs(det).gt.eps) then
       kappa = (-c1+c2*b1)/det
       alpha = (c2-b1*c1)/det
       alpha = alpha/norm_ca
!    write(6,*)'KOR: alpha3=',alpha
       if (alpha.le.(1.d0+tole).and.alpha.ge.-tole) then
          if (kappa.le.0.d0) intinf = kappa
          if (kappa.ge.0.d0) intsup = kappa
          cpt = cpt+1
       endif
    endif
!
    ASSERT(cpt.ge.2)
!
    call jedema()
end subroutine
