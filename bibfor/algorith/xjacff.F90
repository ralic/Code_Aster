subroutine xjacff(elrefp, elrefc, elc, ndim, fpg,&
                  jinter, ifa, cface, ipg, nnop,&
                  igeom, jbasec, xg, jac, ffp,&
                  ffpc, dfdi, nd, tau1, tau2)
! aslint: disable=W1306
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2b.h"
#include "asterfort/elelin.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lteatt.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "blas/ddot.h"
    integer :: jinter, ifa, cface(18, 6), ipg, nnop, igeom, jbasec, ndim
    real(kind=8) :: jac, ffp(27), ffpc(27), dfdi(nnop, ndim)
    real(kind=8) :: nd(ndim), tau1(ndim), tau2(ndim), xg(3)
    character(len=8) :: elrefp, fpg, elrefc, elc
!
!     ------------------------------------------------------------------
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
!                   CALCUL DU JACOBIEN DE LA TRANSFORMATION FACETTE
!                       RÉELLE EN 3D À FACETTE DE RÉFÉRENCE 2D
!                   ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!               ET DE LA NORMALE À LA FACETTE ORIENTÉE DE ESCL -> MAIT
!     ENTREE
!       ELREFP  : TYPE DE L'ELEMENT DE REF PARENT
!       FPG     : FAMILLE DE POINTS DE GAUSS (SCHEMA D'INTEGRATION)
!       PINTER  : COORDONNÉES DES POINTS D'INTERSECTION
!       IFA     : INDINCE DE LA FACETTE COURANTE
!       CFACE   : CONNECTIVITÉ DES NOEUDS DES FACETTES
!       IPG     : NUMÉRO DU POINTS DE GAUSS
!       NNO     : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
!       IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT DE REF PARENT
!
!     SORTIE
!       G       : COORDONNÉES RÉELLES 3D DU POINT DE GAUSS
!       JAC     : PRODUIT DU JACOBIEN ET DU POIDS
!       FF      : FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!       ND      : NORMALE À LA FACETTE ORIENTÉE DE ESCL -> MAIT
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: norme
    real(kind=8) :: grlt(3), grln(3), norm2, ps
    integer :: ibid, nn, nnoc
    integer :: j, k, i, nno, ipoidf, ivff, idfdef, ndimf
    real(kind=8) :: xe(3), coor3d(3*27)
    character(len=8) :: k8bid
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call elrefe_info(elrefe=elc,fami=fpg,ndim=ndimf,nno=nno,&
                     jpoids=ipoidf,jvf=ivff,jdfde=idfdef)
!
    ASSERT(nno.eq.3.or.nno.eq.6)
    ASSERT(ndim.eq.3)
!
! --- INITIALISATION
    call vecini(3, 0.d0, nd)
    call vecini(3, 0.d0, grln)
    call vecini(3, 0.d0, grlt)
    call vecini(3, 0.d0, tau1)
    call vecini(3, 0.d0, tau2)
!
! --- COORDONNÉES DES NOEUDS DE LA FACETTE DANS LE REPERE GLOBAL NDIM
    nn=3*nno
    do i = 1, nn
        coor3d(i)=0.d0
    end do
    do i = 1, nno
        do j = 1, ndim
            coor3d((i-1)*ndim+j)=zr(jinter-1+ndim*(cface(ifa,i)-1)+j)
        end do
    end do
!     CALCUL DE JAC EN 3D
    k = 2*(ipg-1)*nno
    call dfdm2b(nno, zr(ipoidf-1+ipg), zr(idfdef+k), coor3d,&
                jac, nd)
!
! --- COORDONNEES REELLES 3D DU POINT DE GAUSS IPG
    call vecini(3, 0.d0, xg)
    do j = 1, nno
        do i = 1, ndim
          xg(i)=xg(i)+zr(ivff-1+nno*(ipg-1)+j)*coor3d(ndim*(j-1)+i)
        end do
    end do
!
! --- CONSTRUCTION DE LA BASE AU POINT DE GAUSS
!
    do j = 1, ndim
        do k = 1, nno
            grln(j) = grln(j) + zr(ivff-1+nno*(ipg-1)+k)*zr(jbasec-1+ndim*ndim*(k-1)+j)
            grlt(j)= grlt(j) + zr(ivff-1+nno*(ipg-1)+k)*zr(jbasec-1+ndim*ndim*(k-1)+j+&
            ndim)
        end do
    end do
!
    ps=ddot(ndim,grln,1,nd,1)
    if (ps.lt.0.d0) nd(1:3) = -nd(1:3)
    ps=ddot(ndim,grlt,1,nd,1)
    do j = 1, ndim
        tau1(j)=grlt(j)-ps*nd(j)
    end do
!
    call normev(tau1, norme)
!
    if (norme .lt. 1.d-12) then
!       ESSAI AVEC LE PROJETE DE OX
        tau1(1)=1.d0-nd(1)*nd(1)
        tau1(2)=0.d0-nd(1)*nd(2)
        if (ndim .eq. 3) tau1(3)=0.d0-nd(1)*nd(3)
        call normev(tau1, norm2)
        if (norm2 .lt. 1.d-12) then
!         ESSAI AVEC LE PROJETE DE OY
            tau1(1)=0.d0-nd(2)*nd(1)
            tau1(2)=1.d0-nd(2)*nd(2)
            if (ndim .eq. 3) tau1(3)=0.d0-nd(2)*nd(3)
            call normev(tau1, norm2)
        endif
        ASSERT(norm2.gt.1.d-12)
    endif
    if (ndim .eq. 3) then
        call provec(nd, tau1, tau2)
    endif
    call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ffp, dfdi=dfdi)
!
!
    if (elrefc .eq. elrefp) goto 999
    if (elrefc(1:3) .eq. 'NON') goto 999
!
!     CALCUL DES FF DE L'ÉLÉMENT DE CONTACT EN CE POINT DE GAUSS
    call elelin(3, elrefc, k8bid, nnoc, ibid)
!
    call reeref(elrefc, nnoc, zr(igeom), xg, ndim, xe, ffpc)
!
999 continue
!
    call jedema()
end subroutine
