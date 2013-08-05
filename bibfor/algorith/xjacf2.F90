subroutine xjacf2(elrefp, elrefc, elc, ndim, fpg,&
                  jinter, ifa, cface, nptf, ipg,&
                  nno, igeom, jbasec, g, jac,&
                  ffp, ffpc, dfdi, nd, tau1)
    implicit none
!
#include "jeveux.h"
#include "asterfort/abscvf.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/elelin.h"
#include "asterfort/elref4.h"
#include "asterfort/elrfvf.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lteatt.h"
#include "asterfort/normev.h"
#include "asterfort/reeref.h"
#include "asterfort/reereg.h"
#include "asterfort/reerel.h"
#include "asterfort/vecini.h"
#include "blas/ddot.h"
    integer :: jinter, ifa, cface(5, 3), ipg, nno, igeom, jbasec, nptf, ndim
    real(kind=8) :: jac, ffp(27), ffpc(27), dfdi(27, 3)
    real(kind=8) :: nd(3), tau1(3), g(3)
    character(len=8) :: elrefp, fpg, elc, elrefc
!
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
!                   CALCUL DU JACOBIEN DE LA TRANSFORMATION FACETTE
!                       RÉELLE EN 2D À FACETTE DE RÉFÉRENCE 1D
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
!       G       : COORDONNÉES RÉELLES 2D DU POINT DE GAUSS
!       JAC     : PRODUIT DU JACOBIEN ET DU POIDS
!       FF      : FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!       ND      : NORMALE À LA FACETTE ORIENTÉE DE ESCL -> MAIT
!                 AU POINT DE GAUSS
!       TAU1    : TANGENTE A LA FACETTE AU POINT DE GAUSS
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: xg, a(3), b(3), ab(3), ksig1, ksig2(3), ksib
    real(kind=8) :: ff(27), seg(3)
    real(kind=8) :: grlt(3), normab, norme, norm2, ps
    integer :: ndimf, nbnomx, nnoc, nnos, nn
    integer :: i, j, k, nnof, ipoidf, ivff, idfdef
    logical :: axi
    character(len=8) :: k8bid
    integer :: ddlh, nfe, ddls, ddlm
    real(kind=8) :: he, fe(4), dgdgl(4, 3)
    real(kind=8) :: xe(3), f(3, 3), dfdic(27, 3)
    real(kind=8) :: eps(6), grad(3, 3)
    integer :: ibid, ibid2, ibid3, nptfmx
    real(kind=8) :: dfdx(3), rbid, rbid2, rbid3, rbid4
!
    parameter       (nbnomx = 27, nptfmx=4)
    real(kind=8) :: coor2d(nptfmx*3), coor1d(6)
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call elref4(elc, fpg, ndimf, nnof, ibid,&
                ibid2, ipoidf, ivff, idfdef, ibid3)
!
    axi = lteatt(' ','AXIS','OUI')
!
    ASSERT(ndim.eq.2)
    ASSERT(nptf.le.nptfmx)
!
! --- INITIALISATION
    call vecini(3, 0.d0, nd)
    call vecini(3, 0.d0, grlt)
    call vecini(3, 0.d0, a)
    call vecini(3, 0.d0, b)
    call vecini(3, 0.d0, ab)
    call vecini(3, 0.d0, tau1)
    call vecini(3, 0.d0, seg)
!
! --- COORDONNÉES DES SOMMETS DE LA FACETTE DANS LE REPERE GLOBAL NDIM
    nn=3*nptfmx
    do 9 i = 1, nn
        coor2d(i)=0.d0
 9  end do
    do 10 i = 1, nptf
        do 11 j = 1, ndim
            coor2d((i-1)*ndim+j)=zr(jinter-1+ndim*(cface(ifa,i)-1)+j)
11      continue
10  end do
!
    do 20 j = 1, ndim
        a(j)=zr(jinter-1+ndim*(cface(ifa,1)-1)+j)
        b(j)=zr(jinter-1+ndim*(cface(ifa,2)-1)+j)
        ab(j)=b(j)-a(j)
20  end do
!
! --- COORDONNÉES DES SOMMETS DE LA FACETTE DANS LE REPÈRE
!     LIE A LA FACETTE
    if (iselli(elc)) then
!     EN LINEAIRE 2D
        call normev(ab, normab)
        coor1d(1)=0.d0
        coor1d(2)=0.d0
        coor1d(3)=normab
        coor1d(4)=0.d0
        coor1d(5)=0.d0
        coor1d(6)=0.d0
        seg(1)=0.d0
        seg(2)=normab
    else if (.not.iselli(elc)) then
!     EN QUADRATIQUE 2D
        ksib=1.d0
        call abscvf(ndim, coor2d, ksib, normab)
        coor1d(1)=0.d0
        coor1d(2)=0.d0
        coor1d(3)=normab
        coor1d(4)=0.d0
        coor1d(5)=normab/2
        coor1d(6)=0.d0
        seg(1)=0.d0
        seg(2)=normab
        seg(3)=normab/2
    endif
!
! --- CALCUL DE JAC EN 1D
    k = (ipg-1)*nnof
    call dfdm1d(nnof, zr(ipoidf-1+ipg), zr(idfdef+k), coor1d, dfdx,&
                rbid2, jac, rbid3, rbid4)
!
! --- COORDONNEES REELLES 1D DU POINT DE GAUSS IPG (ABS CUR DE G)
    xg=0.d0
    do 30 j = 1, nnof
        xg=xg+zr(ivff-1+nnof*(ipg-1)+j)*coor1d(2*j-1)
30  end do
!
! --- COORDONNEES DE REFERENCE 1D DU POINT DE GAUSS
    call reereg('S', elc, nnof, seg, xg,&
                ndimf, ksig1, ibid)
!
! --- COORDONNEES REELLES 2D DU POINT DE GAUSS
    ksig2(1)=ksig1
    ksig2(2)=0.d0
    call reerel(elc, nnof, ndim, coor2d, ksig2,&
                g)
!
! --- CONSTRUCTION DE LA BASE AU POINT DE GAUSS
!     CALCUL DES FF DE LA FACETTE EN CE POINT DE GAUSS
    call elrfvf(elc, [ksig1], nbnomx, ff, ibid)
!
    do 40 j = 1, ndim
        do 41 k = 1, nnof
            nd(j) = nd(j) + ff(k)*zr(jbasec-1+ndim*ndim*(k-1)+j)
            grlt(j)= grlt(j) + ff(k)*zr(jbasec-1+ndim*ndim*(k-1)+j+&
            ndim)
41      continue
40  end do
!
    call normev(nd, norme)
    ps=ddot(ndim,grlt,1,nd,1)
    do 50 j = 1, ndim
        tau1(j)=grlt(j)-ps*nd(j)
50  end do
    call normev(tau1, norme)
!
    if (norme .lt. 1.d-12) then
!       ESSAI AVEC LE PROJETE DE OX
        tau1(1)=1.d0-nd(1)*nd(1)
        tau1(2)=0.d0-nd(1)*nd(2)
        call normev(tau1, norm2)
        if (norm2 .lt. 1.d-12) then
!         ESSAI AVEC LE PROJETE DE OY
            tau1(1)=0.d0-nd(2)*nd(1)
            tau1(2)=1.d0-nd(2)*nd(2)
            call normev(tau1, norm2)
        endif
        ASSERT(norm2.gt.1.d-12)
    endif
!
!     CALCUL DES FF DE L'ÉLÉMENT PARENT EN CE POINT DE GAUSS
    call elelin(3, elrefp, k8bid, ibid, nnos)
    call reeref(elrefp, axi, nno, nnos, zr(igeom),&
                g, 0, .false., ndim, he,&
                rbid, rbid, ibid, ibid, ddlh,&
                nfe, ddls, ddlm, fe, dgdgl,&
                'DFF', xe, ffp, dfdi, f,&
                eps, grad)
!
    if (elrefc .eq. elrefp) goto 999
    if (elrefc(1:3) .eq. 'NON') goto 999
!
!     CALCUL DES FF DE L'ÉLÉMENT DE CONTACT EN CE POINT DE GAUSS
    call elelin(3, elrefc, k8bid, nnoc, ibid)
!
    call reeref(elrefc, axi, nnoc, nnoc, zr(igeom),&
                g, 0, .false., ndim, he,&
                rbid, rbid, ibid, ibid, ddlh,&
                nfe, ddls, ddlm, fe, dgdgl,&
                'NON', xe, ffpc, dfdic, f,&
                eps, grad)
!
999  continue
!
    call jedema()
end subroutine
