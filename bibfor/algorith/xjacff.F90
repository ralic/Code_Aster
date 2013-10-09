subroutine xjacff(elrefp, elrefc, elc, ndim, fpg,&
                  jinter, ifa, cface, ipg, nno,&
                  igeom, jbasec, g, jac, ffp,&
                  ffpc, dfdi, nd1, tau1, tau2)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elelin.h"
#include "asterfort/elref4.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lteatt.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/reereg.h"
#include "asterfort/vecini.h"
#include "blas/ddot.h"
    integer :: jinter, ifa, cface(5, 3), ipg, nno, igeom, jbasec, ndim
    real(kind=8) :: jac, ffp(27), ffpc(27), dfdi(nno, ndim)
    real(kind=8) :: nd(ndim), tau1(ndim), tau2(ndim)
    character(len=8) :: elrefp, fpg, elrefc, elc
!
!     ------------------------------------------------------------------
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
    real(kind=8) :: a(3), b(3), c(3), ab(3), ac(3), y(3), norme, nab, g(3)
    real(kind=8) :: rbid, xg(2), ksig(2)
    real(kind=8) :: ff(27)
    real(kind=8) :: grlt(3), norm2, ps, nd1(3)
    integer :: ibid, nbnomx, nnoc, nnos
    integer :: j, k, nnof, ipoidf, ivff, idfdef, ndimf
    character(len=8) :: k8bid
    integer :: ddlh, nfe, ddls, ddlm
    real(kind=8) :: he, fe(4), dgdgl(4, 3)
    real(kind=8) :: xe(3), f(3, 3), dfdic(27, 3)
    real(kind=8) :: eps(6), grad(3, 3), coor2d(6)
!
    logical :: axi
!
    parameter       (nbnomx = 27)
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call elref4(elc, fpg, ndimf, nnof, ibid,&
                ibid, ipoidf, ivff, idfdef, ibid)
!
    axi = lteatt(' ','AXIS','OUI')
    ASSERT(nnof.eq.3)
    ASSERT(ndim.eq.3)
!
! --- INITIALISATION
    call vecini(3, 0.d0, a)
    call vecini(3, 0.d0, b)
    call vecini(3, 0.d0, c)
    call vecini(3, 0.d0, ab)
    call vecini(3, 0.d0, ac)
    call vecini(3, 0.d0, nd1)
    call vecini(3, 0.d0, nd)
    call vecini(3, 0.d0, grlt)
    call vecini(3, 0.d0, tau1)
    call vecini(3, 0.d0, tau2)
!
    do j = 1, ndim
        a(j)=zr(jinter-1+ndim*(cface(ifa,1)-1)+j)
        b(j)=zr(jinter-1+ndim*(cface(ifa,2)-1)+j)
        c(j)=zr(jinter-1+ndim*(cface(ifa,3)-1)+j)
        ab(j)=b(j)-a(j)
        ac(j)=c(j)-a(j)
    end do
!
    call provec(ab, ac, nd)
    call normev(nd, norme)
    call normev(ab, nab)
    call provec(nd, ab, y)
!
!     COORDONNÉES DES SOMMETS DE LA FACETTE DANS LE REPÈRE LOCAL 2D
    coor2d(1)=0.d0
    coor2d(2)=0.d0
    coor2d(3)=nab
    coor2d(4)=0.d0
    coor2d(5)=ddot(3,ac,1,ab,1)
    coor2d(6)=ddot(3,ac,1,y ,1)
!
!     CALCUL DE JAC EN 2D
    call dfdm2d(nnof, ipg, ipoidf, idfdef, coor2d,&
                jac)
!
!     COORDONNÉES RÉELLES 2D DU POINT DE GAUSS IPG
    call vecini(2, 0.d0, xg)
    do j = 1, nnof
        xg(1)=xg(1)+zr(ivff-1+nnof*(ipg-1)+j)*coor2d(2*j-1)
        xg(2)=xg(2)+zr(ivff-1+nnof*(ipg-1)+j)*coor2d(2*j)
    end do
!
!     COORDONNÉES RÉELLES 3D DU POINT DE GAUSS
    g(1)=a(1)+ab(1)*xg(1)+y(1)*xg(2)
    g(2)=a(2)+ab(2)*xg(1)+y(2)*xg(2)
    g(3)=a(3)+ab(3)*xg(1)+y(3)*xg(2)
!
! --- COORDONNEES DE REFERENCE 2D DU POINT DE GAUSS
    call reereg('S', elc, nnof, coor2d, xg,&
                ndimf, ksig, ibid)
!
! --- CONSTRUCTION DE LA BASE AU POINT DE GAUSS
!     CALCUL DES FF DE LA FACETTE EN CE POINT DE GAUSS
    call elrfvf(elc, ksig, nbnomx, ff, ibid)
!
    do j = 1, ndim
        do k = 1, nnof
            nd1(j) = nd1(j) + ff(k)*zr(jbasec-1+ndim*ndim*(k-1)+j)
            grlt(j)= grlt(j) + ff(k)*zr(jbasec-1+ndim*ndim*(k-1)+j+&
            ndim)
        end do
    end do
!
    call normev(nd1, norme)
    ps=ddot(ndim,grlt,1,nd1,1)
    do j = 1, ndim
        tau1(j)=grlt(j)-ps*nd1(j)
    end do
!
    call normev(tau1, norme)
!
    if (norme .lt. 1.d-12) then
!       ESSAI AVEC LE PROJETE DE OX
        tau1(1)=1.d0-nd1(1)*nd1(1)
        tau1(2)=0.d0-nd1(1)*nd1(2)
        if (ndim .eq. 3) tau1(3)=0.d0-nd1(1)*nd1(3)
        call normev(tau1, norm2)
        if (norm2 .lt. 1.d-12) then
!         ESSAI AVEC LE PROJETE DE OY
            tau1(1)=0.d0-nd1(2)*nd1(1)
            tau1(2)=1.d0-nd1(2)*nd1(2)
            if (ndim .eq. 3) tau1(3)=0.d0-nd1(2)*nd1(3)
            call normev(tau1, norm2)
        endif
        ASSERT(norm2.gt.1.d-12)
    endif
    if (ndim .eq. 3) then
        call provec(nd1, tau1, tau2)
    endif
    call elelin(3, elrefp, k8bid, ibid, nnos)
    call reeref(elrefp, axi, nno, nnos, zr(igeom),&
                g, 0, .false., ndim, he,&
                rbid, rbid, ibid, ibid, ddlh,&
                nfe, ddls, ddlm, fe, dgdgl,&
                'DFF', xe, ffp, dfdi, f,&
                eps, grad)
!
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
999 continue
!
    call jedema()
end subroutine
