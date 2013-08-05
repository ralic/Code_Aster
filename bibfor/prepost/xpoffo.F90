subroutine xpoffo(ndim, ndime, elrefp, nnop, igeom,&
                  co, ff)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reere3.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: ndim, ndime, nnop, igeom
    real(kind=8) :: co(ndim), ff(nnop)
    character(len=8) :: elrefp
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     FF : FONCTIONS DE FORMES AU NOEUD SOMMET OU D'INTERSECTION
!
!   IN
!     NDIM   : DIMENSION DU MAILLAGE
!     NDIME  : DIMENSION TOPOLOGIQUE DE LA MAILLE PARENT
!     ELREFP : ÉLÉMENT DE RÉFÉRENCE PARENT
!     NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
!     IGEOM  : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
!     CO     : COORDONNÉES DU NOUVEAU NOEUD
!
!   OUT
!     FF    : FONCTIONS DE FORMES DE L'ELEMENT PARENT AU NOUVEAU NOEUD
!
!
    real(kind=8) :: a(3), b(3), c(3), ab(3), ac(3), nd(3), norme, nab, y(3)
    real(kind=8) :: an(ndim), coloc(2), r0, r1, r2, r3, rbid, xe(3), n(ndim)
    integer :: j, igeolo, ino, ibid
    character(len=24) :: geomlo
!
    call jemarq()
!
!     CAS DES ELEMENTS PRINCIPAUX : C SIMPLE, ON APPELLE REEREF
    if (ndim .eq. ndime) then
        call reere3(elrefp, nnop, igeom, co, r0,&
                    .false., ndim, r1, ibid, ibid,&
                    ibid, ibid, ibid, r2, r3,&
                    'NON', xe, ff, r0, r0,&
                    r0, r0)
!
!
!     CAS DES ELEMENTS DE BORDS : C PLUS COMPLIQUÉ
!     ON NE PROCEDE PAS COMME DANS TE0036 CAR ICI, ON N'EST PAS
!     DANS UNE BOUCLE SUR LES SOUS-ELEMENTS
    else if (ndim.ne.ndime) then
!
        ASSERT(ndim.eq.ndime+1)
!
!       CREATION D'UNE BASE LOCALE A LA MAILLE PARENT ABCD
        call vecini(3, 0.d0, ab)
        call vecini(3, 0.d0, ac)
        do 113 j = 1, ndim
            a(j)=zr(igeom-1+ndim*(1-1)+j)
            b(j)=zr(igeom-1+ndim*(2-1)+j)
            if (ndim .eq. 3) c(j)=zr(igeom-1+ndim*(3-1)+j)
            ab(j)=b(j)-a(j)
            if (ndim .eq. 3) ac(j)=c(j)-a(j)
113      continue
!
        if (ndime .eq. 2) then
!         CREATION DU REPERE LOCAL LO : (AB,Y)
            call provec(ab, ac, nd)
            call normev(nd, norme)
            call normev(ab, nab)
            call provec(nd, ab, y)
        else if (ndime.eq.1) then
!         CREATION DU REPERE LOCAL 1D : AB/NAB
            call normev(ab, nab)
            call vecini(3, 0.d0, nd)
            nd(1) = ab(2)
            nd(2) = -ab(1)
        endif
!
!       COORDONNÉES DES NOEUDS DE L'ELREFP DANS LE REPÈRE LOCAL
        geomlo='&&TE0036.GEOMLO'
        call wkvect(geomlo, 'V V R', nnop*ndime, igeolo)
        do 114 ino = 1, nnop
            do 115 j = 1, ndim
                n(j)=zr(igeom-1+ndim*(ino-1)+j)
                an(j)=n(j)-a(j)
115          continue
            zr(igeolo-1+ndime*(ino-1)+1)=ddot(ndim,an,1,ab,1)
            if (ndime .eq. 2) zr(igeolo-1+ndime*(ino-1)+2)=ddot(ndim,an, 1,y ,1 )
114      continue
!
!       COORDONNÉES RÉELLES LOCALES DU POINT EN QUESTION : COLOC
        do 116 j = 1, ndim
            an(j)=co(j)-a(j)
116      continue
        coloc(1)=ddot(ndim,an,1,ab,1)
        if (ndime .eq. 2) coloc(2)=ddot(ndim,an,1,y,1)
        call reere3(elrefp, nnop, igeolo, coloc, rbid,&
                    .false., ndime, rbid, ibid, ibid,&
                    ibid, ibid, ibid, rbid, rbid,&
                    'NON', xe, ff, rbid, rbid,&
                    rbid, rbid)
!
        call jedetr(geomlo)
!
    endif
!
    call jedema()
!
end subroutine
