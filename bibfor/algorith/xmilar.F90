subroutine xmilar(ndim, ndime, elrefp, geom,  pinref,&
                  ia, ib, im, ip, ksia, ksib, milara, milarb,&
                  pintt, pmitt)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/xelrex.h"
#include "asterfort/reeref.h"
#include "asterfort/reerel.h"
#include "asterfort/xnormv.h"
#include "blas/ddot.h"
    integer :: ndim, ndime, ia, ib, im, ip
    real(kind=8) :: milara(3), milarb(3), pinref(*), geom(*)
    real(kind=8) :: ksia(ndime), ksib(ndime), pintt(*), pmitt(*)
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
!                      TROUVER LES PTS MILIEUX ENTRE LES EXTREMITES DE
!                      L'ARETE ET LE POINT D'INTERSECTION
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       PINTER  : COORDONNÉES DES POINTS D'INTERSECTION
!       TABAR   : COORDONNEES DES 3 NOEUDS DE L'ARETE
!       AREINT  : POSITION DU PT INTER DE L'ARETE DANS LA LISTE
!       PINTT   : COORDONNEES REELES DES POINTS D'INTERSECTION
!       PINTT   : COORDONNEES REELES DES POINTS MILIEUX
!
!     SORTIE
!       MILARA  : COOR DU PT MILIEU ENTRE 1ER PT DE COORSG ET PT INTER
!       MILARB  : COOR DU PT MILIEU ENTRE 2EM PT DE COORSG ET PT INTER
!     ----------------------------------------------------------------
!
    integer :: nno, j
    real(kind=8) :: x(81), newpt(ndime), pta(ndime), ptb(ndime), ptm(ndime), ff(27)
    real(kind=8) :: ab(ndime), aip(ndime), normab, normaip, s
!
! --------------------------------------------------------------------
!
    call xelrex(elrefp, nno, x)
    if (ia.lt.1000) then
       do j = 1, ndime
          pta(j) = x(ndime*(ia-1)+j)
       end do
    else
       do j = 1, ndime
          newpt(j) = pintt(ndime*(ia-1001)+j)
       end do
       call reeref(elrefp, nno, geom, newpt, ndime,&
                   pta, ff)
    endif
    if (ib.lt.1000) then
       do j = 1, ndime
          ptb(j) = x(ndime*(ib-1)+j)
       end do
    else
       do j = 1, ndime
          newpt(j) = pintt(ndime*(ib-1001)+j)
       end do
       call reeref(elrefp, nno, geom, newpt, ndime,&
                   ptb, ff)
    endif
    if (im.lt.2000) then
       do j = 1, ndime
          ptm(j) = x(ndime*(im-1)+j)
       end do
    else
       do j = 1, ndime
          newpt(j) = pmitt(ndime*(im-2001)+j)
       end do
       call reeref(elrefp, nno, geom, newpt, ndime,&
                   ptm, ff)
    endif
!
    if (im.lt.2000) then
       do j = 1,ndime
          ksia(j)=(pinref(ndime*(ip-1)+j)+pta(j))/2.d0
          ksib(j)=(pinref(ndime*(ip-1)+j)+ptb(j))/2.d0
       enddo
    else
!   RECHERCHE DE L'ABSCISSE CURVILIGNE DE IP SUR LE SEGMENT AB
       do j = 1, ndime
          ab(j) = ptb(j)-pta(j)
          aip(j) = pinref(ndime*(ip-1)+j)-pta(j)
       end do
       call xnormv(ndime, ab, normab)
       call xnormv(ndime, aip, normaip)
       s = normaip/normab*ddot(ndime, ab, 1, aip, 1)
       do j = 1,ndime
          ksia(j)=(1.d0-s)*(1.d0-s/2.d0)*pta(j) + s/2.d0*(s-1.d0)*ptb(j) + s*(2.d0-s)*ptm(j)
          ksib(j)=s/2.d0*(s-1.d0)*pta(j) + s/2.d0*(s+1.d0)*ptb(j) + (s+1.d0)*(1.d0-s)*ptm(j)
       enddo
    endif
!
    call reerel(elrefp, nno, ndim, geom, ksia,&
                milara)
    call reerel(elrefp, nno, ndim, geom, ksib,&
                milarb)
!
end subroutine
