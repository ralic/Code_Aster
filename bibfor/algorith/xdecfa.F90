subroutine xdecfa(elp, nno, igeom, jlsn, jlst, npi,npis,&
                  pinter, pinref, ainter, jcnset, cooree, cooref, rainter,&
                  noeud, npts, nintar, lst ,lonref, ndim, zxain,&
                  i, face, nnose, jmilt, f, mipos)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/elrfvf.h"
#include "asterfort/iselli.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcenfi.h"
#include "asterfort/xnewto.h"
#include "asterfort/xnormv.h"
!
    integer :: npi, noeud(9), i, face, jmilt, f(6,8), npis
    integer :: igeom, jlsn, jlst, jcnset, zxain, nnose
    integer :: nintar, npts, ndim, nno
    real(kind=8) :: pinter(*), ainter(*), cooree(6,ndim), cooref(6,ndim)
    real(kind=8) :: rainter(3,4), lst(6), lonref, pinref(34*ndim)
    character(len=8) :: elp
    aster_logical :: mipos
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
!            BUT :  TROUVER LES PTS DE LA FISSURE DANS LE TRIA
!
!     ENTREE
!       ELP      : MACRO ELEMENT PARENT
!       NNO      : NOMBRE DE NOEUD DE L'ELEMENT PARENT
!       NPI      : NOMBRE DE NOEUDS DISTINCTS DE LA FISSURE
!       PINTER   : COORDONNES DES POINTS DE LA FISSURE
!       AINTER   : INFOS ARETE ASSOCIÉE AU POINTS D'INTERSECTION
!       COOREE   : COOREDONNEES REELLES DES SOMMETS DU TRIA
!       COOREF   : COOREDONNEES DE REFERENCE DES SOMMETS DU TRIA
!       RAINTER  : INFO ARETES ASSOCIES AUX POINTS D'INTERSECTION
!       LST      : LST AUX NOEUDS SOMMETS DU TRIA
!       LONREF   : LONGUEUR CARACTERISTIQUE DE L'ELEMENT
!       NDIM     : DIMENSION DU MACRO ELEMENT
!       I        : SOUS ELEMENT COURANT
!       FACE     : FACE EN COURS DANS LE SOUS SOUS ELEMENT
!       F        : CONNECTIVITE DU SOUS SOUS ELEMENT
!       NNOSE    : NOMBRE DE NOEUDS PAR SOUS ELEMENT
!
!     SORTIE
!       NPI      : NOMBRE DE NOEUDS DISTINCTS DE LA FISSURE
!       PINTER   : COORDONNES DES POINTS DE LA FISSURE
!       AINTER   : INFOS ARETE ASSOCIÉE AU POINTS D'INTERSECTION
!       NOEUD    : NUMERO DES NOEUDS DE LA FISSURE DANS L'ELEMENT
!       NPTS     : NOMBRE DE NOEUDS SOMMETS DU TRIA TELS QUE LST<=0
!       NINTAR   : NOMBRE d'ARETES DU TRIA STRICTEMENT COUPEES PAR LST
!     ----------------------------------------------------------------
!
    real(kind=8) :: p(ndim), newpt(ndim), newptref(ndim), cenref(ndim)
    real(kind=8) :: norme, geom(ndim*nno), ff(27), cenfi(ndim), tabls(20)
    real(kind=8) :: x(ndim), xref(ndim), miref(ndim), mifis(ndim), ptxx(3*ndim)
    real(kind=8) :: u(ndim), v(ndim), vectn(ndim), ksi(ndim)
    real(kind=8) :: epsmax, cridist, a, b, c
    integer :: k, ii, jj, j, ni, kk, ibid, num(8), nbnomx
    integer :: n(3), kkk
    integer :: itemax
    aster_logical :: deja
    parameter   (nbnomx = 27)
    parameter   (cridist=1.d-7)
!
! --------------------------------------------------------------------
!
!
!   INITIALISATION DU NOMBRE D'ARETES DU TRIA INTERSECTEES
!   STRICTEMENT PAR LST
    nintar = 0
!
!   INITIALISATION DU NOMBRE DE SOMMETS DU TRIA TELS QUE LST<=0
    npts = 0
!
!   BOUCLE SUR LES SOMMETS DU TRIA
    call vecini(ndim, 0.d0, newpt)
    call vecini(20, 0.d0, tabls)
    call vecini(ndim*nno, 0.d0, geom)
    call vecini(ndim, 0.d0, ksi)
    do j = 1, 9
       noeud(j) =0
    end do
!
!   BOUCLE SUR LES SOMMETS DU TRIA
    do k = 1, 3
       if (lst(k).le.0.d0) then
          npts = npts+1
          do ii = 1, ndim
            newpt(ii) = cooree(k,ii)
          end do
!      VERIF SI DEJA
          deja = .false.
          do ii = 1, npi
             do j = 1, ndim
                  p(j) = pinter(ndim*(ii-1)+j)
             end do
             if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                  deja = .true.
                  ni=ii
             endif
          end do
!      ON ARCHIVE LES NOEUDS SOMMETS DU TRIA TELS QUE LST<=0
          if (.not. deja) then
             npi = npi+1
             npis = npis+1
             do j = 1, ndim
                 pinter(ndim*(npi-1)+j) = newpt(j)
                 pinref(ndim*(npi-1)+j) = cooref(k,j)
             end do
             do j = 1, zxain-1
                ainter(zxain*(npi-1)+j) = rainter(k,j)
             end do
             noeud(npts) = npi
          else
             noeud(npts) = ni
          endif
       endif
    end do
    do ii = 1, nno
       tabls(ii) = zr(jlst-1+ii)
       do jj =  1, ndim
          geom((ii-1)*ndim+jj) = zr(igeom-1+ndim*(ii-1)+jj)
       end do
    end do
!
!      ON BOUCLE SUR LES ARETES DU TRIA POUR RECUPERER LES POINTS DU FOND DE
!      FISSURE SUR CES ARETES
    do k = 1, 3
       if (k.eq.1) then
          kk = 2
          kkk = 3
       else if (k.eq.2) then
          kk = 3
          kkk = 1
       else if (k.eq.3) then
          kk = 1
          kkk = 2
       endif
!      SI L'ARETE EST COUPE PAR LE FOND DE FISSURE
       if (lst(k)*lst(kk).lt.0.d0) then
          nintar = nintar+1
!      RECHERCHE DU FOND DE FISSURE SUR L'ARETE
          do jj = 1, ndim
             ptxx(jj) = cooref(k,jj)
             ptxx(jj+ndim) = cooref(kk,jj)
          end do
          ASSERT(abs(lst(k)-lst(kk)) .gt. 1.d0/r8gaem())
          if (.not.iselli(elp)) then
!      RECUPERATION DU NOEUD MILIEU DE L'ARETE
             do jj = 1, ndim
                ptxx(2*ndim+jj) = cooref(k+ndim,jj)
             end do
!      INITIALISATION DU NEWTON
             a = (lst(k) + lst(kk) - 2*lst(k+ndim))/2.d0
             b = (lst(kk) - lst(k))/2.d0
             c = lst(k+ndim)
             ASSERT(b**2.ge.(4*a*c))
             if (abs(a).lt.1.d-8) then
                ksi(1) = lst(k)/(lst(k)-lst(kk))
             else
                ksi(1) = (-b-sqrt(b**2-4*a*c))/(2.d0*a)
                if (abs(ksi(1)).gt.1) ksi(1) = (-b+sqrt(b**2-4*a*c))/(2.d0*a)
                ASSERT(abs(ksi(1)).le.1)
                ksi(1) = (ksi(1)+1)/2.d0
             endif
          else
             ksi(1) = lst(k)/(lst(k)-lst(kk))
             do jj = 1, ndim
                ptxx(2*ndim+jj) = (ptxx(jj)+ptxx(ndim+jj))/2.d0
             end do
          endif
          do ii = 1, 3
             n(ii) = ii
          end do
          epsmax = 1.d-8
          itemax = 100
!      ALGORITHME DE NEWTON POUR TROUVER LE FOND DE FISSURE
          call xnewto(elp, 'XINTER', n, ndim, ptxx, ndim,&
                      geom, tabls ,ibid, ibid, itemax, epsmax, ksi)
          call vecini(ndim, 0.d0, xref)
          do ii = 1, ndim
             xref(ii) = 2.d0*(1.d0-ksi(1))*(5.d-1-ksi(1))*ptxx(j)+4.d0*ksi(1)*&
                       (1.d0-ksi(1))*ptxx(j+2*ndim)+2.d0*ksi(1)*(ksi(1)-5.d-1)*&
                       ptxx(j+ndim)
          end do
          call vecini(27, 0.d0, ff)
          call elrfvf(elp, xref, nbnomx, ff, nno)
!      CALCUL DES COORDONNEES REELES DU FOND DE FISSURE
          call vecini(ndim, 0.d0, x)
          do ii = 1, ndim
             do j = 1, nno
                x(ii) = x(ii) + zr(igeom-1+ndim*(j-1)+ii) *ff(j)
             end do
          end do
!      VERIF SI DEJA
          deja = .false.
          do ii = 1, npi
             do j = 1, ndim
                  p(j) = pinter(ndim*(ii-1)+j)
             end do
             if (padist(ndim,p,x) .lt. (lonref*cridist)) then
                  deja = .true.
                  ni=ii
             endif
          end do
          if (.not. deja) then
             npi = npi+1
             npis = npis+1
             do j = 1, ndim
                 pinter(ndim*(npi-1)+j) = x(j)
                 pinref(ndim*(npi-1)+j) = xref(j)
             end do
             do j = 1, zxain-1
                ainter(zxain*(npi-1)+j) = 0.d0
             end do
             noeud(2+nintar) = npi
          else
             noeud(2+nintar) = ni
          endif
!      DANS LE CAS QUADRATIQUE ON CHERCHE LE POINT MILEU CORRESP0NDANT (SUR
!      l'ARETE INTERSECTEE)
          if (.not.iselli(elp)) then
             if (lst(k).lt.0.d0) then
                ksi(1) = ksi(1)/2.d0
             else
                ksi(1) = (1.d0+ksi(1))/2.d0
             endif
             do ii = 1, ndim
                miref(ii) = 2.d0*(1.d0-ksi(1))*(5.d-1-ksi(1))*ptxx(j)+4.d0*ksi(1)*&
                            (1.d0-ksi(1))*ptxx(j+2*ndim)+2.d0*ksi(1)*(ksi(1)-5.d-1)*&
                            ptxx(j+ndim)
             end do
             call elrfvf(elp, xref, nbnomx, ff, nno)
             call vecini(ndim, 0.d0, mifis)
             do ii = 1, ndim
                do j = 1, nno
                   mifis(ii) = mifis(ii) + zr(igeom-1+ndim*(j-1)+ii)*ff(j)
                end do
             end do
!      VERIF SI DEJA
             deja = .false.
             do ii = 1, npi
                do j = 1, ndim
                     p(j) = pinter(ndim*(ii-1)+j)
                end do
                if (padist(ndim,p,mifis) .lt. (lonref*cridist)) then
                     deja = .true.
                     ni=ii
                endif
             end do
             if (.not. deja) then
                npi = npi+1
                do j = 1, ndim
                    pinter(ndim*(npi-1)+j) = mifis(j)
                    pinref(ndim*(npi-1)+j) = miref(j)
                end do
                do j = 1, zxain-1
                   ainter(zxain*(npi-1)+j) = 0.d0
                end do
                noeud(4+nintar) = npi
             else
                noeud(4+nintar) = ni
             endif
          endif
!      DANS LE CAS QUADRATIQUE ON CHERCHE LES MILIEUX D'ARETES LST<=0
       else if (lst(k).le.0.d0.and.lst(kk).le.0.d0) then
          if (.not.iselli(elp)) then
             ASSERT(npts.eq.2)
             if (zi(jcnset-1+nnose*(i-1)+f(face,3+k)) .gt. 3000) then
                do j = 1, ndim
                     newpt(j) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+f(face,3+k))-3001)+j)
                end do
             else if (zi(jcnset-1+nnose*(i-1)+f(face,3+k)) .gt. 2000) then
                do j = 1, ndim
                     newpt(j) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+f(face,3+k))-2001)+j)
                end do
             else if (zi(jcnset-1+nnose*(i-1)+f(face,3+k)) .lt. 2000) then
                do j = 1, ndim
                     newpt(j) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+f(face,3+k))-1)+j)
                end do
             endif
             call reeref(elp, nno, zr(igeom), newpt, ndim, newptref, ff)
!      VERIF SI DEJA
             deja = .false.
             do ii = 1, npi
                do j = 1, ndim
                   p(j) = pinter(ndim*(ii-1)+j)
                end do
                if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                     deja = .true.
                     ni=ii
                endif
             end do
             if (.not. deja) then
                npi = npi+1
                do j = 1, ndim
                   pinter(ndim*(npi-1)+j) = newpt(j)
                   pinref(ndim*(npi-1)+j) = newptref(j)
                end do
                do j = 1, zxain-1
                   ainter(zxain*(npi-1)+j) = 0.d0
                end do
                noeud(7) = npi
             else
                noeud(7) = ni
             endif
          endif
       endif
    end do
!   ON DETERMINE MAINTENANT DANS LE CAS QUADRATIQUE LE NOEUD MILIEU 
!   ENTRE LES DEUX POINTS DU FOND DE FISSURE
!
!   ON DETERMINE D'ABORD UNE BASE ORTHONORMEE DU PLAN DANS LEQUEL ON VA FAIRE LA
!   RECHERCHE
    if (.not. iselli(elp)) then
       if (mipos.and.npts.eq.2) then
          do j = 1, ndim
             xref(j) = (pinref((noeud(3)-1)*ndim+j) +&
                        pinref((noeud(4)-1)*ndim+j))/2.d0
          end do
          call vecini(ndim, 0.d0, x)
          call elrfvf(elp, xref, nbnomx, ff, nno)
          do j = 1, ndim
             do ii = 1, nno
                x(j) = x(j) + zr(igeom-1+ndim*(ii-1)+j) * ff(ii)
             end do
          end do
       else
          if (nintar.eq.2) then
             do j = 1, ndim
                vectn(j) = pinref((noeud(4)-1)*ndim+j) - pinref((noeud(3)-1)*ndim+j)
                xref(j) = (pinref((noeud(4)-1)*ndim+j) +&
                           pinref((noeud(3)-1)*ndim+j))/2.d0
             end do
          elseif (lst(1).eq.0.d0) then
             do j = 1, ndim
                vectn(j) = pinref((noeud(3)-1)*ndim+j) - pinref((noeud(1)-1)*ndim+j)
                xref(j) = (pinref((noeud(3)-1)*ndim+j) +&
                           pinref((noeud(1)-1)*ndim+j))/2.d0
             end do
          else
             do j = 1, ndim
                vectn(j) = pinref((noeud(3)-1)*ndim+j) - pinref((noeud(2)-1)*ndim+j)
                xref(j) = (pinref((noeud(3)-1)*ndim+j) +&
                           pinref((noeud(2)-1)*ndim+j))/2.d0
             end do
          endif
          call vecini(ndim, 0.d0, u)
          if ((vectn(1).eq.0.d0).and.(vectn(2).eq.0.d0)) then
             u(1) = 1
             u(2) = 1
          else
             u(1) = vectn(2)
             u(2) = -vectn(1)
          endif
          call provec(vectn, u, v)
          call xnormv(ndim, u, norme)
          call xnormv(ndim, v, norme)
!   ON RECHERCHE DANS LE PLAN MEDIATEUR LE POINT DU FOND DE FISSURE
          do j= 1, ndim
             ptxx(j) = u(j)
             ptxx(ndim+j) = v(j)
          end do
          call xnewto(elp, 'XINTFA', n, ndim, ptxx, ndim,&
                      geom, tabls ,ibid, ibid, itemax, epsmax, xref)
          call elrfvf(elp, xref, nbnomx, ff, nno)
          call vecini(ndim, 0.d0, x)
          do ii = 1, ndim
             do j = 1, nno
                x(ii) = x(ii) + zr(igeom-1+ndim*(j-1)+ii) *ff(j)
             end do
          end do
       endif
!   ON ARCHIVE CE POINT
       npi = npi+1
       do j = 1, ndim
          pinter(ndim*(npi-1)+j) = x(j)
          pinref(ndim*(npi-1)+j) = xref(j)
       end do
       noeud(8) = npi
       do j = 1, zxain-1
          ainter(zxain*(npi-1)+j) = 0.d0
       end do
!   DANS LE CAS NPTS=2 ET NINTAR=2, IL NOUS RESTE ENCORE UN POINT MILIEU A
!   DETERMINER (DANS LE QUAD TEL QUE LST<=0)
       if (npts.eq.2.and.nintar.eq.2.) then
          if (lst(3).gt.0.d0) then
             num(1) = noeud(1)
             num(2) = noeud(2)
             num(3) = noeud(4)
             num(4) = noeud(3)
             num(5) = noeud(7)
             num(6) = noeud(6)
             num(7) = noeud(8)
             num(8) = noeud(5)
          else
             num(1) = noeud(1) 
             num(2) = noeud(2)
             num(3) = noeud(3)
             num(4) = noeud(4)
             num(5) = noeud(7)
             num(6) = noeud(5)
             num(7) = noeud(8)
             num(8) = noeud(6)
          endif
          call xcenfi(elp, ndim, ndim, geom, zr(jlsn),&
                      pinref, pinref, cenref, cenfi, num)
  !   ON ARCHIVE CE POINT
          npi = npi+1
          do j = 1, ndim
             pinter(ndim*(npi-1)+j) = cenfi(j)
             pinref(ndim*(npi-1)+j) = cenref(j)
          end do
          noeud(9) = npi 
          do j = 1, zxain-1
             ainter(zxain*(npi-1)+j) = 0.d0
          end do
       endif
    endif
!
!
end subroutine
