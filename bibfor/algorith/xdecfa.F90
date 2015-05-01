subroutine xdecfa(elp, nno, igeom, jlsn, jlst, npi,npis,&
                  pinter, pinref, ainter, jcnset, cooree, cooref, rainter,&
                  noeud, npts, nintar, lst ,lonref, ndim, zxain,&
                  jnit, i, face, nnose, jmilt, f, mipos)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elraca.h"
#include "asterfort/elrfvf.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcenfi.h"
#include "asterfort/xdivte.h"
#include "asterfort/xmifis.h"
#include "asterfort/xnewto.h"
#include "asterfort/xnormv.h"
!
    integer :: npi, noeud(9), i, nit, face, jmilt, f(6,8), npis
    integer :: igeom, jlsn, jlst, jcnset, zxain, jnit, nnose
    integer :: nintar, npts, ndim, nno
    real(kind=8) :: pinter(*), ainter(*), cooree(3,ndim), cooref(3,ndim)
    real(kind=8) :: rainter(3,4), lst(3), lonref, pinref(*)
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
!       NIT      : NOMBRE DE SOUS ELEMENTS
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
    real(kind=8) :: p(ndim), newpt(ndim), rbid, newptref(ndim), cenref(ndim)
    real(kind=8) :: norme, scal, geom(ndim*nno), ff(20), cenfi(ndim), tabls(40)
    real(kind=8) :: x(ndim), xref(ndim), miref(ndim), mifis(ndim), base(2*ndim)
    real(kind=8) :: u(ndim), v(ndim), det, vectn(ndim), ab(ndim), coor(nno*ndim)
    real(kind=8) :: n1(ndim), n2(ndim) , n3(ndim), ac(ndim), maxlsn, epsmax, cridist
    character(len=8) :: cbid(20)
    integer :: k, ii, jj, j, ni, kk, ibid, num(8), iibid(20), nbnomx
    integer :: cnsref(nnose), nsse, ip1, ip2, n(3), cnset(60), ind, kkk
    integer :: itemax, ntetra
    aster_logical :: deja , find
    parameter   (nbnomx = 27)
    parameter   (cridist=1.d-7)
!
! --------------------------------------------------------------------
!
    call jemarq()
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
!
!   IL FAUT RETROUVER LE SOUS TETRA DONT ON EST ISSU
!   ON RECUPERE EGALEMENT LE NOMBRE DE SOUS ELEMENTS ISSUS DE CE SOUS TETRA
    call xdivte(elp, cnset, nit, nnose)
    if (i.le.nit) then
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(i-1)+ii)
       end do
       nsse = zi(jnit-1+4+i)
    else if (i.lt.(nit+zi(jnit-1+4+1))) then
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(1-1)+ii)
       end do
       nsse = zi(jnit-1+4+1)
    else if (i.lt.(nit+zi(jnit-1+4+1)+zi(jnit-1+4+2)-1)) then
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(2-1)+ii)
       end do
       nsse = zi(jnit-1+4+2)
    else if (i.lt.(nit+zi(jnit-1+4+1)+zi(jnit-1+4+2)+zi(&
             jnit-1+4+3)-2)) then
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(3-1)+ii)
       end do
       nsse = zi(jnit-1+4+3)
    else if (i.lt.(nit+zi(jnit-1+4+1)+zi(jnit-1+4+2)+zi(&
             jnit-1+4+3)+zi(jnit-1+4+4)-3)) then
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(4-1)+ii)
       end do
       nsse = zi(jnit-1+4+4)
    else if (i.lt.(nit+zi(jnit-1+4+1)+zi(jnit-1+4+2)+zi(&
             jnit-1+4+3)+zi(jnit-1+4+4)+zi(jnit-1+4+5)-4)) then
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(5-1)+ii)
       end do
       nsse = zi(jnit-1+4+5)
    else
       do ii = 1, nnose
          cnsref(ii) = cnset(nnose*(6-1)+ii)
       end do
       nsse = zi(jnit-1+4+6)
    endif
!
!      ON CALCULE LE NOMBRE DE NOEUDS SOMMETS DU TETRA PARENT TELS QUE LSN=0
    ntetra= 0
    do ii = 1, 4
       if (zr(jlsn-1+cnsref(ii)).eq.0.d0) ntetra = ntetra + 1
    end do
!
!      ON RECUPERE LES COORDONNES DE REFERENCE DES NOEUDS DE L'ELEMENT PARENT
    call elraca(elp, ndim, nno, ibid, ibid, cbid, iibid, coor, rbid)
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
!      ON DETERMINE LES COORDONNES DE REFERENCE DES ARETES LSN=0
          do ii = 1, ndim
             ab(ii) = cooref(kk,ii)-cooref(k,ii)
             u(ii) = cooref(kk,ii)-cooref(k,ii)
          end do
          do ii = 1, ndim
             ac(ii) = cooref(kkk,ii)-cooref(k,ii)
          end do
!      ON CHERCHE UN PLAN DANS LEQUEL CHERCHER LE FOND DE FISSURE
          find = .false.
          if ((nsse.eq.1).or.(nsse.eq.6.and.ntetra.eq.1)&
              .or.(nsse.eq.4.and.ntetra.eq.2)) then
             maxlsn = 0.d0
             do ii = 1, 4
                if (abs(zr(jlsn-1+cnsref(ii))).ge.maxlsn) then
                  do j = 1, ndim
                     v(j) = coor((cnsref(ii)-1)*ndim+j) - cooref(k,j)
                  end do
                  maxlsn = abs(zr(jlsn-1+cnsref(ii)))
                endif
             end do
             find = .true.
          else
!         BOUCLE SUR LES FACES DU SOUS ELEMENT DONT ON EST ISSU
             j = 1
100          continue
             ASSERT(j.le.4)
!         ON DETERMINE LES COORDONNES DE REFERENCE DE DEUX ARETES DE
!         LA FACE
             if (j.eq.1) ind = 3
             if (j.eq.2) ind = 1
             if (j.eq.3) ind = 4
             if (j.eq.4) ind = 2
             do ii = 1, ndim
                u(ii) = coor((cnsref(j)-1)*ndim+ii)-coor((cnsref(ind)-1)*ndim+ii)
                v(ii) = coor((cnsref(5-j)-1)*ndim+ii)-coor((cnsref(ind)-1)*ndim+ii)
             end do
             det = 0.d0
             det = ab(1)*u(2)*v(3)+u(1)*v(2)*ab(3)+v(1)*ab(2)*u(3)-&
                   ab(3)*u(2)*v(1)-ab(2)*u(1)*v(3)-ab(1)*u(3)*v(2)
!             det = det**(1/3)
             call provec(u,v,n1)
             call provec(ab,ac,n2)
             call provec(n1,n2,n3)
             call xnormv(ndim, n3, norme)
             if ((abs(det).le.lonref*cridist).and.(norme.ge.cridist)) then
                find = .true.
             else if (j.lt.4) then
                j = j+1
                goto 100
             endif
          endif
          if (.not.find) then
             do ii = 1, ndim
                u(ii) = cooref(kk,ii)-cooref(k,ii)
             end do
             maxlsn = 0.d0
             do ii = 1, 4
                if (abs(zr(jlsn-1+cnsref(ii))).ge.maxlsn) then
                  do j = 1, ndim
                     v(j) = coor((cnsref(ii)-1)*ndim+j) - cooref(k,j)
                  end do
                  maxlsn = abs(zr(jlsn-1+cnsref(ii)))
                endif
             end do
          endif
!         ON ORTHONORMALISE LA BASE (U,V)
          call xnormv(ndim, u, norme)
          scal = 0.d0
          do ii = 1, ndim
              scal = scal + u(ii)*v(ii)
          end do
          do ii = 1, ndim
             v(ii) = v(ii) -scal*u(ii)
          end do
          call xnormv(ndim, v, norme)
!         POINT DE DEPART POUR LA RECHERCHE DU FOND DE FISSURE
          do ii = 1, ndim
            xref(ii) = (cooref(k,ii)+cooref(kk,ii))/2.d0 
            base(ii) = u(ii)
            base(ndim+ii) = v(ii)
          end do
          do ii = 1, nno
             do jj =  1, ndim
                geom((ii-1)*ndim+jj) = zr(igeom-1+ndim*(ii-1)+jj)
             end do
             tabls(2*ii-1) = zr(jlsn-1+ii)
             tabls(2*ii) = zr(jlst-1+ii)
          end do
          do ii = 1, 3
             n(ii) = ii
          end do
          epsmax = 1.d-8
          itemax = 100
!      ALGORITHME DE NEWTON POUR TROUVER LE FOND DE FISSURE
          call xnewto(elp, 'XINTFA', n, ndim, base, ndim,&
                      geom, tabls ,ibid, ibid, itemax, epsmax, xref)
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
             ip2 = noeud(2+nintar)
             if (npts.eq.1) then
                ip1 = noeud(1)
             else if (npts.eq.2) then
                if (lst(3).gt.1.d-6) then
                   ip1 = noeud(3-nintar)
                else 
                   ip1 = noeud(nintar)
                endif
             endif
             call xmifis(ndim, ndim, elp, geom, zr(jlsn), n, &
                         ip1, ip2, pinref, miref, mifis, u, v)
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
!   ON DETERMINE MAINTENAT DANS LE CAS QUADRATIQUE LE NOEUD MILIEU 
!   ENTRE LES DEUX POINTS DU FOND DE FISSURE
!
!   ON DETERMINE D'ABORD UNE BASE ORTHONORMEE DU PLAN DANS LEQUEL ON VA FAIRE LA
!   RECHERCHE
    if (.not. iselli(elp)) then
       if (mipos.and.npts.eq.2) then
          do j = 1, ndim
             xref(j) = (pinref((noeud(2)-1)*ndim+j) +&
                        pinref((noeud(1)-1)*ndim+j))/2.d0
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
          else
             do j = 1, ndim
                vectn(j) = pinref((noeud(3)-1)*ndim+j) - pinref((noeud(1)-1)*ndim+j)
                xref(j) = (pinref((noeud(3)-1)*ndim+j) +&
                           pinref((noeud(1)-1)*ndim+j))/2.d0
             end do
          endif
          call vecini(ndim, 0.d0, u)
          if ((vectn(1).eq.0.d0).and.(vectn(2).eq.0.d0)) then
             u(1) = 1
             u(2) = 1
          else
             u(1) = -vectn(2)
             u(2) = -vectn(1)
          endif
          call provec(vectn, u, v)
          call xnormv(ndim, u, norme)
          call xnormv(ndim, v, norme)
!   ON RECHERCHE DANS LE PLAN LE POINT DU FOND DE FISSURE
          do j= 1, ndim
             base(j) = u(j)
             base(ndim+j) = v(j)
          end do
          call xnewto(elp, 'XINTFA', n, ndim, base, ndim,&
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
          if (mipos) then
          do j = 1, ndim
             cenfi(j) = (pinter(ndim*(noeud(1)-1)+j)+pinter(ndim*(noeud(2)&
                         -1)+j)+pinter(ndim*(noeud(3)-1)+j)+pinter(ndim*&
                         (noeud(4)-1)+j))/4.d0 
             cenref(j) = (pinref(ndim*(noeud(1)-1)+j)+pinref(ndim*(noeud(2)&
                         -1)+j)+pinref(ndim*(noeud(3)-1)+j)+pinref(ndim*&
                         (noeud(4)-1)+j))/4.d0
          end do
          else
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
          endif
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
    call jedema()
!
end subroutine
