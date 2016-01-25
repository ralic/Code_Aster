subroutine xfacxt(elp, jpint, jmilt, jnit, jcnset, pinter,&
                  ninter, jphe, ndim, ainter,nface,nptf, cface,&
                  igeom, jlsn, jlst, jaint, jgrlsn)
! aslint: disable=W1501
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/elrfvf.h"
#include "asterfort/iselli.h"
#include "asterfort/loncar.h"
#include "asterfort/matini.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/tecael.h"
#include "asterfort/vecini.h"
#include "asterfort/xassfa.h"
#include "asterfort/xdecfa.h"
#include "asterfort/xnewto.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
!
    integer :: ninter, nface, cface(30, 6), jcnset, jnit, jmilt, jpint
    integer :: nptf, ndim, jphe, igeom, jlsn, jaint, jlst, jgrlsn
    real(kind=8) :: pinter(*), ainter(*)
    character(len=8) :: elp
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!                TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES,
!                ET LE PLAN DE FISSURE, DECOUPAGE EN FACETTES,
!                POINT MILIEU DE FISSURE DANS LE CAS QUADRATIQUE (2D ET 3D)
!                POUR UN ELEMENT CONTENANT LE FRONT DE FISSURE STRICTEMENT
!     ENTREE
!       ENR     : TYPE D'ENRICHISSEMENT POUR LA MAILLE COURANTE
!
!     SORTIE
!       PINTER  : COORDONNEES DES POINTS D'INTERSECTION
!       NINTER  : NOMBRE DE POINTS D'INTERSECTION AVEC LSN = 0
!       AINTER  : INFOS ARETE ASSOCIEE AU POINTS D'INTERSECTION
!       NFACE   : NOMBRE DE FACETTES
!       NPTF    : NOMBRE DE POINTS PAR FACETTE
!       CFACE   : CONNECTIVITE DES NOEUDS DES FACETTES
!
!     ----------------------------------------------------------------
!
    real(kind=8) :: minlsn, maxlsn, newpt(ndim), p(ndim), lonref, rainter(3,4)
    real(kind=8) :: ff(20), ptref(ndim), ptree(ndim), cooree(6,ndim), cooref(6,ndim)
    real(kind=8) :: maxlst, minlst, lst(6), m(ndim), miref(ndim), pinref(34*ndim), mref(ndim)
    real(kind=8) :: newptref(ndim), geom(20*ndim), ptxx(3*ndim), ksi(1)
    real(kind=8) :: ptreem(ndim), ptrefm(ndim), epsmax, ls(20), det, cridist, a, b, c
    real(kind=8) :: ab(ndim), bc(ndim), normfa(ndim), gradlsn(ndim), lsn(ndim)
    integer :: iadzi, iazk24, npi, ni, npis, ip1, ip2, n(3), nnose
    integer :: i, j, k, nelttot, ino, noeud(9), nintar, npts, h
    integer :: zxain, ar(12,3), ii, jj, nnos, nno, signe
    integer :: nbf, f(6,8), ibid2(12,3), ibid, nbnomx, itemax, tempo
    aster_logical :: deja, mipos
    character(len=8) :: typma, typsma
    parameter   (cridist=1.d-7)
    parameter   (nbnomx = 27)
!
! --------------------------------------------------------------------
!
!
    ASSERT(ndim.eq.2 .or. ndim .eq. 3)
!
    zxain = xxmmvd('ZXAIN')
!
!     RECUPERATION DES INFORMATIONS SUR LE MACRO-ELEMENT PARENT
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos)
!
!     INITIALISATION DU NOMBRE DE POINTS TELS QUE LSN=0
    ninter =0
!
!     INITIALISATION DU TYPE DE LA MAILLE ASSOCIEE AU SOUS ELEMENT
    typsma = '        '
!
!     RECHERCHE DE MINLSN, MAXLSN, MINLST, MAXLST
    minlsn=0.d0
    minlst=0.d0
    maxlsn=0.d0
    maxlst=0.d0
    do i = 1, nno
        minlsn=min(zr(jlsn-1+i),minlsn)
        maxlsn=max(zr(jlsn-1+i),maxlsn)
        minlst=min(zr(jlst-1+i),minlst)
        maxlst=max(zr(jlst-1+i),maxlst)
    end do
!     ON NE PREND QUE LES ELEMENTS AVEC MINLSN<0
    if (minlsn.ge.0.d0) goto 999
!     ON NE PREND PAS LES ELEMENTS QUI NE CONTIENNENT STRICTEMENT PAS LE FRONT DE FISSURE
    if (minlsn*maxlsn.gt.0.d0) goto 999
    if (minlst*maxlst.gt.0.d0) goto 999
!
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
!     CALCUL D'UNE LONGUEUR CARACTERISTIQUE DE L'ELEMENT
    call loncar(ndim, typma, zr(igeom), lonref)
!
!      INITIALISATION DU SIGNE POUR LA RECHERCHE DANS LES SOUS ELEMENTS
    signe = -1

!      NOMBRE TOTAL DE SOUS SOUS ELEMENTS
    nelttot = zi(jnit-1+1)
!      COMPTEUR DU NOMBRE D'ELEMENTS QUI CONSTITUENT LA LEVRE
    nface = 0
!      COMPTEUR DU NOMBRE DE POINTS DISTINCTS DE LA FISSURE
    npi = 0
!      COMPTEUR DU NOMBRE DE POINTS D'INTERSECTION AVEC LES ARETES DES SOUS ELEMENTS
    npis = 0
!
    if (ndim .eq. 2) then
       if (.not. iselli(elp)) then
           nnose = 6
           nptf = 3
           typsma = 'TRIA6   '
           call conare(typsma ,ar,nbf)
       else 
           nnose = 3
           nptf = 2
           typsma = 'TRIA3   '
           call conare(typsma ,ar,nbf)
       endif
!      BOUCLE SUR LES SOUS ELEMENTS
              do i = 1, nelttot
!      ON NE REGARDE QUE LES SOUS ELEMENTS AYANT HE=-1
                if (zi(jphe-1+i) .eq. signe) then
!      BOUCLE SUR LES ARETES (2D) OU FACES (3D) DU SOUS ELEMENT
                  do j = 1, nbf
!      COMPTEUR DE NOEUDS SOMMETS SUR LA LSN=0 PAR ARETE
                     h = 0
!      BOUCLE SUR SOMMETS DE l'ARETE (2D) OU DE LA FACE (3D)
                     do k = 1, 2
                        if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .gt. 1000) then
                           h = h+1
                        else if (zr(jlsn-1+zi(jcnset-1+nnose*(i-1)+ar(j,k))).eq.0.d0) then
                           h = h+1
                        else
                           goto 98
                        endif
                     end do
!      EN QUADRATIQUE ON VERIFIE QUE LE NOEUD MILIEU DE L'ARETE VERIFIE LSN=0,
!      SINON ON EXCLUE CETTE ARETE
                     if (.not.iselli(elp)) then
                        call vecini(ndim, 0.d0, lsn)
                        if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 3000) then
                           do ii = 1, ndim
                              newpt(ii)=zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+ar(j,3))-3001)+ii)
                           end do
                        else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 2000) then
                           do ii = 1, ndim
                              newpt(ii)=zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+ar(j,3))-2001)+ii)
                           end do
                        else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .lt. 2000) then
                           if (zr(jlsn-1+zi(jcnset-1+nnose*(i-1)+ar(j,3))).ne.0.d0) then
                              goto 98
                           else
                              goto 48
                           endif
                        endif
                        call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                    ptref, ff)
                        do ino = 1, nno
                           lsn(1)= lsn(1)+zr(jlsn-1+ino)*ff(ino)
                        end do
                        if (abs(lsn(1)).ge.lonref*cridist) goto 98
                     endif
48                   continue
!      SI LE NOMBRE DE NOEUDS SOMMETS DE L'ARETE QUI SONT SUR LA LSN EST 2
                     if (h .eq. 2) then
!      ON DOIT DETERMINER LST AUX EXTREMITES DE CE SEG
!      RECUPERATION DES COORDONNEES REELLES ET DE REFERENCE DES NOEUDS SITUES SUR CE SEGMENT
!      ET CALCUL DE LST AUX EXTREMITES DE CE SEGMENT
                        call vecini(6, 0.d0, lst)
                        call matini(3, 4, 0.d0, rainter)
                        call matini(6, ndim, 0.d0, cooree)
                        call matini(6, ndim, 0.d0, cooref)
                        minlst = 0.d0
                        maxlst = 0.d0
                        do k = 1, 2
                           if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .gt. 1000) then
                              do ii = 1, ndim
                                   cooree(k,ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                  ar(j,k))-1001)+ii)
                                   ptree(ii) = cooree(k,ii)
                              end do
                              do ii = 1, zxain-1
                                   rainter(k,ii) = zr(jaint-1+zxain*(zi(jcnset-1+nnose*(i-1)+&
                                                   ar(j,k))-1001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .lt. 1000) then
                              do ii = 1, ndim
                                   cooree(k,ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                  ar(j,k))-1)+ii)
                                   ptree(ii) = cooree(k,ii)
                              end do
                              rainter(k,1) = 0.d0
                              rainter(k,2) = zi(jcnset-1+nnose*(i-1)+ar(j,k))
                              rainter(k,3) = 0.d0
                              rainter(k,4) = 0.d0
                           endif
                           call reeref(elp, nno, zr(igeom), ptree, ndim, ptref, ff) 
                           do ii = 1, ndim
                              cooref(k,ii) = ptref(ii)
                           end do
                           do ino = 1, nno
                              lst(k) = lst(k)+ zr(jlst-1+ino)*ff(ino)
                           end do
                           if (abs(lst(k)).le.1.d-4) lst(k) = 0.d0
                           minlst= min(minlst,lst(k))
                           maxlst= max(maxlst,lst(k))
                        end do
!      SI MAXLST<=0
                        if (maxlst.le.0.d0) then
!      ON PREND CETTE ARETE EN ENTIER
                           nface = nface+1
                           do k = 1,2
                              do ii = 1, ndim
                                 newpt(ii) = cooree(k,ii)
                                 newptref(ii) = cooref(k,ii)
                              end do
                              deja = .false.
!      VERIF SI CE POINT A DEJA ETE STOCKE OU NON
                              do ii = 1, npi
                                 do jj = 1, ndim
                                      p(jj) = pinter(ndim*(ii-1)+jj)
                                 end do
                                 if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                                      deja = .true.
                                      ni=ii
                                 endif
                              end do
!      ON ARCHIVE PINTER ET CFACE
                              if (.not.deja) then
                                  npi = npi+1
                                  npis = npis+1
                                  do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = newpt(jj)
                                      pinref(ndim*(npi-1)+jj) = newptref(jj)
                                  end do
                                  cface(nface,k)=npi
                                  do jj = 1, zxain-1
                                     ainter(zxain*(npi-1)+jj) = rainter(k,jj)
                                  end do
                              else
                                  cface(nface,k)=ni
                              endif
                           end do
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
                           det = (pinter(ndim*(cface(nface,2)-1)+1)-pinter(ndim*(cface(nface,1)&
                                 -1)+1))*zr(jgrlsn-1+2) -(pinter(ndim*(cface(nface,2)-1)+2)-&
                                 pinter(ndim*(cface(nface,1)-1)+2))*zr(jgrlsn-1+1)
                           if (det.lt.0.d0) then
                              tempo = cface(nface,1)
                              cface(nface,1) = cface(nface,2)
                              cface(nface,2) = tempo
                           endif
!      DANS LE CAS QUADRATIQUE ON PREND EGALEMENT LE POINT MILIEU
                           if (.not. iselli(elp)) then
                              if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 3000) then
                                 do ii = 1, ndim
                                     newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                 ar(j,3))-3001)+ii)
                                 end do
                              else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 2000) then
                                 do ii = 1, ndim
                                     newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                 ar(j,3))-2001)+ii)
                                 end do
                              else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .lt. 2000) then
                                 do ii = 1, ndim
                                     newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                 ar(j,3))-1)+ii)
                                 end do
                              endif
!      ON ARCHIVE PINTER ET CFACE
                              npi = npi+1
                              call reeref(elp, nno, zr(igeom), newpt, ndim, ptref,ff)
                              do jj = 1, ndim
                                  pinter(ndim*(npi-1)+jj) = newpt(jj)
                                  pinref(ndim*(npi-1)+jj) = ptref(jj)
                              end do
                              cface(nface,3)=npi
                              do jj = 1, zxain-1
                                 ainter(zxain*(npi-1)+jj) = 0.d0
                              end do
                           endif
!      SI MINLST>=0
                        else if (minlst.ge.0.d0) then
!      ON NE PREND PAS CETTE ARETE
                           goto 98
                        else
!      C'EST LE CAS OU LE SEG EST TRAVERSE PAR LST STRICTEMENT
                           nface= nface+1
!      DETERMINATION DU POINT D'INTERSECTION AVEC LST
                           do ii = 1, nno
                              do jj =  1, ndim
                                 geom((ii-1)*ndim+jj) = zr(igeom-1+ndim*(ii-1)+jj)
                              end do
                              ls(ii) = zr(jlst-1+ii)
                           end do
                           do ii = 1, 3
                              n(ii) = ii
                           end do
                           do jj = 1, ndim
                              ptxx(jj) = cooref(1,jj)
                              ptxx(jj+ndim) = cooref(2,jj)
                           end do
                           ASSERT(abs(lst(1)-lst(2)) .gt. 1.d0/r8gaem())
                           if (.not.iselli(elp)) then
!      RECUPERATION DU NOEUD MILIEU DE L'ARETE
                              if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 3000) then
                                 do ii = 1, ndim
                                    newpt(ii)=zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                              ar(j,3))-3001)+ii)
                                 end do
                              else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 2000) then
                                 do ii = 1, ndim
                                    newpt(ii)=zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                              ar(j,3))-2001)+ii)
                                 end do
                              else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .lt. 2000) then
                                 do ii = 1, ndim
                                    newpt(ii)=zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                              ar(j,3))-1)+ii)
                                 end do
                              endif
                              call reeref(elp, nno, zr(igeom), newpt, ndim, ptref,ff)
                              do jj = 1, ndim
                                 ptxx(2*ndim+jj) = ptref(jj)
                              end do
                              do ino = 1, nno
                                 lst(3) = lst(3) + zr(jlst-1+ino)*ff(ino)
                              end do
!      INITIALISATION DU NEWTON
                              a = (lst(1) + lst(2) - 2*lst(3))/2.d0
                              b = (lst(2) - lst(1))/2.d0
                              c = lst(3)
                              ASSERT(b**2.ge.(4*a*c))
                              if (abs(a).lt.1.d-8) then
                                 ksi(1) = lst(1)/(lst(1)-lst(2))
                              else
                                 ksi(1) = (-b-sqrt(b**2-4*a*c))/(2.d0*a)
                                 if (abs(ksi(1)).gt.1) ksi(1) = (-b+sqrt(b**2-4*a*c))/(2.d0*a)
                                 ASSERT(abs(ksi(1)).le.1)
                                 ksi(1) = (ksi(1)+1)/2.d0
                              endif
                           else
                              ksi(1) = lst(1)/(lst(1)-lst(2))
                              do jj = 1, ndim
                                 ptxx(2*ndim+jj) = (ptxx(jj)+ptxx(ndim+jj))/2.d0
                              end do
                           endif
                           epsmax = 1.d-8
                           itemax = 100
                           call xnewto(elp, 'XINTER', n, ndim, ptxx, ndim,&
                                       zr(igeom), ls, ibid, ibid, itemax, epsmax, ksi)
                           call vecini(ndim, 0.d0, mref)
                           do ii = 1, ndim
                              mref(ii) = 2.d0*(1.d0-ksi(1))*(5.d-1-ksi(1))*ptxx(j)+4.d0*ksi(1)*&
                                        (1.d0-ksi(1))*ptxx(j+2*ndim)+2.d0*ksi(1)*(ksi(1)-5.d-1)*&
                                        ptxx(j+ndim)
                           end do
                           call elrfvf(elp, mref, nbnomx, ff, nno)
                           call vecini(ndim, 0.d0, m)
                           do ii = 1, ndim
                              do ino = 1, nno
                                 m(ii) = m(ii) + zr(igeom-1+ndim*(ino-1)+ii) * ff(k)
                              end do
                           end do
                           npi = npi+1
                           npis = npis+1
                           ip2 = npi
!      ON ARCHIVE LES COORDONNES REELLES ET DE REFERENCE DU FOND DE FISSURE
                           do jj = 1, ndim
                               pinter(ndim*(npi-1)+jj) = m(jj)
                               pinref(ndim*(npi-1)+jj) = mref(jj)
                           end do
                           do jj = 1, zxain-1
                              ainter(zxain*(npi-1)+jj) = 0.d0
                           end do
!      ON DISTINGUE ENSUITE DEUX CAS EN FONCTION DU SIGNE DE LST A LA PREMIERE
!      EXTREMITE DU SEG
                           if (lst(1).lt.0.d0) then
!      VERIF SI DEJA
                              deja = .false.
                              do ii = 1, npi
                                 do jj = 1, ndim
                                      p(jj) = pinter(ndim*(ii-1)+jj)
                                      newpt(jj) = cooree(1,jj)
                                      newptref(jj) = cooref(1,jj)
                                 end do
                                 if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                                      deja = .true.
                                      ni=ii
                                      ip1 = ni
                                 endif
                              end do
                              if (.not.deja) then
                                  npi = npi+1
                                  ip1 = npi
                                  npis = npis+1
                                  do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = newpt(jj)
                                      pinref(ndim*(npi-1)+jj) = newptref(jj)
                                  end do
                                  do jj = 1, zxain-1
                                     ainter(zxain*(npi-1)+jj) = rainter(1,jj)
                                  end do
                              endif
                              cface(nface,1) = ip1
                              cface(nface,2) = ip2
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
                              det = (pinter(ndim*(cface(nface,2)-1)+1)-pinter(ndim*(cface(nface,1)&
                                    -1)+1))*zr(jgrlsn-1+2)-(pinter(ndim*(cface(nface,2)-1)+2)-&
                                    pinter(ndim*(cface(nface,1)-1)+2))*zr(jgrlsn-1+1)
                              if (det.lt.0.d0) then
                                 tempo = cface(nface,1)
                                 cface(nface,1) = cface(nface,2)
                                 cface(nface,2) = tempo
                              endif
!      DANS LE CAS QUADRATIQUE
                              if (.not.iselli(elp)) then
!      RECHERCHE DU POINT MILIEU
                                 ksi(1) = ksi(1)/2.d0
                                 do ii = 1, ndim
                                    mref(ii) = 2.d0*(1.d0-ksi(1))*(5.d-1-ksi(1))*ptxx(j)+&
                                               4.d0*ksi(1)*(1.d0-ksi(1))*ptxx(j+2*ndim)+&
                                               2.d0*ksi(1)*(ksi(1)-5.d-1)*ptxx(j+ndim)
                                 end do
                                 call elrfvf(elp, mref, nbnomx, ff, nno)
                                 call vecini(ndim, 0.d0, m)
                                 do ii = 1, ndim
                                    do ino = 1, nno
                                       m(ii) = m(ii) + zr(igeom-1+ndim*(ino-1)+ii) * ff(k)
                                    end do
                                 end do
!      ON ARCHIVE POUR LE POINT MILIEU
                                 npi = npi+1
                                 do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = m(jj)
                                      pinref(ndim*(npi-1)+jj) = miref(jj)
                                 end do
                                 cface(nface,3)=npi
                                 do jj = 1, zxain-1
                                    ainter(zxain*(npi-1)+jj) = 0.d0
                                 end do
                              end if
                           else if (lst(2).lt.0.d0) then
!      VERIF SI DEJA
                              deja = .false.
                              do ii = 1, npi
                                 do jj = 1, ndim
                                      p(jj) = pinter(ndim*(ii-1)+jj)
                                      newpt(jj) = cooree(2,jj)
                                      newptref(jj) = cooref(2,jj)
                                 end do
                                 if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                                      deja = .true.
                                      ni=ii
                                      ip1 = ni
                                 endif
                              end do
                              if (.not.deja) then
                                  npi = npi+1
                                  ip1 = npi
                                  npis = npis+1
                                  do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = newpt(jj)
                                      pinref(ndim*(npi-1)+jj) = newptref(jj)
                                  end do
                                  do jj = 1, zxain-1
                                     ainter(zxain*(npi-1)+jj) = rainter(2,jj)
                                  end do
                              endif
                              cface(nface,1) = ip2
                              cface(nface,2) = ip1
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
                              det = (pinter(ndim*(cface(nface,2)-1)+1)-pinter(ndim*(cface(nface,1)&
                                    -1)+1))*zr(jgrlsn-1+2)-(pinter(ndim*(cface(nface,2)-1)+2)-&
                                    pinter(ndim*(cface(nface,1)-1)+2))*zr(jgrlsn-1+1)
                              if (det.lt.0.d0) then
                                 tempo = cface(nface,1)
                                 cface(nface,1) = cface(nface,2)
                                 cface(nface,2) = tempo
                              endif
!      DANS LE CAS QUADRATIQUE
                              if (.not.iselli(elp)) then
!      RECHERCHE DU POINT MILIEU
                                 ksi(1) = (1.d0+ksi(1))/2.d0
                                 do ii = 1, ndim
                                    mref(ii) = 2.d0*(1.d0-ksi(1))*(5.d-1-ksi(1))*ptxx(j)+&
                                               4.d0*ksi(1)*(1.d0-ksi(1))*ptxx(j+2*ndim)+2.d0*&
                                               ksi(1)*(ksi(1)-5.d-1)*ptxx(j+ndim)
                                 end do
                                 call elrfvf(elp, mref, nbnomx, ff, nno)
                                 call vecini(ndim, 0.d0, m)
                                 do ii = 1, ndim
                                    do ino = 1, nno
                                       m(ii) = m(ii) + zr(igeom-1+ndim*(ino-1)+ii) * ff(k)
                                    end do
                                 end do
!      ON ARCHIVE POUR LE POINT MILIEU
                                 npi = npi+1
                                 do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = m(jj)
                                      pinref(ndim*(npi-1)+jj) = miref(jj)
                                 end do
                                 cface(nface,3)=npi
                                 do jj = 1, zxain-1
                                    ainter(zxain*(npi-1)+jj) = 0.d0
                                 end do
                              endif
                           endif
                        endif
                     endif
98                  continue
                  end do
               endif
            end do
    else 
       if (.not. iselli(elp)) then
           nnose = 10
           nptf = 6
           typsma = 'TETRA10'
           call confac(typsma,ibid2, ibid, f,nbf, quad='OUI')
       else
           nnose = 4
           nptf = 3
           typsma = 'TETRA4'
           call confac(typsma,ibid2, ibid, f,nbf)
       endif
!      BOUCLE SUR LES SOUS ELEMENTS
              do i = 1, nelttot
!      ON NE REGARDE QUE LES SOUS ELEMENTS AYANT HE=-1
                if (zi(jphe-1+i) .eq. signe) then
!      BOUCLE SUR LES ARETES (2D) OU FACES (3D) DU SOUS ELEMENT
                  do j = 1, nbf
!      COMPTEUR DE NOEUDS SOMMETS SUR LA LSN=0 PAR FACE
                     h = 0
!      BOUCLE SUR SOMMETS DE l'ARETE (2D) OU DE LA FACE (3D)
                     do k = 1, 3
                        if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 1000) then
                           h = h+1
                        else if (zr(jlsn-1+zi(jcnset-1+nnose*(i-1)+f(j,k))).eq.0.d0) then
                           h = h+1
                        else 
                           goto 99
                        endif
                     end do
!      EN QUADRATIQUE ON VERIFIE QUE LES NOEUDS MILIEU DU TRIA VERIFIENT LSN=0,
!      SINON ON EXCLUE CE TRIA
                     if (.not.iselli(elp)) then
                        call vecini(ndim, 0.d0, lsn)
                        do k = 1, 3
                           if (zi(jcnset-1+nnose*(i-1)+f(j,3+k)) .gt. 3000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                              f(j,3+k))-3001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+f(j,3+k)) .gt. 2000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                              f(j,3+k))-2001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+f(j,3+k)) .lt. 2000) then
                              if(zr(jlsn-1+zi(jcnset-1+nnose*(i-1)+f(j,3+k))).ne.0.d0) then
                                 goto 99
                              else
                                 goto 49
                              endif
                           endif
                           call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                       ptref, ff)
                           do ino = 1, nno
                              lsn(3)=lsn(3)+zr(jlsn-1+ino)*ff(ino)
                           end do
                           if (abs(lsn(k)).ge.lonref*cridist) goto 99
                        end do
                     endif
49                   continue
!      SI LE NOMBRE DE NOEUDS SOMMETS DE LA FACE QUI SONT SUR LA LSN EST 3
                     if (h .eq. 3) then
!      ON DOIT DETERMINER LST AUX EXTREMITES DE CE TRIA
!      RECUPERATION DES COORDONNEES REELLES ET DE REFERENCE DES NOEUDS SITUES SUR CE TRIA
                        call vecini(6, 0.d0, lst)
                        call matini(3, 4, 0.d0, rainter)
                        call matini(6, ndim, 0.d0, cooree)
                        call matini(6, ndim, 0.d0, cooref)
                        minlst = 0.d0
                        maxlst = 0.d0
                        do k = 1, 3
                           if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 1000) then
                              do ii = 1, ndim
                                   cooree(k,ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                  f(j,k))-1001)+ii)
                                   ptree(ii) = cooree(k,ii)
                              end do
                              do ii = 1, zxain-1
                                   rainter(k,ii) = zr(jaint-1+zxain*(zi(jcnset-1+nnose*(i-1)+&
                                                   f(j,k))-1001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .lt. 1000) then
                              do ii = 1, ndim
                                   cooree(k,ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                  f(j,k))-1)+ii)
                                   ptree(ii) = cooree(k,ii)
                              end do
                              rainter(k,1) = 0.d0
                              rainter(k,2) = zi(jcnset-1+nnose*(i-1)+f(j,k))
                              rainter(k,3) = 0.d0
                              rainter(k,4) = 0.d0
                           endif
                           call reeref(elp, nno, zr(igeom), ptree, ndim, ptref, ff)
                           do ii = 1, ndim
                              cooref(k,ii) = ptref(ii)
                           end do
                           do ino = 1, nno
                              lst(k) = lst(k)+ zr(jlst-1+ino)*ff(ino)
                           end do
                           if (abs(lst(k)).le.1.d-4) lst(k) = 0.d0
                           minlst= min(minlst,lst(k))
                           maxlst= max(maxlst,lst(k))
                        end do
!      DANS LE CAS QUADRATIQUE ON DOIT IDENTIFIER LES SITUATIONS OU LE SOUS
!      DECOUPAGE RISQUE DE DONNER DES ELEMENTS DISTORDUS
                        mipos = .true.
                        if (.not.iselli(elp)) then
                           do k = 4, 6
                              if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 3000) then
                                 do ii = 1, ndim
                                      ptreem(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                   f(j,k))-3001)+ii)
                                 end do
                              else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 2000) then
                                 do ii = 1, ndim
                                      ptreem(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                   f(j,k))-2001)+ii)
                                 end do
                              else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .lt. 2000) then
                                 do ii = 1, ndim
                                      ptreem(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                   f(j,k))-1)+ii)
                                 end do
                              endif
                              call reeref(elp, nno, zr(igeom), ptreem, ndim, ptrefm, ff)
                              do ii = 1, ndim
                                 cooree(k,ii) = ptreem(ii)
                                 cooref(k,ii) = ptrefm(ii)
                              end do
                              do ino = 1, nno
                                 lst(k) = lst(k)+ zr(jlst-1+ino)*ff(ino)
                              end do
                              if (lst(k).lt.1.d-4) mipos = .false.
                           end do
                        endif
!      SI MAXLST<=0
                        if (maxlst.le.0.d0) then
!      ON PREND CETTE FACE EN ENTIER
                           nface = nface+1
                           do k = 1, 3
                              do ii = 1, ndim
                                 newpt(ii) = cooree(k,ii)
                                 newptref(ii) = cooref(k,ii)
                              end do
                              deja = .false.
!      VERIF SI CE POINT A DEJA ETE STOCKE OU NON
                              do ii = 1, npi
                                 do jj = 1, ndim
                                      p(jj) = pinter(ndim*(ii-1)+jj)
                                 end do
                                 if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                                      deja = .true.
                                      ni=ii
                                 endif
                              end do
!      ON ARCHIVE PINTER ET CFACE
                              if (.not.deja) then
                                  npi = npi+1
                                  npis = npis+1
                                  do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = newpt(jj)
                                      pinref(ndim*(npi-1)+jj) = newptref(jj)
                                  end do
                                  cface(nface,k)=npi
                                  do jj = 1, zxain-1
                                     ainter(zxain*(npi-1)+jj) = rainter(k,jj)
                                  end do
                              else
                                  cface(nface,k)=ni
                              endif
                           end do
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
                           do jj = 1, ndim
                              ab(jj) = pinter(ndim*(cface(nface,2)-1)+jj) -&
                                       pinter(ndim*(cface(nface,1)-1)+jj)
                              bc(jj) = pinter(ndim*(cface(nface,3)-1)+jj) -&
                                       pinter(ndim*(cface(nface,2)-1)+jj)
                              gradlsn(jj) = zr(jgrlsn-1+jj)
                              normfa(jj) = 0.d0
                           end do
                           call provec(ab,bc,normfa)
                           det = ddot(ndim, gradlsn, 1, normfa, 1)
                           if (det.lt.0.d0) then
                              tempo = cface(nface,2)
                              cface(nface,2) = cface(nface,3)
                              cface(nface,3) = tempo
                              if (.not. iselli(elp)) then
                                 tempo = cface(nface,4)
                                 cface(nface,4) = cface(nface,6)
                                 cface(nface,6) = tempo
                              endif
                           endif
!      DANS LE CAS QUADRATIQUE ON PREND EGALEMENT LES POINTS MILIEUX
                           if (.not. iselli(elp)) then
                              do k = 4, 6
                                 deja = .false.
                                 if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 3000) then
                                    do ii = 1, ndim
                                         newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                     f(j,k))-3001)+ii)
                                    end do
                                 else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 2000) then
                                    do ii = 1, ndim
                                         newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                     f(j,k))-2001)+ii)
                                    end do
                                 else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .lt. 2000) then
                                    do ii = 1, ndim
                                         newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                                     f(j,k))-1)+ii)
                                    end do
                                 endif
!      VERIF SI DEJA
                                 do ii = 1, npi
                                    do jj = 1, ndim
                                         p(jj) = pinter(ndim*(ii-1)+jj)
                                    end do
                                    if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                                         deja = .true.
                                         ni=ii
                                    endif
                                 end do
!      ON ARCHIVE PINTER ET CFACE
                                 if (.not. deja) then
                                    npi = npi+1
                                    call reeref(elp, nno, zr(igeom), newpt, ndim, ptref,ff)
                                    do jj = 1, ndim
                                        pinter(ndim*(npi-1)+jj) = newpt(jj)
                                        pinref(ndim*(npi-1)+jj) = ptref(jj)
                                    end do
                                    cface(nface,k)=npi
                                    do jj = 1, zxain-1
                                       ainter(zxain*(npi-1)+jj) = 0.d0
                                    end do
                                 else
                                    cface(nface,k) = ni
                                 endif
                              end do
                           endif
!      SI MINLST>=0
                        else if (minlst.ge.0.d0) then
!      ON NE PREND PAS CETTE FACE
                           goto 99
                        else
!      C'EST LE CAS OU LA FACE EST TRAVERSEE PAR LST STRICTEMENT
                           call xdecfa(elp, nno, igeom, jlsn, jlst, npi,npis,&
                                       pinter, pinref, ainter, jcnset, cooree, cooref, rainter,&
                                       noeud, npts, nintar, lst ,lonref, ndim, zxain,&
                                       i, j , nnose, jmilt, f, mipos)
                           call xassfa(elp, npts, nintar, lst, noeud, cface, nface, pinter, jgrlsn)
                        endif
                     endif
99                   continue
                  end do
               endif
            end do
    endif
!
    ninter = npi
!
999  continue
!
!
end subroutine
