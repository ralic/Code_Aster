subroutine xfacxh(elp, jpint, jmilt, jnit, jcnset, pinter,&
                  ninter, jphe, ndim, ainter, nface, nptf,&
                  cface, igeom, jlsn, jaint, jgrlsn, nfiss,&
                  ifiss, fisc, nfisc, nfisc2, ncompe, jstano)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/loncar.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xnormv.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
!
    integer :: ninter, nface, cface(30, 6), jcnset, jnit, jmilt, jpint
    integer :: nptf, ndim, jphe, igeom, jlsn, jaint, jgrlsn
    integer :: nfiss, ifiss, fisc(*), nfisc, nfisc2, ncompe, jstano
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
!                POUR UN ELEMENT COMPLETEMENT TRAVERSE PAR LA FISSURE
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
    real(kind=8) :: lsnabs, minlsn, newpt(ndim), p(ndim), lonref, rainter(4), lsninter(20)
    real(kind=8) :: maxlsn, det, ab(ndim), bc(ndim), normfa(ndim), gradlsn(ndim)
    real(kind=8) :: ptref(ndim), ff(20), lsn(ndim+1), cridist, crijonc
    integer :: iadzi, iazk24, npi, ni, npis, ino
    integer :: i, j, k, nelttot, h, nnose, signe, ifisc, intersec
    integer :: zxain, ar(12,3), nbar, ii, jj, nnos, nno, nbinter
    integer :: nbf, f(6,8), ibid2(12,3), ibid, tempo, inc
    aster_logical :: cut, arete, deja, jonc
    parameter(cridist=1.d-7)
    parameter(crijonc=1.d-2)
    character(len=8) :: typma, typsma
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
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
!     INITIALISATION DU NOMBRE DE POINTS TELS QUE LSN=0
    ninter = 0
!
!     INITIALISATION DU TYPE DE LA MAILLE ASSOCIEE AU SOUS ELEMENT
    typsma = '        '
!
!     CALCUL D'UNE LONGUEUR CARACTERISTIQUE DE L'ELEMENT
    call loncar(ndim, typma, zr(igeom), lonref)
    inc = 0
!
!     L'ELEMENT EST-IL TRAVERSE STRICTEMENT PAR LSN=0?
    cut=.false.
    i=1
 1  continue
!     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
    if (zr(jlsn-1+(i-1)*nfiss+ifiss) .ne. 0.d0 .and. i .lt. nno) then
        do 30 k = i+1, nno
!     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
            if (zr(jlsn-1+(i-1)*nfiss+ifiss)*zr(jlsn-1+(k-1)*nfiss+ifiss) .lt. 0.d0) cut=.true.
30      continue
    else if (i.lt.nno) then
        i=i+1
        goto 1
    endif
!     RECHERCHE DE MINLSN
    minlsn = 0.d0
    maxlsn = 0.d0
    do i = 1, nnos
        minlsn=min(zr(jlsn-1+(i-1)*nfiss+ifiss),minlsn)
        maxlsn=max(zr(jlsn-1+(i-1)*nfiss+ifiss),maxlsn)
    end do
!     ON NE PREND QUE CERTAINS ELEMENTS POUR NE PAS AVOIR DE "DOUBLONS"
    arete = .false.
    lsnabs = 0.d0
    if(.not.cut) then
       if (ndim.eq.3) then
           call confac(typma, ibid2, ibid, f, nbf)
           do i = 1, nbf
                lsnabs = 0.d0
                do j = 1, 4
                  if (f(i,j).ne.0.d0) lsnabs = lsnabs+abs(zr(jlsn-1+(f(i,j)-1)*nfiss+ifiss))
                end do
                if (lsnabs.le.cridist*lonref) arete = .true. 
           end do
       else if (ndim.eq.2) then
           call conare(typma, ar, nbar)
           do i = 1, nbar
               lsnabs = 0.d0
               lsnabs = abs(zr(jlsn-1+(ar(i,1)-1)*nfiss+ifiss))+&
                        abs(zr(jlsn-1+(ar(i,2)-1)*nfiss+ifiss))
               if (lsnabs.le.cridist*lonref) arete = .true. 
           end do
       endif
       if (.not.arete) goto 999
       if (arete.and.minlsn.ge.0.d0) goto 999
    endif      
!
!      INITIALISATION DU SIGNE POUR LA RECHERCHE DANS LES SOUS ELEMENTS
    signe = -1
!
    if (nfisc2.ge.1) then
!      Y-A-T-IL UNE FISSURE BRANCHEE SUR IFISS DANS L'ELEMENT?
!      SI OUI, ON DOIT ADAPTER LE COTE DE LA FISSURE SUR LEQUEL ON CHERCHE LES
!      FACETTES DE CONTACT DE MANIERE A CE QUE LES FACETTES SOIENT CONFORMES A
!      LA JONCTION
       nbinter = 0
       intersec = 0
       do ifisc = 1, nfisc2
!      ON RECUPERE LA VALEUR DE LA LEVEL SET AUX NOEUDS DE L'ELEMENT
          call vecini(20, 0.d0, lsninter)
          do i = 1, nno
             lsninter(i) = zr(jlsn-1+(i-1)*nfiss+fisc(2*(ifisc+nfisc)-1))
          end do
          i = 1
          jonc = .false.
10        continue
!     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
          if (lsninter(i) .ne. 0.d0 .and. i .lt. nno) then
            do k = i+1, nno
!     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
               if (lsninter(i)*lsninter(k).lt. 0.d0) then
                  jonc = .true.
!     LA FISSURE EN QUESTION DOIT ETRE VUE PAR TOUS LES NEOUDS DE L'ELEMENT
                  do j = 1, nno
                     if (zi(jstano-1+(j-1)*nfiss+fisc(2*(ifisc+nfisc)-1)).eq.0) jonc= .false.
                  end do
                  if (jonc) then
                     nbinter = nbinter+1
                     intersec = ifisc
                  endif
                  exit
               endif
            end do
          else if (i.lt.nno) then
             i=i+1
             goto 10
          endif
       end do
       if (nbinter.ge.2) call utmess('A', 'XFEM_54')
       if (nbinter.ge.1) then
          if (fisc(2*(intersec+nfisc)).lt.0) signe = 1
          if (fisc(2*(intersec+nfisc)).gt.0) signe = -1
       endif
    endif
!      NOMBRE TOTAL DE SOUS SOUS ELEMENTS
    nelttot = zi(jnit-1+1)
!      COMPTEUR DU NOMBRE D'ELEMENTS QUI CONSTITUENT LA LEVRE
    nface = 0
!      COMPTEUR DU NOMBRE DE POINTS DISTINCTS DE LA FISSURE (PTS MILIEUX COMPRIS)
    npi = 0
!      COMPTEUR DU NOMBRE DE POINTS D'INTERSECTION AVEC LES ARETES DES SOUS ELEMENTS
    npis = 0
!
    if (ndim .eq. 2) then
       if (.not. iselli(elp)) then
           nnose = 6
           nptf = 3
           typsma = 'TRIA6   '
           call conare(typsma,ar,nbf)
       else 
           nnose = 3
           nptf = 2
           typsma = 'TRIA3   '
           call conare(typsma,ar,nbf)
       endif
!      BOUCLE SUR LES SOUS ELEMENTS
              do i = 1, nelttot
!      ON NE REGARDE QUE LES SOUS ELEMENTS AYANT HE=-1
                if (zi(jphe-1+(ifiss-1)*ncompe+i) .eq. signe) then
!      BOUCLE SUR LES ARETES (2D) OU FACES (3D) DU SOUS ELEMENT
                  do j = 1, nbf
!      COMPTEUR DE NOEUDS SOMMETS SUR LA LSN=0 PAR ARETE
                     h = 0
!      BOUCLE SUR SOMMETS DE l'ARETE (2D) OU DE LA FACE (3D)
                     do k = 1, 2
                        if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .gt. 1000) then
                           h = h+1
                        else if (zr(jlsn-1+(zi(jcnset-1+nnose*(i-1)+ar(j,k))&
                                 -1)*nfiss+ifiss).eq.0.d0 ) then
                           h = h+1
                        else
                          goto 22
                        endif
                     end do
!      TEST SPECIFIQUE POUR LES ELEMENTS MULTI-HEAVISIDE
                     if (nfiss.gt.1) then
                           call vecini(ndim+1, 0.d0, lsn)
                        do k = 1, 2
                           call vecini(ndim, 0.d0, newpt)
                           if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .gt. 1000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*&
                                              (i-1)+ar(j,k))-1001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .lt. 1000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*&
                                              (i-1)+ar(j,k))-1)+ii)
                              end do
                           endif
                           call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                       ptref, ff)
                           do ino = 1, nno
                              lsn(k)= lsn(k) + zr(jlsn-1+(ino-1)*nfiss+ifiss)*ff(ino)
                           end do
!      ON AJUSTE LES POINTS DE JONCTION DE FISSURE A ZERO
                           if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .gt. 1000) then
                              if (zr(jaint-1+zxain*(zi(jcnset-1+nnose*(i-1)+&
                                  ar(j,k))-1001)+4) .eq. -1.d0) then
                                 if (abs(lsn(k)).le.(lonref*crijonc)) lsn(k) = 0.d0
                              endif
                           endif
                        end do
                        if ((abs(lsn(1))+abs(lsn(2))).ge.(lonref*cridist)) goto 22
!      TEST SUPPLEMENTAIRE POUR LES ELEMENTS TRES ALONGES A PROXIMITE DE LA FISSURE
                        if (zi(jcnset-1+nnose*(i-1)+6-ar(j,1)-ar(j,2)) .gt. 1000) then
                           call vecini(ndim, 0.d0, newpt)
                           do ii = 1, ndim
                               newpt(ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*&
                                           (i-1)+6-ar(j,1)-ar(j,2))-1001)+ii)
                           end do
                           call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                       ptref, ff)
                           do ino = 1, nno
                              lsn(3)= lsn(3) + zr(jlsn-1+(ino-1)*nfiss+ifiss)*ff(ino)
                           end do
                           if (abs(lsn(3)).lt.max(abs(lsn(1)),abs(lsn(2)))) goto 22
                        endif
!      EN QUADRATIQUE ON VERIFIE QUE LE NOEUD MILIEU DE L'ARETE VERIFIE LSN=0,
!      SINON ON EXCLUE CETTE ARETE
                     elseif (.not.iselli(elp)) then
                        call vecini(ndim+1, 0.d0, lsn)
                        call vecini(ndim, 0.d0, newpt)
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
                              goto 22
                           else
                              goto 48
                           endif
                        endif
                        call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                    ptref, ff)
                        do ino = 1, nno
                           lsn(1)= lsn(1)+zr(jlsn-1+ino)*ff(ino)
                        end do
                        if (abs(lsn(1)).ge.lonref*cridist) goto 22
                     endif
48                   continue
!      SI LE NOMBRE DE NOEUDS SOMMETS DE L'ARETE QUI SONT SUR LA LSN EST 2
                     if (h .eq. 2) then
!      ON AJOUTE CETTE ARETE
                        nface = nface+1
!      RECUPERATION DES COORDONNEES REELLES DES NOEUDS SITUES SUR CE SEGMENT ET ARCHIVAGE
!      SI NON DEJA ARCHIVE
                        do k = 1, 2
                           if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .gt. 1000) then
                              do ii = 1, ndim
                                   newpt(ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                               ar(j,k))-1001)+ii)
                              end do
                              do ii = 1, zxain-1
                                   rainter(ii) = zr(jaint-1+zxain*(zi(jcnset-1+nnose*(i-1)+&
                                                 ar(j,k))-1001)+ii)
                              end do
!                              if (rainter(1) .eq. 0.d0 .and. rainter(2) .eq. 0.d0 &
!                                  .and. ifiss .gt. 1) then
!                                 do ii = 1, nbar
!                                    do jj = 1, ndim
!                                       ab(jj) = zr(igeom-1+ndim*(ar(ii,1)-1)+jj) - zr(igeom&
!                                                   -1+ndim*(ar(ii,2)-1)+jj)
!                                       bc(jj) = zr(igeom-1+ndim*(ar(ii,1)-1)+jj) - newpt(jj)
!                                    end do
!                                    det = ab(1)*bc(2)-ab(2)*bc(1)
!                                    if (abs(det) .le. cridist*lonref) then
!                                       rainter(1) = ii
!                                    else
!                                       inc = inc - 1
!                                       rainter(1) = inc
!                                    endif
!                                 end do
!                              endif
                           else if (zi(jcnset-1+nnose*(i-1)+ar(j,k)) .lt. 1000) then
                              do ii = 1, ndim
                                   newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                               ar(j,k))-1)+ii)
                              end do
                              rainter(1) = 0.d0
                              rainter(2) = zi(jcnset-1+nnose*(i-1)+ar(j,k))
                              rainter(3) = 0.d0
                              rainter(4) = 0.d0
                           endif
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
                               end do
                               cface(nface,k)=npi
                               do jj = 1, zxain-1
                                  ainter(zxain*(npi-1)+jj) = rainter(jj)
                               end do
                           else
                               cface(nface,k)=ni
                           endif
                        end do
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
                        det = (pinter(ndim*(cface(nface,2)-1)+1)-pinter(ndim*(cface(nface,1)-1)+&
                              1))*zr(jgrlsn-1+ndim*(ifiss-1)+2) - (pinter(ndim*(cface(nface,2)-1&
                              )+2)-pinter(ndim*(cface(nface,1)-1)+2))*zr(jgrlsn-1+ndim*(ifiss-1)+1)
                        if (det.lt.0.d0) then
                           tempo = cface(nface,1)
                           cface(nface,1) = cface(nface,2)
                           cface(nface,2) = tempo
                        endif
!       DANS LE CAS QUADRATIQUE ON RAJOUTE LE POINT MILIEU DU SEGMENT
                        if (.not. iselli(elp)) then
                           if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 3000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+ar(j,3))-&
                                              3001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .gt. 2000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(jmilt-1+ndim*(zi(jcnset-1+nnose*(i-1)+ar(j,3))-&
                                              2001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+ar(j,3)) .lt. 2000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+ar(j,3))-&
                                              1)+ii)
                              end do
                           endif
!      ON ARCHIVE PINTER ET CFACE
                           npi = npi+1
                           do jj = 1, ndim
                               pinter(ndim*(npi-1)+jj) = newpt(jj)
                           end do
                           cface(nface,3)=npi
                           do jj = 1, zxain-1
                              ainter(zxain*(npi-1)+jj) = 0.d0
                           end do
                        endif
                     endif
22                   continue
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
           call confac(typsma, ibid2, ibid,f,nbf)
       endif
!      BOUCLE SUR LES SOUS ELEMENTS
              do i = 1, nelttot
!      ON NE REGARDE QUE LES SOUS ELEMENTS AYANT HE=-1
                if (zi(jphe-1+(ifiss-1)*ncompe+i) .eq. signe) then
!      BOUCLE SUR LES ARETES (2D) OU FACES (3D) DU SOUS ELEMENT
                  do j = 1, nbf
!      COMPTEUR DE NOEUDS SOMMETS SUR LA LSN=0 PAR FACE
                     h = 0
!      BOUCLE SUR SOMMETS DE l'ARETE (2D) OU DE LA FACE (3D)
                     do k = 1, 3
                       if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 1000) then
                          h = h+1
                       else if (zr(jlsn-1+(zi(jcnset-1+nnose*(i-1)+f(j,k))-1)*nfiss+&
                                ifiss).eq.0.d0) then
                          h = h+1
                       else
                         goto 23
                       endif
                     end do
!      TEST SPECIFIQUE POUR LES ELEMENTS MULTI-HEAVISIDE
                     if (nfiss.gt.1) then
                        call vecini(ndim+1, 0.d0, lsn)
                        do k = 1, 3
                           call vecini(ndim, 0.d0, newpt)
                           if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 1000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*(i-1)+f(j,k))-&
                                              1001)+ii)
                              end do
                           else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .lt. 1000) then
                              do ii = 1, ndim
                                  newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+f(j,k))-&
                                              1)+ii)
                              end do
                           endif
                           call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                       ptref, ff)
                           do ino = 1, nno
                              lsn(k)= lsn(k) + zr(jlsn-1+(ino-1)*nfiss+ifiss)*ff(ino)
                           end do
!      ON AJUSTE LES POINTS DE JONCTION DE FISSURE A ZERO
                           if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 1000) then
                              if (zr(jaint-1+zxain*(zi(jcnset-1+nnose*(i-1)+&
                                  f(j,k))-1001)+4) .eq. -1.d0) then
                                 if (abs(lsn(k)).le.(lonref*crijonc)) lsn(k) = 0.d0
                              endif
                           endif
                        end do
                        if ((abs(lsn(1))+abs(lsn(2))+abs(lsn(3))).ge.(lonref*cridist)) goto 23
!      TEST SUPPLEMENTAIRE POUR LES ELEMENTS TRES ALONGES A PROXIMITE DE LA FISSURE
                        if (zi(jcnset-1+nnose*(i-1)+10-f(j,1)-f(j,2)-f(j,3)) .gt. 1000) then
                           call vecini(ndim, 0.d0, newpt)
                           do ii = 1, ndim
                               newpt(ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*&
                                           (i-1)+10-f(j,1)-f(j,2)-f(j,3))-1001)+ii)
                           end do
                           call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                       ptref, ff)
                           do ino = 1, nno
                              lsn(4)= lsn(4) + zr(jlsn-1+(ino-1)*nfiss+ifiss)*ff(ino)
                           end do
                           if (abs(lsn(4)).lt.max(abs(lsn(1)),abs(lsn(2)),abs(lsn(3)))) goto 23
                        endif
!      EN QUADRATIQUE ON VERIFIE QUE LES NOEUDS MILIEU DU TRIA VERIFIENT LSN=0,
!      SINON ON EXCLUE CE TRIA
                     elseif (.not.iselli(elp)) then
                        call vecini(ndim+1, 0.d0, lsn)
                        do k = 1, 3
                           call vecini(ndim, 0.d0, newpt)
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
                                 goto 23
                              else
                                 goto 49
                              endif
                           endif
                           call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                       ptref, ff)
                           do ino = 1, nno
                              lsn(3)=lsn(3)+zr(jlsn-1+ino)*ff(ino)
                           end do
                           if (abs(lsn(k)).ge.lonref*cridist) goto 23
                        end do
                     endif
49                   continue
!      SI LE NOMBRE DE NOEUDS SOMMETS DE LA FACE QUI SONT SUR LA LSN EST 3
                     if (h .eq. 3) then
!      ON AJOUTE CETTE FACE
                        nface = nface+1
!      RECUPERATION DES COORDONNEES REELLES DES NOEUDS SITUES SUR CETTE FACE ET ARCHIVAGE
!      SI NON DEJA ARCHIVE
                        do k = 1, 3
                           if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .gt. 1000) then
                              do ii = 1, ndim
                                   newpt(ii) = zr(jpint-1+ndim*(zi(jcnset-1+nnose*(i-1)+f(j,k))-&
                                               1001)+ii)
                              end do
                              do ii = 1, zxain-1
                                   rainter(ii) = zr(jaint-1+zxain*(zi(jcnset-1+nnose*(i-1)+&
                                                 f(j,k))-1001)+ii)
                              end do
!                              if (rainter(1) .eq. 0.d0 .and. rainter(2) .eq. 0.d0 .and. &
!                                  ifiss .gt. 1) then
!                                 do ii = 1, nbar
!                                    do jj = 1, ndim
!                                       ab(jj) = zr(igeom-1+ndim*(ar(ii,1)-1)+jj) - zr(igeom-1+&
!                                                ndim*(ar(ii,2)-1)+jj)
!                                       bc(jj) = zr(igeom-1+ndim*(ar(ii,1)-1)+jj) - newpt(jj)
!                                    end do
!                                    call provec(ab, bc, normfa)
!                                    call xnormv(ndim, normfa, det)
!                                    if (det .le. cridist*lonref) then
!                                       rainter(1) = ii
!                                    else
!                                       inc = inc - 1
!                                       rainter(1) = inc
!                                    endif
!                                 end do
!                              endif
                           else if (zi(jcnset-1+nnose*(i-1)+f(j,k)) .lt. 1000) then
                              do ii = 1, ndim
                                   newpt(ii) = zr(igeom-1+ndim*(zi(jcnset-1+nnose*(i-1)+&
                                               f(j,k))-1)+ii)
                              end do
                              rainter(1) = 0.d0
                              rainter(2) = zi(jcnset-1+nnose*(i-1)+f(j,k))
                              rainter(3) = 0.d0
                              rainter(4) = 0.d0 
                           endif
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
                               end do
                               cface(nface,k)=npi
                               do jj = 1, zxain-1
                                  ainter(zxain*(npi-1)+jj) = rainter(jj)
                               end do
                           else
                               cface(nface,k)=ni
                           endif
                        end do
!      DANS LE CAS QUADRATIQUE ON AJOUTE LES POINTS MILIEUX
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
                                  do jj = 1, ndim
                                      pinter(ndim*(npi-1)+jj) = newpt(jj)
                                  end do
                                  cface(nface,k)=npi
                               do jj = 1, zxain-1
                                  ainter(zxain*(npi-1)+jj) = 0.d0
                               end do
                              else
                                  cface(nface,k)=ni
                              endif
                           end do
                        endif
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
                        do jj = 1, ndim
                           ab(jj) = pinter(ndim*(cface(nface,2)-1)+jj) - pinter(ndim*&
                                    (cface(nface,1)-1)+jj)
                           bc(jj) = pinter(ndim*(cface(nface,3)-1)+jj) - pinter(ndim*&
                                    (cface(nface,2)-1)+jj)
                           gradlsn(jj) = zr(jgrlsn-1+ndim*(ifiss-1)+jj)
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
                     endif
23                   continue
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
