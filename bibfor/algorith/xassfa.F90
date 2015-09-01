subroutine xassfa(elp, npts, nintar, lst, noeud, cface, nface, pinter, jgrlsn)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/iselli.h"
#include "asterfort/provec.h"
#include "blas/ddot.h"
!
    integer :: npts, nintar, noeud(9), cface(18,6), nface, jgrlsn
    real(kind=8) :: lst(3), pinter(*)
    character(len=8) :: elp
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
!                TROUVER LES CONNECTIVITES DES MAILLES DE LA FISSURE POUR UN
!                ELEMENT EN FOND DE FISSURE EN 3D
!
!     ENTREE
!       NPTS    : NOMBRE DE NOEUDS SOMMET DU TRIA TELS QUE LST<=0
!       NINTAR  : NOMBRE DE POINTS D'INTERSECTION DU FOND DE FISSURE AVEC LES
!                 ARETES DU TRIA
!       LST     : LST AUX SOMMETS DU TRIA
!       NOEUD   : INDICE DES NOEUDS DE LA FISSURE DANS LE TRIA
!
!     SORTIE
!       NFACE   : NOMBRE DE FACETTES
!       CFACE   : CONNECTIVITE DES NOEUDS DES FACETTES
!
!     ----------------------------------------------------------------
!
    integer :: tempo, jj
    real(kind=8) ::  ab(3), bc(3), normfa(3), gradlsn(3), det
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      3 CAS SONT POSSIBLES:
!         -UN DES NOEUDS SOMMETS A LST=0, ET LES DEUX AUTRES ONT
!          RESPECTIVEMENT LST<0 et LST >0
!         -UN NOEUD SOMMET A LST<0 ET LES DEUX AUTRES ONT LST>0
!         -DEUX NOEUDS SOMMETS ONT LST<0 ET LE DERNIER A LST>0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   PREMIER CAS
    if (npts.eq.1.and.nintar.eq.2) then
       nface = nface+1
       if (lst(1).lt.0.d0.or.lst(2).lt.0.d0) then
          cface(nface,1) = noeud(3)
          cface(nface,2) = noeud(1)
          cface(nface,3) = noeud(4)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(5)
             cface(nface,5) = noeud(6)
             cface(nface,6) = noeud(8)
          endif
       else if (lst(3).lt.0.d0) then
          cface(nface,1) = noeud(1)
          cface(nface,2) = noeud(3)
          cface(nface,3) = noeud(4)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(5)
             cface(nface,5) = noeud(8)
             cface(nface,6) = noeud(6)
          endif
       else 
          ASSERT(.false.)
       endif
!   DEUXIEME CAS
    else if (npts.eq.2.and.nintar.eq.2) then
       nface = nface+2
       if (lst(1).gt.0.d0) then
          cface(nface-1,1) = noeud(1)
          cface(nface-1,2) = noeud(2)
          cface(nface-1,3) = noeud(4)
          if (.not.iselli(elp)) then
             cface(nface-1,4) = noeud(7)
             cface(nface-1,5) = noeud(6)
             cface(nface-1,6) = noeud(9)
          endif
          cface(nface,1) = noeud(1)
          cface(nface,2) = noeud(4)
          cface(nface,3) = noeud(3)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(9)
             cface(nface,5) = noeud(8)
             cface(nface,6) = noeud(5)
          endif
       else if (lst(2).gt.0.d0) then
          cface(nface-1,1) = noeud(1)
          cface(nface-1,2) = noeud(3)
          cface(nface-1,3) = noeud(2)
          if (.not.iselli(elp)) then
             cface(nface-1,4) = noeud(5)
             cface(nface-1,5) = noeud(9)
             cface(nface-1,6) = noeud(7)
          endif
          cface(nface,1) = noeud(3)
          cface(nface,2) = noeud(4)
          cface(nface,3) = noeud(2)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(8)
             cface(nface,5) = noeud(6)
             cface(nface,6) = noeud(9)
          endif
       else if (lst(3).gt.0.d0) then
          cface(nface-1,1) = noeud(1)
          cface(nface-1,2) = noeud(2)
          cface(nface-1,3) = noeud(3)
          if (.not.iselli(elp)) then
             cface(nface-1,4) = noeud(7)
             cface(nface-1,5) = noeud(5)
             cface(nface-1,6) = noeud(9)
          endif
          cface(nface,1) = noeud(1)
          cface(nface,2) = noeud(3)
          cface(nface,3) = noeud(4)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(9)
             cface(nface,5) = noeud(8)
             cface(nface,6) = noeud(6)
          endif
       else
          ASSERT(.false.)
       endif
!   TROISIEME CAS
    else if (npts.eq.2.and.nintar.eq.1) then
       nface = nface+1
       if (lst(1).gt.0.d0.or.lst(3).gt.0.d0) then
          cface(nface,1) = noeud(1)
          cface(nface,2) = noeud(2)
          cface(nface,3) = noeud(3)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(7)
             cface(nface,5) = noeud(8)
             cface(nface,6) = noeud(5)
          endif
       else if (lst(2).gt.0.d0) then
          cface(nface,1) = noeud(1)
          cface(nface,2) = noeud(3)
          cface(nface,3) = noeud(2)
          if (.not.iselli(elp)) then
             cface(nface,4) = noeud(8)
             cface(nface,5) = noeud(5)
             cface(nface,6) = noeud(7)
          endif
       else
          ASSERT(.false.)
       endif
    else
       ASSERT(.false.)
    endif
!
!       NECESSITE D'INVERSER LA CONNECTIVITE? (ON SOUHAITE TOUJOURS GARDER LA
!       MEME CONVENTION NORMALE DIRIGEE SELON GRADLSN)
    do jj = 1, 3
       ab(jj) = pinter(3*(cface(nface,2)-1)+jj)-pinter(3*(cface(nface,1)-1)+jj)
       bc(jj) = pinter(3*(cface(nface,3)-1)+jj)-pinter(3*(cface(nface,2)-1)+jj)
       gradlsn(jj) = zr(jgrlsn-1+jj)
       normfa(jj) = 0.d0
    end do
    call provec(ab,bc,normfa)
    det = ddot(3, gradlsn, 1, normfa, 1)
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
!
    if (npts.eq.2.and.nintar.eq.2) then
       do jj = 1, 3
          ab(jj) =pinter(3*(cface(nface-1,2)-1)+jj)-pinter(3*(cface(nface-1,1)-1)+jj)
          bc(jj) =pinter(3*(cface(nface-1,3)-1)+jj)-pinter(3*(cface(nface-1,2)-1)+jj)
          normfa(jj) = 0.d0
       end do
       call provec(ab,bc,normfa)
       det = ddot(3, gradlsn, 1, normfa, 1)
       if (det.lt.0.d0) then
          tempo = cface(nface-1,2)
          cface(nface-1,2) = cface(nface-1,3)
          cface(nface-1,3) = tempo
          if (.not. iselli(elp)) then
             tempo = cface(nface-1,4)
             cface(nface-1,4) = cface(nface-1,6)
             cface(nface-1,6) = tempo
          endif
       endif
    endif
!
!
end subroutine
