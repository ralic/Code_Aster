subroutine xpolsn(elrefp, ino, n, jlsn, jlst,&
                  ima, iad, igeom, nfiss, ndime,&
                  ndim, jconx1, jconx2, co, lsn,&
                  lst)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/vecini.h"
#include "asterfort/xpoffo.h"
    integer :: n, jlsn, jlst, ndim, ndime, ino, nfiss
    integer :: iad, igeom, ima, jconx1, jconx2
    character(len=8) :: elrefp
    real(kind=8) :: co(3), lsn(nfiss), lst(nfiss)
!
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
!              RECUPERATION DES COORDONNEES RELLES ET
!                 CALCUL DES LEVEL SET DE L'ENTITE
!
!   IN
!     ELREFP : ÉLÉMENT DE RÉFÉRENCE PARENT
!     INO   : NUMERO DU POINT D'INTER OU DU NOEUD
!     N      : NOMBRE DE NOEUDS DE L'ÉLÉMENT PARENT
!     JLSN   : ADRESSE DU CHAM_NO_S DE LA LEVEL NORMALE
!     JLST   : ADRESSE DU CHAM_NO_S DE LA LEVEL TANGENTE
!     IMA    : NUMERO DE MAILLE COURANTE PARENT
!     IAD    : POINTEUR DES COORDONÉES DE INO
!     IGEOM  : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
!     NFISS  : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT PARENT
!     NDIME  : DIMENSION TOPOLOGIQUE DE LA MAILLE PARENT
!     NDIM   : DIMENSION DU MAILLAGE
!     JCONX1 : ADRESSE DE LA CONNECTIVITE DU MAILLAGE SAIN
!              (CONNECTIVITE QUADRATIQUE SI LAGRANGES DE CONTACT
!              AUX ARETES)
!     JCONX2 : LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE SAIN
!              (CONNECTIVITE QUADRATIQUE SI LAGRANGES DE CONTACT
!              AUX ARETES)
!   OUT
!     CO     : COORDONNEES DU NOEUD OU DU POINT
!     LSN    : LEVEL SET NORMALE DU NOEUD OU DU POINT
!     LST    : LEVEL SET TANGENTE DU NOEUD OU DU POINT
!
    real(kind=8) :: ff(n)
    integer :: i, ifiss
!
    call jemarq()
    call vecini(3, 0.d0, co)
!
!     CAS D'UN NOEUD
    if (ino .lt. 1000) then
        i=zi(jconx1-1+zi(jconx2+ima-1)+ino-1)
        co(1)=zr(iad-1+3*(i-1)+1)
        co(2)=zr(iad-1+3*(i-1)+2)
        co(3)=zr(iad-1+3*(i-1)+3)
!
        do 23 ifiss = 1, nfiss
            lsn(ifiss) = zr(jlsn-1+nfiss*(ino-1)+ifiss)
            lst(ifiss) = zr(jlst-1+nfiss*(ino-1)+ifiss)
23      continue
!
!     CAS D'UN POINT D'INTERSECTION OU D'UN POINT MILIEU
    else if (ino.gt.1000) then
        do 11 i = 1, ndim
            co(i)=zr(iad-1+i)
11      continue
!
!       FF : FONCTIONS DE FORMES
        call xpoffo(ndim, ndime, elrefp, n, igeom,&
                    co, ff)
!
        do 35 ifiss = 1, nfiss
            lsn(ifiss) = 0.d0
            lst(ifiss) = 0.d0
            do 30 i = 1, n
                lsn(ifiss)=lsn(ifiss)+zr(jlsn-1+nfiss*(i-1)+ifiss)*ff(&
                i)
                lst(ifiss)=lst(ifiss)+zr(jlst-1+nfiss*(i-1)+ifiss)*ff(&
                i)
30          continue
35      continue
!
    endif   
!
    call jedema()
!
end subroutine

