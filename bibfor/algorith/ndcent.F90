subroutine ndcent(igeom, ndim, lsn, tx, txlsn, nnc)
    implicit none
!
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/elrfvf.h"
#include "asterfort/matini.h"
#include "asterfort/reereg.h"
#include "asterfort/reerel.h"
#include "asterfort/vecini.h"
    integer :: igeom, nnc, ndim
    real(kind=8) :: tx(3, 7), lsn(*), txlsn(7)
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
!       CALCUL DES COORDONNEES ET DE LA LSN DES NOEUDS MILIEUX
!       CENTRAUX D UN ELEMENT QUADRATIQUE
!
!     ENTREE
!       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
!       LSN      : LSN DES NOEUDS DE L'ELT PARENT
!     SORTIE
!       X        : COORDONNEES DES NOEUDS MILIEUX CENTRAUX
!       XLSN     : LSN AUX NOEUDS MILIEUX CENTRAUX
!       NNC      : NOMBRE DE NOEUDS MILIEUX CENTRAUX
!
    integer :: nbnomx
    parameter     (nbnomx = 20)
    integer :: i, j, nnop, ibid
    real(kind=8) :: ff(nbnomx), xlsn, xe(3)
    character(len=8) :: elp
!
!
    call elref1(elp)
    call elrefe_info(elrefe=elp,fami='RIGI',nno=nnop)
    call matini(3, 7, 0.d0, tx)
    call vecini(7, 0.d0, txlsn)
!
!     INITIALIASATION PAR DEFAUT DU NOMBRE DE NOEUDS CENTRAUX A ZERO
!     (E.G. TRIANGLES ET TETRAHEDRES)
    nnc=0
!
!     CALCUL DES COORDONNEES DU MILIEU DE [AB] DANS LE CAS 'H20'
    if (nnop .eq. 20) then
        nnc=7
        tx(1,1)=0.d0
        tx(2,1)=0.d0
        tx(3,1)=-1.0
        tx(1,2)=0.d0
        tx(2,2)=-1.0
        tx(3,2)=0.d0
        tx(1,3)=1.0
        tx(2,3)=0.d0
        tx(3,3)=0.d0
        tx(1,4)=0.d0
        tx(2,4)=1.0
        tx(3,4)=0.d0
        tx(1,5)=-1.0
        tx(2,5)=0.d0
        tx(3,5)=0.d0
        tx(1,6)=0.d0
        tx(2,6)=0.d0
        tx(3,6)=1.0
        tx(1,7)=0.d0
        tx(2,7)=0.d0
        tx(3,7)=0.d0
!
!     CALCUL DES COORDONNEES DU MILIEU DE [AB] DANS LE CAS 'P15'
    else if (nnop.eq.15) then
        nnc=3
        tx(1,1)=0.d0
        tx(2,1)=0.5
        tx(3,1)=0.5
        tx(1,2)=0.d0
        tx(2,2)=0.d0
        tx(3,2)=0.5
        tx(1,3)=0.d0
        tx(2,3)=0.5
        tx(3,3)=0.d0
!
!     CALCUL DES COORDONNEES DU MILIEU DE [AB] DANS LE CAS 'P13'
    else if (nnop.eq.13) then
        nnc=1
        tx(1,1)=0.d0
        tx(2,1)=0.d0
        tx(3,1)=0.d0
!     CALCUL DES COORDONNEES DU MILIEU DE [AB] DANS LE CAS 2D 'QU8'
    else if ((nnop.eq.8).and.(ndim.eq.2)) then
        nnc=1
        tx(1,1)=0.d0
        tx(2,1)=0.d0
!     CALCUL DES COORDONNEES DU MILIEU DE [AB] DANS LE CAS 3D 'QU8'
    else if ((nnop.eq.8).and.(ndim.eq.3)) then
        nnc=1
        tx(1,1)=0.d0
        tx(2,1)=0.d0
        tx(3,1)=0.d0
    endif
!
!.....................................................................
!     CALCUL DE LA LSN DU MILIEU
!
    do 10 j = 1, nnc
        do 11 i = 1, ndim
            xe(i)=tx(i,j)
11      continue
!
        call elrfvf(elp, xe, nbnomx, ff, ibid)
        xlsn = 0
        do 12 i = 1, nnop
            xlsn = xlsn + ff(i)*lsn(i)
12      continue
        txlsn(j)=xlsn
10  end do
!
!.....................................................................
!      CALCUL DES COORDONNES DANS L ELEMENT REEL
    do 20 j = 1, nnc
        call reerel(elp, nnop, ndim, zr(igeom), tx(1:ndim,j),&
                    xe)
        do 21 i = 1, ndim
            tx(i,j)=xe(i)
21      continue
20  continue
end subroutine
