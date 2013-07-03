subroutine ndcent(igeom, lsn, x, xlsn)
    implicit none
!
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/elrfvf.h"
#include "asterfort/reereg.h"
    integer :: igeom
    real(kind=8) :: x(3), lsn(8), xlsn
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
!       CALCUL DES COORDONNEES ET DE LA LSN DU NOEUD MILIEU
!       DE LA DIAGONALE DROITE D'UN QUAD8
!
!     ENTREE
!       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
!       LSN      : LSN DES NOEUDS DE L'ELT PARENT
!     SORTIE
!       X        : COORDONNEES DU NOEUD CENTRAL (MILIEU DE L'ARETE 2-4)
!       XLSN     : LSN DU NOEUD CENTRAL (MILIEU DE L'ARETE 2-4)
!......................................................................
!
    integer :: i, iret, nno, ndim, nbnomx, ibid
    real(kind=8) :: ff(8), xe(3)
    character(len=8) :: elp
    parameter     (nbnomx = 8)
!
!......................................................................
!
    elp='QU8'
    call elref4(elp, 'RIGI', ndim, nno, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!      CALCUL DES COORDONNEES DU MILIEU DE [AB]
    do 10 i = 1, ndim
        x(i)=(zr(igeom-1+ndim*(2-1)+i)+zr(igeom-1+ndim*(4-1)+i))/2
10  continue
!
!     CALCUL DE LA LSN DU MILIEU DE [AB]
    call reereg('S', elp, nno, zr(igeom), x,&
                ndim, xe, iret)
    call elrfvf(elp, xe, nbnomx, ff, nno)
!
    xlsn = 0
    do 50 i = 1, nno
        xlsn = xlsn + ff(i)*lsn(i)
50  continue
!
end subroutine
