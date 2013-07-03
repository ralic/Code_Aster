subroutine reerel(elrefp, nnop, ndim, tabar, xe,&
                  xg)
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/vecini.h"
    integer :: ndim, nnop
    real(kind=8) :: xe(ndim), xg(ndim), tabar(*)
    character(len=8) :: elrefp
!
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
!                      TROUVER LES COORDONNEES REELLES D'UN POINT
!                      A PARTIR DE SES COORDONNEES DE REFERENCE
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT
!       ELP     : TYPE DE L'ELEMENT
!       XE      : COORDONNES DE REFERENCE DU POINT
!
!     SORTIE
!       XG       : COORDONNES REELLES DU POINT
!......................................................................
!
    real(kind=8) :: ff(nnop)
    integer :: i, j, nbnomx, nno
    parameter   (nbnomx = 27)
!
!......................................................................
!
    call jemarq()
!
    call vecini(ndim, 0.d0, xg)
!
! --- VALEURS DES FONCTIONS DE FORME EN XE: FF
!
    if (elrefp(1:2) .eq. 'SE') then
        call elrfvf(elrefp, xe(1), nbnomx, ff, nno)
    else
        call elrfvf(elrefp, xe, nbnomx, ff, nno)
    endif
!
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
!
    do 100 j = 1, ndim
        do 200 i = 1, nnop
            xg(j) = xg(j) + tabar(ndim*(i-1)+j)*ff(i)
200      continue
100  end do
!
    call jedema()
end subroutine
