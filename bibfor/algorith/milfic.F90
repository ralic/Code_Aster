subroutine milfic(ndim, geom, xg)
    implicit none
!
#include "jeveux.h"
#include "asterfort/abscvf.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reerel.h"
#include "asterfort/xinvac.h"
    integer :: ndim
    real(kind=8) :: xg(ndim), geom(*)
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
!                      COORDONNEES RELLES DU POINT MILIEU D'UNE ARETE
!                      QUADRATIQUE
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       GEOM   : COORDONNEES DES 3 NOEUDS DE L'ARETE
!
!     SORTIE
!       XG       : COORDONNES RELLES DU PT MILIEU DE L'ARETE
!
!......................................................................
!
    real(kind=8) :: s, s1, xe1, xe(1)
    integer :: nno
    character(len=8) :: elp
    parameter     (elp='SE3')
    parameter     (nno=3)
!
!......................................................................
!
    call jemarq()
!     DANS COORSG : 1. C ---> ETA(C)=-1
!                   2. 101 ---> ETA(101)= 1
!                   3. F ---> ETA(F)= 0
!
!     CALCUL DE L'ABSCISSE CURVILIGNE DE 101
    xe1=1
    call abscvf(ndim, geom, xe1, s1)
!
! --- COORDONNEES DU POINT DANS L'ELEMENT DE REFERENCE
!     ABSCURV(M)=[ABSCURV(A)]/2
    s=s1/2
    call xinvac(elp, ndim, geom, s, xe)
    ASSERT(xe(1).ge.-1 .and. xe(1).le.1)
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
    call reerel(elp, nno, ndim, geom, xe,&
                xg)
!
    call jedema()
end subroutine
