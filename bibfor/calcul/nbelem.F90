function nbelem(ligrlz, igrel, icalc)

use calcul_module, only : ca_illiel_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnum.h"

        character(len=*), intent(in) :: ligrlz
        integer, intent(in) :: igrel
        integer, intent(in), optional :: icalc
        integer :: nbelem
!-----------------------------------------------------------------------
!     Entrees:
!       ligrel (o) : nom d'1 ligrel
!       igrel  (o) : numero d'1 grel
!       icalc  (f) : / 1 :  on est "sous" calcul (et apres appel debca1) 
!                           => on va plus vite
!                    / absent :  => on va moins vite
!     Sorties:
!       nbelem : nombre d'elements du grel igrel
!-----------------------------------------------------------------------
    integer :: n1
    character(len=19) :: ligrel
!-------------------------------------------------------------------
    ligrel=ligrlz
    ASSERT(igrel.gt.0)

!   -- si on est "sous" calcul, on peut aller plus vite :
    if (present(icalc)) then
        ASSERT(icalc.eq.1)
        n1=zi(ca_illiel_-1+igrel+1)-zi(ca_illiel_-1+igrel)
    else
        call jelira(jexnum(ligrel//'.LIEL', igrel), 'LONMAX', n1)
    endif
    nbelem=n1-1

end function
