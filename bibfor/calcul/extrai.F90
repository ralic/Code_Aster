subroutine extrai(nin, lchin, lpain, opt, nute,&
                  ligrel, init)

use calcul_module, only : ca_calvoi_, ca_igr_, ca_nbelgr_, ca_nbgr_

implicit none

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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "asterfort/assert.h"
#include "asterfort/extra1.h"
#include "asterfort/nbelem.h"
#include "asterfort/typele.h"

    integer :: nin, opt, nute
    character(len=*) :: lchin(*), init
    character(len=8) :: lpain(*)
    character(len=19) :: ligrel
!-----------------------------------------------------------------------
!     but: preparer les champs locaux "in"
!-----------------------------------------------------------------------

    ASSERT(init.eq.' '.or.init.eq.'INIT')

    if (ca_calvoi_ .eq. 0) then
        if (init .ne. 'INIT') then
            call extra1(nin, lchin, lpain, opt, nute)
        endif
    else
!       -- on prepare tout la 1ere fois :
        if (init .eq. 'INIT') then
            do ca_igr_=1,ca_nbgr_
                ca_nbelgr_=nbelem(ligrel,ca_igr_)
                if (ca_nbelgr_.gt.0) then
                    nute=typele(ligrel,ca_igr_)
                    call extra1(nin, lchin, lpain, opt, nute)
                endif
            enddo
        endif
    endif


end subroutine
