subroutine check_aster_allocate(init)
use allocate_module
! person_in_charge: jacques.pellet at edf.fr
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
    implicit none
    integer, optional, intent(in) :: init
!
! --------------------------------------------------------------------------
! verifier que les objets alloues par as_allocate ont bien ete desalloues
! init=0 => on (re)initialise la variable du common : cuvtrav=0
! --------------------------------------------------------------------------
!
#include "jeveux_private.h"
#include "asterc/jdcget.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
    integer, save :: icode = -1
!
    if (present(init)) then
        ASSERT (init.eq.0)
        cuvtrav=0.d0
    endif
!
    if (abs(cuvtrav) > r8prem()) then
        call utmess('A', 'DVP_6', sr=cuvtrav*lois/1.e6)
        if (icode < 0) then
            icode = jdcget('icode')
        endif
        if (icode == 1) then
            ASSERT(abs(cuvtrav) < r8prem())
        endif
        call deallocate_all_slvec()
    endif
!
end subroutine
