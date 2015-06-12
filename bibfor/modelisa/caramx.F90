subroutine caramx(sdcont, cont_form, nb_cont_zone)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/caracc.h"
#include "asterfort/caracd.h"
#include "asterfort/caracm.h"
#include "asterfort/caracp.h"
#include "asterfort/caracx.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    integer, intent(in) :: cont_form
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Creation of datastructures
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  cont_form        : formulation of contact
! In  nb_cont_zone     : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_mesh
!
! --------------------------------------------------------------------------------------------------
!
    l_cont_mesh = (cont_form.eq.1).or.(cont_form.eq.2)
!
! - Datastructures for all formulations (Not depending on contact zone)
!
    call caracp(sdcont)
!
! - Datastructures for meshed formulations (depending on contact zone)
!
    if (l_cont_mesh) then
        call caracm(sdcont, nb_cont_zone)
    endif
!
! - Datastructures for formulations
!
    if (cont_form .eq. 1) then
        call caracd(sdcont, nb_cont_zone)
    else if (cont_form.eq.2) then
        call caracc(sdcont, nb_cont_zone)
    else if (cont_form.eq.3) then
        call caracx(sdcont, nb_cont_zone)
    else
        ASSERT(.false.)
    endif
!
end subroutine