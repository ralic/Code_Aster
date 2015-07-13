subroutine cazoco(sdcont      , model, keywf, cont_form, i_zone,&
                  nb_cont_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cazocc.h"
#include "asterfort/cazocd.h"
#include "asterfort/cazocm.h"
#include "asterfort/cazocx.h"
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
    character(len=8), intent(in) :: model
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: nb_cont_zone
    integer, intent(in) :: cont_form
    integer, intent(in) :: i_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get parameters (depending on contact zones)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  model            : name of model
! In  keywf            : factor keyword to read
! In  nb_cont_zone     : number of zones of contact
! In  cont_form        : formulation of contact
! In  i_zone           : index of contact zone
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_mesh
!
! --------------------------------------------------------------------------------------------------
!
    l_cont_mesh = (cont_form.eq.1).or.(cont_form.eq.2)
!
! - Main parameters for pairing
!
    if (l_cont_mesh) then
        call cazocm(sdcont, keywf, i_zone)
    endif
!
! - Other parameters depending on formulation
!
    if (cont_form .eq. 1) then
        call cazocd(sdcont, keywf, i_zone, nb_cont_zone)
    else if (cont_form.eq.2) then
        call cazocc(sdcont, keywf, i_zone)
    else if (cont_form.eq.3) then
        call cazocx(sdcont, model, keywf, i_zone)
    else
        ASSERT(.false.)
    endif
!
end subroutine
