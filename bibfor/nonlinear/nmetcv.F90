subroutine nmetcv(field_refe, field_in, field_disc_in, field_out, field_disc_out)
!
implicit none
!
#include "asterfort/chpchd.h"
#include "asterfort/copisd.h"
#include "asterfort/utmess.h"
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
    character(len=24), intent(in) :: field_refe
    character(len=24), intent(in) :: field_in
    character(len=24), intent(in) :: field_out
    character(len=24), intent(in) :: field_disc_in
    character(len=24), intent(in) :: field_disc_out
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Field conversion (discretization)
!
! --------------------------------------------------------------------------------------------------
!
! In  field_refe      : name of a reference field to convert ELGA fields
! In  field_in        : name of field to convert
! In  field_disc_in   : spatial discretization of field to convert
! In  field_out       : name of field converted
! In  field_disc_out  : spatial discretization of field converted
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: valk(3)
!
! --------------------------------------------------------------------------------------------------
!
!
! - Good discretization -> nothing to do
!
    if (field_disc_in .eq. field_disc_out) then
        call copisd('CHAMP_GD', 'V', field_in, field_out)
    else
!
! ----- Not good discretization -> is it possible to convert ?
!
        valk(1) = field_in
        valk(2) = field_disc_in
        valk(3) = field_disc_out
        if (field_disc_out .eq. 'ELGA') then
            if (field_refe .eq. ' ') then
                call utmess('F', 'ETATINIT_52', nk=3, valk=valk)
            else
                call utmess('I', 'ETATINIT_51', nk=3, valk=valk)
            endif
        else
            call utmess('F', 'ETATINIT_52', nk=3, valk=valk)
        endif
!
! ----- Not good discretization -> convert
!
        call chpchd(field_in , field_disc_out, field_refe, 'NON', 'V',&
                    field_out)
    endif
!
end subroutine
