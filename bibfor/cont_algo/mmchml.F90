subroutine mmchml(ds_contact, sddisc, sddyna, nume_inst)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/cfdisl.h"
#include "asterfort/detrsd.h"
#include "asterfort/diinst.h"
#include "asterfort/mmchml_c.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: nume_inst
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue methods - Create and fill input field
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  sddisc           : datastructure for time discretization
! In  sddyna           : datastructure for dynamic
! In  nume_inst        : index of current time step
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: ligrcf, chmlcf
    real(kind=8) :: time_prev, time_curr, time_incr
    aster_logical :: l_new_pair, l_cont_cont, l_cont_lac
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CREATION DU CHAM_ELEM POUR LES ELEMENTS DE CONTACT'
    endif
!
! - <LIGREL> for contact elements
!
    ligrcf = ds_contact%ligrel_elem_cont
!
! - <CHELEM> for input field
!
    chmlcf = ds_contact%field_input
!
! - Get parameters
!
    l_cont_cont  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_CONTINUE')
    l_cont_lac   = .false._1
!   l_cont_lac   = cfdisl(ds_contact%sdcont_defi, 'FORMUL_LAC')
!
! - Get time parameters
!
    time_prev = diinst(sddisc,nume_inst-1)
    time_curr = diinst(sddisc,nume_inst)
    time_incr = time_curr-time_prev
!
! - Create input field
!
    l_new_pair = ds_contact%l_renumber
    if (l_new_pair) then
        call detrsd('CHAM_ELEM', chmlcf)
        call alchml(ligrcf, 'RIGI_CONT', 'PCONFR', 'V', chmlcf, iret, ' ')
        ASSERT(iret.eq.0)
    endif
!
! - Fill input field
!
    if (l_cont_cont) then
        call mmchml_c(ds_contact, ligrcf, chmlcf, sddyna, time_incr)
    else if (l_cont_lac) then
        ASSERT(.false.)
!       call mmchml_l(ds_contact, ligrcf, chmlcf, sddyna, time_incr)
    endif
!
end subroutine
