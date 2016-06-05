subroutine nmctce(model    , mesh, ds_contact, sddyna, sddisc,&
                  nume_inst)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmchml.h"
#include "asterfort/xmcart.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in)  :: sddyna
    character(len=19), intent(in)  :: sddisc
    integer, intent(in) :: nume_inst
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Continue/XFEM method
!
! Create elements for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  sddyna           : dynamic parameters datastructure
! In  sddisc           : datastructure for time discretization
! In  nume_inst        : index of current step time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: cont_form
    aster_logical :: l_cont_xfem_gg
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> Create input fields for contact'
    endif
!
! - Parameters
!
    cont_form      = cfdisi(ds_contact%sdcont_defi,'FORMULATION')
    l_cont_xfem_gg = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
!
! - Create input fields for contact
!
    if (cont_form .eq. 2 .or. cont_form .eq. 5) then
        call mmchml(mesh, ds_contact, sddisc, sddyna, nume_inst)
    elseif  (cont_form .eq. 3) then
        if (l_cont_xfem_gg) then
            call xmcart(mesh, model, ds_contact)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
