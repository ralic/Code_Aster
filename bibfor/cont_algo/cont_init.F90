subroutine cont_init(mesh  , model     , ds_contact, nume_inst, ds_measure    ,&
                     sddyna, hat_valinc, sdnume    , nume_dof , list_func_acti)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/xminit.h"
#include "asterfort/mminit.h"
#include "asterfort/cfinit.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: model
    type(NL_DS_Contact), intent(inout) :: ds_contact
    type(NL_DS_Measure), intent(inout) :: ds_measure
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: hat_valinc(*)
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: sdnume
    character(len=24), intent(in) :: nume_dof    
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! All methods - Initializations for current time step
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IO  ds_contact       : datastructure for contact management
! In  nume_inst        : index of current step time
! In  hat_valinc       : hat variable for algorithm fields
! In  nume_dof         : name of numbering object (NUME_DDL)
! IO  ds_measure       : datastructure for measure and statistics management
! In  sddyna           : datastructure for dynamic
! In  sdnume           : name of dof positions datastructure
! In  list_func_acti   : list of active functionnalities
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_disc, l_cont_allv, l_cont_cont, l_cont_xfem
!
! --------------------------------------------------------------------------------------------------
!
    l_cont_disc = isfonc(list_func_acti,'CONT_DISCRET')
    l_cont_cont = isfonc(list_func_acti,'CONT_CONTINU')
    l_cont_xfem = isfonc(list_func_acti,'CONT_XFEM')
    l_cont_allv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
!    
    if (.not.l_cont_allv) then
!
! ----- For discrete contact
!
        if (l_cont_disc) then
            call cfinit(ds_contact, nume_inst)
        endif
!
! ----- For continue contact
!
        if (l_cont_cont) then
            call mminit(mesh  , ds_contact, sddyna  , hat_valinc, ds_measure,&
                        sdnume, nume_dof  , nume_inst)
        endif
!
! ----- For XFEM contact
!
        if (l_cont_xfem) then
            call xminit(mesh  , model , ds_contact, nume_inst, ds_measure,&
                        sddyna, hat_valinc)
        endif   
    endif
!
end subroutine
