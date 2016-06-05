subroutine nmetac(list_func_acti, sddyna, ds_contact, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/SetIOField.h"
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
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Contact), intent(in) :: ds_contact
    type(NL_DS_InOut), intent(inout) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Input/output datastructure
!
! Select fields depending on active functionnalities
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  sddyna           : name of dynamic parameters datastructure
! In  ds_contact       : datastructure for contact management
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi
    aster_logical :: l_cont_xfem, l_frot_xfem, l_xfem_czm, l_cont
    aster_logical :: l_dyna, l_inte_node, l_muap, l_strx
    aster_logical :: l_vibr_mode, l_crit_stab, l_dof_stab, l_ener
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi = ds_contact%sdcont_defi
!
! - Active functionnalities
!
    l_dyna      = ndynlo(sddyna,'DYNAMIQUE' )
    l_muap      = ndynlo(sddyna,'MULTI_APPUI')
    l_crit_stab = isfonc(list_func_acti,'CRIT_STAB' )
    l_dof_stab  = isfonc(list_func_acti,'DDL_STAB' )
    l_vibr_mode = isfonc(list_func_acti,'MODE_VIBR' )
    l_strx      = isfonc(list_func_acti,'EXI_STRX')
    l_ener      = isfonc(list_func_acti,'ENERGIE' )
    l_cont_xfem = isfonc(list_func_acti,'CONT_XFEM' )
    l_cont      = isfonc(list_func_acti,'CONTACT' )
    if (l_cont_xfem) then
        l_frot_xfem = isfonc(list_func_acti,'FROT_XFEM')
        l_xfem_czm  = cfdisl(sdcont_defi,'EXIS_XFEM_CZM')
    endif
!
! - Standard: DEPL/SIEF_ELGA/VARI_ELGA/FORC_NODA/COMPOR/EPSI_ELGA
!
    call SetIOField(ds_inout, 'DEPL'        , l_acti_ = .true._1)
    call SetIOField(ds_inout, 'SIEF_ELGA'   , l_acti_ = .true._1)
    call SetIOField(ds_inout, 'EPSI_ELGA'   , l_acti_ = .true._1)
    call SetIOField(ds_inout, 'VARI_ELGA'   , l_acti_ = .true._1)
    call SetIOField(ds_inout, 'FORC_NODA'   , l_acti_ = .true._1)    
    call SetIOField(ds_inout, 'COMPORTEMENT', l_acti_ = .true._1) 
!
! - Dynamic: VITE/ACCE
!
    if (l_dyna) then
        call SetIOField(ds_inout, 'VITE', l_acti_ = .true._1)
        call SetIOField(ds_inout, 'ACCE', l_acti_ = .true._1)
    endif
!
! - XFEM
!
    if (l_cont_xfem) then
        call SetIOField(ds_inout, 'INDC_ELEM', l_acti_ = .true._1)
        if (l_frot_xfem) then
            call SetIOField(ds_inout, 'SECO_ELEM', l_acti_ = .true._1)
        endif
        if (l_xfem_czm) then
            call SetIOField(ds_inout, 'COHE_ELEM', l_acti_ = .true._1)
        endif
    endif
!
! - Contact
!
    if (l_cont) then
        l_inte_node = cfdisl(sdcont_defi, 'ALL_INTEG_NOEUD')
        if (l_inte_node) then
            call SetIOField(ds_inout, 'CONT_NOEU', l_acti_ = .true._1)
        endif
        if (ds_contact%l_form_lac) then
            call SetIOField(ds_inout, 'CONT_ELEM', l_acti_ = .true._1)
        endif
    endif
!
! - Stability criterion (buckling)
!
    if (l_crit_stab) then
        call SetIOField(ds_inout, 'MODE_FLAMB', l_acti_ = .true._1)
    endif
!
! - Stability criterion (with dof selection)
!
    if (l_dof_stab) then
        call SetIOField(ds_inout, 'MODE_STAB', l_acti_ = .true._1)
    endif
!
! - Vibration modes
!
    if (l_vibr_mode) then
        call SetIOField(ds_inout, 'DEPL_VIBR', l_acti_ = .true._1)
    endif
!
! - "MULTI-APPUIS": DEPL/VITE/ACCE d'entrainement
!
    if (l_muap) then
        call SetIOField(ds_inout, 'DEPL_ABSOLU', l_acti_ = .true._1)
        call SetIOField(ds_inout, 'VITE_ABSOLU', l_acti_ = .true._1)
        call SetIOField(ds_inout, 'ACCE_ABSOLU', l_acti_ = .true._1)
    endif
!
! - Special elements: multifibers beams
!
    if (l_strx) then
        call SetIOField(ds_inout, 'STRX_ELGA', l_acti_ = .true._1)
    endif
!
! - Energy
!
    if (l_ener) then
        call SetIOField(ds_inout, 'FORC_AMOR', l_acti_ = .true._1)
        call SetIOField(ds_inout, 'FORC_LIAI', l_acti_ = .true._1)
    endif
!
end subroutine
