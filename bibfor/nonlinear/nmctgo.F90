subroutine nmctgo(mesh, sderro, hval_incr, ds_print, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfverl.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmmcri_geom.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sderro
    character(len=19), intent(in) :: hval_incr(*)
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algo
!
! Geometry loop management - Management
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sderro           : datastructure for errors during algorithm
! In  hval_incr        : hat-variable for incremental values fields
! IO  ds_print         : datastructure for printing parameters
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_cont_cont, l_cont_disc, l_cont_xfem
    aster_logical :: l_geom_sans, l_geom_manu, l_geom_auto
    integer :: nb_iter_geom, iter_geom_maxi
    integer :: loop_geom_count
    character(len=19) :: disp_curr, loop_geom_disp, disp_prev
    character(len=16) :: loop_geom_node
    real(kind=8) :: loop_geom_vale
    aster_logical :: loop_geom_conv, loop_geom_error
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> MISE A JOUR DU SEUIL DE GEOMETRIE'
    endif
!
! - Initializations
!
    loop_geom_node  = ' '
    loop_geom_vale  = r8vide()
    call mmbouc(ds_contact, 'Geom', 'Set_NoError')
!
! - Get fields
!
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
!
! - Get contact parameters
!
    l_cont_cont = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    l_cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
!
! - Get geometry loop parameters
!
    loop_geom_disp = ds_contact%sdcont_solv(1:14)//'.DEPG'
    iter_geom_maxi = cfdisi(ds_contact%sdcont_defi, 'ITER_GEOM_MAXI')
    nb_iter_geom   = cfdisi(ds_contact%sdcont_defi, 'NB_ITER_GEOM' )
    l_geom_manu    = cfdisl(ds_contact%sdcont_defi, 'REAC_GEOM_MANU')
    l_geom_sans    = cfdisl(ds_contact%sdcont_defi, 'REAC_GEOM_SANS')
    l_geom_auto    = cfdisl(ds_contact%sdcont_defi, 'REAC_GEOM_AUTO')
!
! - Update triggers
!
    if (l_cont_cont .or. l_cont_xfem) then
!
! ----- Compute geometry criterion
!
        call mmmcri_geom(mesh      , disp_prev, loop_geom_disp, disp_curr,&
                         ds_contact)
!
! ----- Get values
!
        call mmbouc(ds_contact, 'Geom', 'Read_Counter'  , loop_geom_count)
        call mmbouc(ds_contact, 'Geom', 'Is_Convergence', loop_state_ = loop_geom_conv)         
!
! ----- For REAC_GEOM = 'MANU'
!
        if (l_geom_manu) then
            if (loop_geom_count .eq. nb_iter_geom) then
                if ((.not.loop_geom_conv) .and. (nb_iter_geom.gt.1)) then
                    call utmess('A', 'CONTACT3_96')
                endif
                call mmbouc(ds_contact, 'Geom', 'Set_Convergence')
            else
                call mmbouc(ds_contact, 'Geom', 'Set_Divergence')
            endif
        endif
!
! ----- For REAC_GEOM = 'SANS'
!
        if (l_geom_sans) then
            call mmbouc(ds_contact, 'Geom', 'Set_Convergence')
        endif
!
! ----- For REAC_GEOM = 'AUTO'
!
        if (l_geom_auto) then
            if ((.not.loop_geom_conv) .and. (loop_geom_count .eq. iter_geom_maxi)) then
                if (l_cont_cont) then
                    call cfverl(ds_contact)
                endif
                call mmbouc(ds_contact, 'Geom', 'Set_Error')
            endif
        endif
!
! ----- Update reference displacement for geometry loop
!
        call mmbouc(ds_contact, 'Geom', 'Is_Convergence', loop_state_ = loop_geom_conv)
        if (.not.loop_geom_conv) then
            call copisd('CHAMP_GD', 'V', disp_curr, loop_geom_disp)
        endif
    endif
!
! - Get final loop state
!
    call mmbouc(ds_contact, 'Geom', 'Is_Convergence', loop_state_ = loop_geom_conv)
    call mmbouc(ds_contact, 'Geom', 'Is_Error'      , loop_state_ = loop_geom_error)
    call mmbouc(ds_contact, 'Geom', 'Get_Locus'     , loop_locus_ = loop_geom_node)
    call mmbouc(ds_contact, 'Geom', 'Get_Vale'      , loop_vale_  = loop_geom_vale)
!
! - Save events
!
    call nmcrel(sderro, 'ERRE_CTCG', loop_geom_error)
    if (loop_geom_conv) then
        call nmcrel(sderro, 'DIVE_FIXG', .false._1)
    else
        call nmcrel(sderro, 'DIVE_FIXG', .true._1)
    endif
!
! - Set values in convergence table for contact geoemtry informations
!
    if (l_cont_cont .or. l_cont_xfem) then
        call nmimck(ds_print, 'BOUC_NOEU', loop_geom_node, .true._1)
        call nmimcr(ds_print, 'BOUC_VALE', loop_geom_vale, .true._1)
    endif
!
end subroutine
