subroutine nmtble(cont_loop     , model   , mesh  , mate     , ds_contact,&
                  list_func_acti, ds_print, ds_measure, sddyna    ,&
                  sderro        , ds_conv , sddisc, nume_inst, hval_incr ,&
                  hval_algo, ds_constitutive)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mm_cycl_erase.h"
#include "asterfort/mm_cycl_init.h"
#include "asterfort/nmaffi.h"
#include "asterfort/nmctcc.h"
#include "asterfort/nmctcf.h"
#include "asterfort/nmctgo.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
    integer, intent(inout) :: cont_loop
    character(len=24), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: mate
    type(NL_DS_Contact), intent(inout) :: ds_contact
    integer, intent(in) :: list_func_acti(*)
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sderro
    type(NL_DS_Conv), intent(in) :: ds_conv
    character(len=19), intent(in) :: sddisc
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algo
!
! Contact loop management - END
!
! --------------------------------------------------------------------------------------------------
!
! IO  cont_loop        : level of loop for contact (see nmible.F90)
!                        0 - Not use (not cotnact)
!                        1 - Loop for contact status
!                        2 - Loop for friction triggers
!                        3 - Loop for geometry
! In  model            : name of model
! In  mesh             : name of mesh
! In  mate             : name of material characteristics (field)
! IO  ds_contact       : datastructure for contact management
! In  list_func_acti   : list of active functionnalities
! IO  ds_print         : datastructure for printing parameters
! IO  ds_measure       : datastructure for measure and statistics management
! In  sddyna           : dynamic parameters datastructure
! In  sderro           : datastructure for errors during algorithm
! In  ds_conv          : datastructure for convergence management
! In  sddisc           : datastructure for time discretization
! In  nume_inst        : index of current step time
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  ds_constitutive  : datastructure for constitutive laws management
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: loop_cont_conv, loop_fric_conv, loop_geom_conv
    aster_logical :: l_loop_frot, l_loop_geom, l_loop_cont, l_cont_cont
    integer :: loop_geom_count, loop_fric_count, loop_cont_count, loop_cont_vali
    character(len=4) :: state_newt
    real(kind=8) :: loop_cont_vale
!
! --------------------------------------------------------------------------------------------------
!
    if (cont_loop.eq.0) then
        goto 999
    endif
!
! - State of Newton loop
!
    call nmleeb(sderro, 'NEWT', state_newt)
!
! - To evaluate contact loops: Newton has covnerged
!
    if (state_newt.ne.'CONV') then
        goto 999
    endif
!
! - Contact loops
!
    l_cont_cont = isfonc(list_func_acti,'CONT_CONTINU')
    l_loop_frot = isfonc(list_func_acti, 'BOUCLE_EXT_FROT')
    l_loop_geom = isfonc(list_func_acti, 'BOUCLE_EXT_GEOM')
    l_loop_cont = isfonc(list_func_acti, 'BOUCLE_EXT_CONT')
!
! - Initializations
!
    loop_cont_vali = 0
    loop_cont_conv = .false.
    loop_fric_conv = .false.
    loop_geom_conv = .false.
!
! - <1> - Contact loop
!
    if (cont_loop .le. 1) then
        if (l_loop_cont) then
            cont_loop = 1
            call nmctcc(mesh      , model     , mate  , nume_inst, sddyna   ,&
                        sderro    , ds_measure, sddisc, hval_incr, hval_algo,&
                        ds_contact, ds_constitutive   , list_func_acti)
            call mmbouc(ds_contact, 'Cont', 'Is_Convergence', loop_state_ = loop_cont_conv)
            call mmbouc(ds_contact, 'Cont', 'Get_Vale'      , loop_vale_  = loop_cont_vale)
            loop_cont_vali = nint(loop_cont_vale)
            if (.not.loop_cont_conv) then
                cont_loop = 1
                goto 500
            endif
        endif
    endif
!
! - <2> - Friction loop
!
    if (cont_loop .le. 2) then
        if (l_loop_frot) then
            cont_loop = 2
            call nmctcf(mesh, model, sderro, hval_incr, ds_print, ds_contact)
            call mmbouc(ds_contact, 'Fric', 'Is_Convergence', loop_state_ = loop_fric_conv)
            if (.not.loop_fric_conv) then
                cont_loop = 2
                goto 500
            endif
        endif
    endif
!
! - <3> - Geometric loop
!
    if (cont_loop .le. 3) then
        if (l_loop_geom) then
            cont_loop = 3
            call nmctgo(mesh, sderro, hval_incr, ds_print, ds_contact)
            call mmbouc(ds_contact, 'Geom', 'Is_Convergence' , loop_state_ = loop_geom_conv)
            if (.not.loop_geom_conv) then
                cont_loop = 3
                goto 500
            endif
        endif
    endif
!
500 continue
!
! - Initialization of data structures for cycling detection and treatment
!
    if ((loop_cont_conv .or. loop_fric_conv .or. loop_geom_conv).and.l_cont_cont) then
        call mm_cycl_erase(ds_contact, 0, 0)
    endif
!
! - Print line
!
    call nmaffi(list_func_acti, ds_conv, ds_print, sderro, sddisc,&
                'FIXE')
!
! - New iteration in loops
!
    if (.not.loop_cont_conv .and. cont_loop .eq. 1) then
        call mmbouc(ds_contact, 'Cont', 'Incr_Counter')
    endif
    if (.not.loop_fric_conv .and. cont_loop .eq. 2) then 
        call mmbouc(ds_contact, 'Fric', 'Incr_Counter')
    endif
    if (.not.loop_geom_conv .and. cont_loop .eq. 3) then 
        call mmbouc(ds_contact, 'Geom', 'Incr_Counter')
    endif
!
! - Update loops index
!
    call mmbouc(ds_contact, 'Cont', 'Read_Counter', loop_cont_count)
    call mmbouc(ds_contact, 'Fric', 'Read_Counter', loop_fric_count)
    call mmbouc(ds_contact, 'Geom', 'Read_Counter', loop_geom_count)
!
! - Print management
!
    call nmimci(ds_print, 'CONT_NEWT', loop_cont_vali , .true._1)
    call nmimci(ds_print, 'BOUC_CONT', loop_cont_count, .true._1)
    call nmimci(ds_print, 'BOUC_FROT', loop_fric_count, .true._1)
    call nmimci(ds_print, 'BOUC_GEOM', loop_geom_count, .true._1)
!
999 continue
!
! - Set loop state
!
    call nmevcv(sderro, list_func_acti, 'FIXE')
!
end subroutine
