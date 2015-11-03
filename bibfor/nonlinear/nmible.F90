subroutine nmible(cont_loop     , model   , mesh  , ds_contact,&
                  list_func_acti, nume_dof, sdstat, sdtime    , ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmctcg.h"
#include "asterfort/nmimci.h"
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
    integer, intent(inout) :: cont_loop
    character(len=24), intent(in) :: model
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(inout) :: ds_contact
    integer, intent(in):: list_func_acti(*)
    character(len=24), intent(in) :: nume_dof
    character(len=24), intent(in) :: sdstat
    character(len=24), intent(in) :: sdtime
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algo
!
! Contact loop management - BEGIN
!
! --------------------------------------------------------------------------------------------------
!
! IO  cont_loop        : level of loop for contact (see nmtble.F90)
!                        0 - Not use (not cotnact)
!                        1 - Loop for contact status
!                        2 - Loop for friction triggers
!                        3 - Loop for geometry
! In  model            : name of model
! In  mesh             : name of mesh
! IO  ds_contact       : datastructure for contact management
! In  list_func_acti   : list of active functionnalities
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  sdstat           : datastructure for statistics
! In  sdtime           : datastructure for timers
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_loop_geom, i_loop_cont, i_loop_frot
    aster_logical :: l_loop_frot, l_loop_geom, l_loop_cont
    aster_logical :: l_pair
!
! --------------------------------------------------------------------------------------------------
!
    if (cont_loop .eq. 0) then
        goto 999
    endif
!
! - Print geometric loop iteration
!
    call mmbouc(ds_contact, 'Geom', 'READ', i_loop_geom)
    call nmimci(ds_print  , 'BOUC_GEOM', i_loop_geom, .true._1)
!
! - No pairing at first iteration (see mminit/xminit)
!
    l_pair = (i_loop_geom .gt. 1)
!
! - Contact loops
!
    l_loop_frot = isfonc(list_func_acti, 'BOUCLE_EXT_FROT')
    l_loop_geom = isfonc(list_func_acti, 'BOUCLE_EXT_GEOM')
    l_loop_cont = isfonc(list_func_acti, 'BOUCLE_EXT_CONT')
!
! - <3 - BEGIN> - Geometric loop
!
    if (cont_loop .ge. 3) then
        if (l_loop_geom) then
            cont_loop = 3
            if (l_pair) then
                call nmctcg(model   , mesh, ds_contact, sdstat, sdtime,&
                            nume_dof)
            endif
        endif
        call mmbouc(ds_contact, 'Fric', 'INIT')
        call mmbouc(ds_contact, 'Fric', 'INCR')
        call mmbouc(ds_contact, 'Fric', 'READ', i_loop_frot)
        call nmimci(ds_print   , 'BOUC_FROT', i_loop_frot, .true._1)
    endif
!
! - <2> - Friction loop
!
    if (cont_loop .ge. 2) then
        if (l_loop_frot) then
            cont_loop = 2
        endif
        call mmbouc(ds_contact, 'Cont', 'INIT')
        call mmbouc(ds_contact, 'Cont', 'INCR')
        call mmbouc(ds_contact, 'Cont', 'READ', i_loop_cont)
        call nmimci(ds_print   , 'BOUC_CONT', i_loop_cont, .true._1)
    endif
!
! - <1> - Contact loop
!
    if (cont_loop .ge. 1) then
        if (l_loop_cont) then
            cont_loop = 1
        endif
    endif
!
999 continue
!
end subroutine
