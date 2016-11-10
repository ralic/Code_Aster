subroutine nmfocc(phase      , model     , mate     , nume_dof , list_func_acti,&
                  ds_contact , ds_measure, hval_algo, hval_incr, hval_veelem   ,&
                  hval_veasse, ds_constitutive)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assvec.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/lccsst.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmelcv.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/nmvcex.h"
#include "asterfort/vtaxpy.h"
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
    character(len=10), intent(in) :: phase
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: list_func_acti(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_veelem(*)
    character(len=19), intent(in) :: hval_veasse(*)
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algo
!
! Continue/XFEM/LAC methods - Compute second member
!
! --------------------------------------------------------------------------------------------------
!
! In  phase            : phase
!               'PREDICTION' - PHASE DE PREDICTION
!               'CONVERGENC' - PHASE DE CONVERGENCE
!               'CORRECTION' - PHASE DE CORRECTION
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  nume_dof         : name of numbering (NUME_DDL)
! In  list_func_acti   : list of active functionnalities
! In  ds_contact       : datastructure for contact management
! IO  ds_measure       : datastructure for measure and statistics management
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_veelem      : hat-variable for elementary vectors
! In  hval_veasse      : hat-variable for vectors (node fields)
! In  ds_constitutive  : datastructure for constitutive laws management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_elem_cont, l_elem_frot, l_all_verif, l_newt_cont, l_newt_geom, l_cont_lac
    aster_logical :: l_xthm
    character(len=8) :: mesh
    character(len=19) :: vect_elem_cont, vect_elem_frot
    character(len=19) :: vect_asse_frot, vect_asse_cont, vect_asse_fint
    character(len=19) :: disp_prev, disp_cumu_inst, vite_prev, acce_prev, vite_curr
    character(len=19) :: varc_prev, varc_curr, time_prev, time_curr
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CALCUL DU SECOND MEMBRE'
    endif
!
! - Active functionnalities
!
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    l_elem_cont = isfonc(list_func_acti,'ELT_CONTACT')
    l_elem_frot = isfonc(list_func_acti,'ELT_FROTTEMENT')
    l_all_verif = isfonc(list_func_acti,'CONT_ALL_VERIF')
    l_newt_cont = isfonc(list_func_acti,'CONT_NEWTON')
    l_newt_geom = isfonc(list_func_acti,'GEOM_NEWTON')
    l_cont_lac  = isfonc(list_func_acti,'CONT_LAC')
    l_xthm = isfonc(list_func_acti,'THM')
!
! - Get fields
!
    call nmchex(hval_veasse, 'VEASSE', 'CNFINT', vect_asse_fint)
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(hval_incr, 'VALINC', 'VITMOI', vite_prev)
    call nmchex(hval_incr, 'VALINC', 'ACCMOI', acce_prev)
    call nmchex(hval_incr, 'VALINC', 'VITPLU', vite_curr)
    call nmchex(hval_algo, 'SOLALG', 'DEPDEL', disp_cumu_inst)
    call nmchex(hval_veelem, 'VEELEM', 'CNELTC', vect_elem_cont)
    call nmchex(hval_veelem, 'VEELEM', 'CNELTF', vect_elem_frot)
    call nmchex(hval_veasse, 'VEASSE', 'CNELTC', vect_asse_cont)
    call nmchex(hval_veasse, 'VEASSE', 'CNELTF', vect_asse_frot)
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
    call nmvcex('INST', varc_prev, time_prev)
    call nmvcex('INST', varc_curr, time_curr)
!
! - Generalized Newton: contact status evaluate before
!
    if ((phase.eq.'CONVERGENC') .and. (l_newt_cont .or. l_newt_geom)) then
        goto 999
    endif
!
! - Prepare internal forces: no previous contact forces
!
    if (phase .eq. 'CORRECTION') then
        if (l_elem_cont .and. (.not.l_all_verif)) then
            call vtaxpy(-1.d0, vect_asse_cont, vect_asse_fint)
        endif
        if (l_elem_frot .and. (.not.l_all_verif) .and. (.not.l_xthm)) then
            call vtaxpy(-1.d0, vect_asse_frot, vect_asse_fint)
        endif
    endif
!
! - Compute contact forces
!
    if (l_elem_cont .and. (.not.l_all_verif)) then
        call nmtime(ds_measure, 'Init'  , 'Cont_Elem')
        call nmtime(ds_measure, 'Launch', 'Cont_Elem')
        call nmelcv('CONT'        , mesh     , model    , mate     , ds_contact    ,&
                    disp_prev     , vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                    vect_elem_cont, time_prev, time_curr, ds_constitutive, list_func_acti)
        call assvec('V', vect_asse_cont, 1, vect_elem_cont, [1.d0],&
                    nume_dof, ' ', 'ZERO', 1)
        call nmtime(ds_measure, 'Stop', 'Cont_Elem')
        call nmrinc(ds_measure, 'Cont_Elem')
        if (niv .eq. 2) then
            call nmdebg('VECT', vect_asse_cont, ifm)
        endif
    endif
!
! - Compute friction forces
!
    if (l_elem_frot .and. (.not.l_all_verif) .and. (.not.l_xthm)) then
        call nmtime(ds_measure, 'Init'  , 'Cont_Elem')
        call nmtime(ds_measure, 'Launch', 'Cont_Elem')
        call nmelcv('FROT'        , mesh     , model    , mate     , ds_contact    ,&
                    disp_prev     , vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                    vect_elem_frot, time_prev, time_curr, ds_constitutive, list_func_acti)
        call assvec('V', vect_asse_frot, 1, vect_elem_frot, [1.d0],&
                    nume_dof, ' ', 'ZERO', 1)
        call nmtime(ds_measure, 'Stop', 'Cont_Elem')
        call nmrinc(ds_measure, 'Cont_Elem')
        if (niv .eq. 2) then
            call nmdebg('VECT', vect_asse_frot, ifm)
        endif
    endif
!
! - Prepare internal forces: add new contact forces
!
    if (phase .eq. 'CORRECTION') then
        if (l_elem_cont .and. (.not.l_all_verif)) then
            call vtaxpy(+1.d0, vect_asse_cont, vect_asse_fint)
        endif
        if (l_elem_frot .and. (.not.l_all_verif) .and. (.not.l_xthm)) then
            call vtaxpy(+1.d0, vect_asse_frot, vect_asse_fint)
        endif
    endif
!
! - Special post-treatment for LAC contact method
!
    if (l_cont_lac) then
        call lccsst(ds_contact, vect_asse_cont)
    end if
!
999 continue
!
end subroutine
