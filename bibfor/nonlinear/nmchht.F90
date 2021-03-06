subroutine nmchht(model    , mate     , cara_elem  , ds_constitutive,&
                  list_load, nume_dof , varc_refe  , list_func_acti, ds_measure,&
                  sddyna   , sddisc   , sdnume     , ds_contact,&
                  hval_incr, hval_algo, hval_veasse, hval_measse   , ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/diinst.h"
#include "asterfort/isfonc.h"
#include "asterfort/mecact.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmassv.h"
#include "asterfort/nmcalv.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmmacv.h"
#include "asterfort/copisd.h"
#include "asterfort/nmvcex.h"
#include "asterfort/utmess.h"
#include "asterfort/nd_mstp_time.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: varc_refe
    integer, intent(in) :: list_func_acti(*)
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sdnume
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_veasse(*)
    character(len=19), intent(in) :: hval_measse(*)
    type(NL_DS_InOut), intent(in) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Dynamic
!
! Compute previous second member for multi-step schemes
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  ds_constitutive  : datastructure for constitutive laws management
! In  nume_dof         : name of numbering (NUME_DDL)
! In  list_load        : name of datastructure for list of loads
! In  varc_refe        : name of reference command variables vector
! In  list_func_acti   : list of active functionnalities
! IO  ds_measure       : datastructure for measure and statistics management
! In  sddyna           : dynamic parameters datastructure
! In  sddisc           : datastructure for time discretization
! In  sdnume           : datastructure for dof positions
! In  ds_contact       : datastructure for contact management
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_veasse      : hat-variable for vectors (node fields)
! In  hval_measse      : hat-variable for matrix
! In  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: londe, llapl, ldidi, lviss, lsstf, l_comp_mstp, lmacr
    character(len=8) :: k8bid
    character(len=16) :: k16bla
    character(len=19) :: vefint, vedido
    character(len=19) :: vefedo, veondp, vedidi, velapl, vesstf
    character(len=19) :: cnfedo, cndidi, cnfint
    character(len=19) :: cndido, cncine, cnviss
    character(len=19) :: cnondp, cnlapl, cnsstf
    character(len=19) :: matr_sstr, disp_prev
    character(len=24) :: cnsstr, codere
    character(len=19) :: varc_prev, varc_curr, time_prev, time_curr
    real(kind=8) :: time_init, time_prev_step
    integer :: iterat, ldccvg
!
! --------------------------------------------------------------------------------------------------
!
    k8bid  = ' '
    k16bla = ' '
    iterat = 0
    codere = '&&NMCHHT.CODERE'
!
! - Active functionnalities
!
    londe  = ndynlo(sddyna,'ONDE_PLANE')
    lviss  = ndynlo(sddyna,'VECT_ISS')
    lsstf  = isfonc(list_func_acti,'SOUS_STRUC')
    llapl  = isfonc(list_func_acti,'LAPLACE')
    ldidi  = isfonc(list_func_acti,'DIDI')
    lmacr  = isfonc(list_func_acti,'MACR_ELEM_STAT')
!
! - Initial time
!
    time_init = diinst(sddisc,0)
!
! - Get previous time
!
    call nd_mstp_time(ds_inout, list_func_acti, time_prev_step, l_comp_mstp)
!
! - Protection
!
    if (abs(time_prev_step-time_init).le.r8prem()) then
        l_comp_mstp = .false.
        call utmess('A','DYNAMIQUE_52')
    endif
!
! - No computation
!
    if (.not.l_comp_mstp) then
        goto 99
    endif
!
! - Create <CARTE> for time
!
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
    call nmvcex('INST', varc_prev, time_prev)
    call copisd('CHAMP_GD', 'V', time_prev, varc_curr(1:14)//'.INST')
    call nmvcex('INST', varc_curr, time_curr)

    call mecact('V', time_prev, 'MODELE', model(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=time_prev_step)
    call mecact('V', time_curr, 'MODELE', model(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=time_init)
!
! - Get fields from hat-variables - 
!
    call ndynkk(sddyna, 'OLDP_VEFEDO', vefedo)
    call ndynkk(sddyna, 'OLDP_VEDIDO', vedido)
    call ndynkk(sddyna, 'OLDP_VEDIDI', vedidi)
    call ndynkk(sddyna, 'OLDP_VEFINT', vefint)
    call ndynkk(sddyna, 'OLDP_VEONDP', veondp)
    call ndynkk(sddyna, 'OLDP_VELAPL', velapl)
    call ndynkk(sddyna, 'OLDP_VESSTF', vesstf)
    call ndynkk(sddyna, 'OLDP_CNFEDO', cnfedo)
    call ndynkk(sddyna, 'OLDP_CNDIDO', cndido)
    call ndynkk(sddyna, 'OLDP_CNDIDI', cndidi)
    call ndynkk(sddyna, 'OLDP_CNFINT', cnfint)
    call ndynkk(sddyna, 'OLDP_CNONDP', cnondp)
    call ndynkk(sddyna, 'OLDP_CNLAPL', cnlapl)
    call ndynkk(sddyna, 'OLDP_CNCINE', cncine)
    call ndynkk(sddyna, 'OLDP_CNVISS', cnviss)
    call ndynkk(sddyna, 'OLDP_CNSSTF', cnsstf)
    call nmchex(hval_veasse, 'VEASSE', 'CNSSTR', cnsstr(1:19))
    call nmchex(hval_measse, 'MEASSE', 'MESSTR', matr_sstr)
    call nmchex(hval_incr  , 'VALINC', 'DEPMOI', disp_prev)
!
! - Forces from macro-elements
! 
    if (lmacr) then
        call nmmacv(disp_prev, matr_sstr, cnsstr)
    endif  
!
! - Internal forces
!
    call nmfint(model         , mate  , cara_elem, varc_refe, ds_constitutive,&
                list_func_acti, iterat, sddyna   , ds_measure,&
                hval_incr, hval_algo  , ldccvg   , vefint)
    call nmaint(nume_dof, list_func_acti, ds_contact, hval_veasse, vefint,&
                cnfint  , sdnume)
!
! - Given displacements
!
    call nmcalv('CNDIDO'       , model    , list_load, mate      , cara_elem     ,&
                ds_constitutive, nume_dof , varc_refe, ds_measure, time_prev_step,&
                time_init      , hval_incr, hval_algo, sddyna    , k16bla        ,&
                vedido)
    call nmassv('CNDIDO'       , model      , list_load      , mate     , cara_elem  ,&
                ds_constitutive, nume_dof   , time_prev_step , time_init, &
                sddyna         , ds_measure , hval_incr, varc_refe  ,&
                ds_inout       , hval_measse, vedido  , cndido)
    if (ldidi) then
        call nmcalv('CNDIDI'       , model    , list_load, mate     , cara_elem     ,&
                    ds_constitutive, nume_dof , varc_refe, ds_measure, time_prev_step,&
                    time_init      , hval_incr, hval_algo, sddyna   , k16bla        ,&
                    vedidi)
        call nmassv('CNDIDI'       , model      , list_load      , mate     , cara_elem  ,&
                    ds_constitutive, nume_dof   , time_prev_step , time_init, &
                    sddyna         , ds_measure, hval_incr, varc_refe  ,&
                    ds_inout       , hval_measse, vedidi         , cndidi)
    endif
!
! - Laplace forces
!
    if (llapl) then
        call nmcalv('CNLAPL'       , model    , list_load, mate     , cara_elem     ,&
                    ds_constitutive, nume_dof , varc_refe, ds_measure, time_prev_step,&
                    time_init      , hval_incr, hval_algo, sddyna   , k16bla        ,&
                    velapl)
        call nmassv('CNLAPL'       , model      , list_load      , mate     , cara_elem  ,&
                    ds_constitutive, nume_dof   , time_prev_step , time_init, &
                    sddyna         , ds_measure, hval_incr, varc_refe  ,&
                    ds_inout       , hval_measse, velapl  , cnlapl)
    endif
!
! - Plane wave
!
    if (londe) then
        call nmcalv('CNONDP'       , model    , list_load, mate     , cara_elem     ,&
                    ds_constitutive, nume_dof , varc_refe, ds_measure, time_prev_step,&
                    time_init      , hval_incr, hval_algo, sddyna   , k16bla        ,&
                    veondp)
        call nmassv('CNONDP'       , model      , list_load      , mate     , cara_elem  ,&
                    ds_constitutive, nume_dof   , time_prev_step , time_init, &
                    sddyna         , ds_measure, hval_incr, varc_refe  ,&
                    ds_inout       , hval_measse, veondp, cnondp)
    endif
!
! - Substructuring
!
    if (lsstf) then
        call nmcalv('CNSSTF'       , model    , list_load, mate     , cara_elem     ,&
                    ds_constitutive, nume_dof , varc_refe, ds_measure, time_prev_step,&
                    time_init      , hval_incr, hval_algo, sddyna   , k16bla        ,&
                    vesstf)
        call nmassv('CNSSTF'       , model      , list_load      , mate     , cara_elem  ,&
                    ds_constitutive, nume_dof   , time_prev_step , time_init, &
                    sddyna         , ds_measure         , hval_incr, varc_refe  ,&
                    ds_inout       , hval_measse, vesstf  , cnsstf)
    endif
!
! - FORCE_SOL
!
    if (lviss) then
        call nmassv('CNVISS'       , model      , list_load      , mate     , cara_elem  ,&
                    ds_constitutive, nume_dof   , time_prev_step , time_init, &
                    sddyna         , ds_measure , hval_incr, varc_refe  ,&
                    ds_inout       , hval_measse, k16bla  , cnviss)
    endif
!
! - Neumann forces
!
    call nmcalv('CNFEDO'       , model    , list_load, mate     , cara_elem     ,&
                ds_constitutive, nume_dof , varc_refe, ds_measure   , time_prev_step,&
                time_init      , hval_incr, hval_algo, sddyna   , k16bla        ,&
                vefedo)
    call nmassv('CNFEDO'       , model      , list_load      , mate     , cara_elem  ,&
                ds_constitutive, nume_dof   , time_prev_step , time_init, &
                sddyna         , ds_measure , hval_incr, varc_refe  ,&
                ds_inout       , hval_measse, vefedo  , cnfedo)
!
! - Given displacements (AFFE_CHAR_CINE)
!
    call nmassv('CNCINE'       , model      , list_load      , mate     , cara_elem  ,&
                ds_constitutive, nume_dof   , time_prev_step , time_init, &
                sddyna         , ds_measure , hval_incr, varc_refe  ,&
                ds_inout       , hval_measse, k16bla  , cncine)
!
99  continue
end subroutine
