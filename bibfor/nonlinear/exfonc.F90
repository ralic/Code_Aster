subroutine exfonc(list_func_acti, ds_algopara, solver, ds_contact, sddyna,&
                  mate)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/getvtx.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
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
    character(len=19), intent(in) :: solver
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: mate
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Initializations
!
! Check compatibility of some functionnalities
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  ds_algopara      : datastructure for algorithm parameters
! In  solver           : datastructure for solver parameters 
! In  ds_contact       : datastructure for contact management
! In  sddyna           : dynamic parameters datastructure
! In  mate             : name of material characteristics (field)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: reac_incr, reac_iter
    aster_logical :: l_cont, lallv, l_cont_cont, l_cont_disc, lpena, leltc, l_cont_lac, l_iden_rela
    aster_logical :: l_pilo, l_line_search, lmacr, l_unil, l_diri_undead, l_cont_xfem
    aster_logical :: l_vibr_mode, l_buckling, lexpl, lxfem, lmodim, l_mult_front
    aster_logical :: lgcpc, lpetsc, lamg, limpex, l_matr_rigi_syme
    aster_logical :: londe, l_dyna, l_grot_gdep, ltheta, l_newt_krylov, l_mumps, l_rom
    aster_logical :: l_energy, lproj, lmatdi, lldsp, lctgcp, l_comp_rela, lammo
    character(len=24) :: typilo, metres
    character(len=16) :: reli_meth, matrix_pred
    character(len=3) :: mfdet
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: slvi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Active functionnalites
!
    lxfem           = isfonc(list_func_acti,'XFEM')
    l_cont_cont     = isfonc(list_func_acti,'CONT_CONTINU')
    l_cont_disc     = isfonc(list_func_acti,'CONT_DISCRET')
    l_cont_xfem     = isfonc(list_func_acti,'CONT_XFEM')
    l_cont          = isfonc(list_func_acti,'CONTACT')
    l_cont_lac      = isfonc(list_func_acti,'CONT_LAC')
    l_unil          = isfonc(list_func_acti,'LIAISON_UNILATER')
    l_pilo          = isfonc(list_func_acti,'PILOTAGE')
    l_line_search   = isfonc(list_func_acti,'RECH_LINE')
    lmacr           = isfonc(list_func_acti,'MACR_ELEM_STAT')
    l_vibr_mode     = isfonc(list_func_acti,'MODE_VIBR')
    l_buckling      = isfonc(list_func_acti,'CRIT_STAB')
    londe           = ndynlo(sddyna,'ONDE_PLANE')
    l_dyna          = ndynlo(sddyna,'DYNAMIQUE')
    lexpl           = isfonc(list_func_acti,'EXPLICITE')
    l_grot_gdep     = isfonc(list_func_acti,'GD_ROTA')
    ltheta          = ndynlo(sddyna,'THETA_METHODE')
    lammo           = ndynlo(sddyna,'AMOR_MODAL')
    limpex          = isfonc(list_func_acti,'IMPLEX')
    l_newt_krylov   = isfonc(list_func_acti,'NEWTON_KRYLOV')
    l_rom           = isfonc(list_func_acti,'ROM')
    l_energy        = isfonc(list_func_acti,'ENERGIE')
    lproj           = isfonc(list_func_acti,'PROJ_MODAL')
    lmatdi          = isfonc(list_func_acti,'MATR_DISTRIBUEE')
    leltc           = isfonc(list_func_acti,'ELT_CONTACT')
    l_comp_rela     = isfonc(list_func_acti,'RESI_COMP')
    lgcpc           = isfonc(list_func_acti,'GCPC')
    lpetsc          = isfonc(list_func_acti,'PETSC')
    lldsp           = isfonc(list_func_acti,'LDLT_SP')
    l_mumps         = isfonc(list_func_acti,'MUMPS')
    l_mult_front    = isfonc(list_func_acti,'MULT_FRONT')
    l_diri_undead   = isfonc(list_func_acti,'DIRI_UNDEAD')
!
! - Get algorithm parameters
!
    reac_iter        = ds_algopara%reac_iter
    reac_incr        = ds_algopara%reac_incr
    matrix_pred      = ds_algopara%matrix_pred
    reli_meth        = ds_algopara%line_search%method
    l_matr_rigi_syme = ds_algopara%l_matr_rigi_syme
!
! - Get solver parameters
!
    call jeveuo(solver//'.SLVK', 'E', vk24=slvk)
    call jeveuo(solver//'.SLVI', 'E', vi  =slvi)
    metres = slvk(1)
    lamg  = ((slvk(2).eq.'ML') .or. (slvk(2).eq.'BOOMER'))
!
! - Contact (DISCRETE)
!
    if (l_cont_disc) then
        lmodim = cfdisl(ds_contact%sdcont_defi,'MODI_MATR_GLOB')
        lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
        lpena = cfdisl(ds_contact%sdcont_defi,'CONT_PENA')
        lctgcp = cfdisl(ds_contact%sdcont_defi,'CONT_GCP')
        if (l_pilo) then
            call utmess('F', 'MECANONLINE_43')
        endif
        if (l_line_search .and. (.not.lallv)) then
            call utmess('A', 'MECANONLINE3_89')
        endif
        if (lgcpc .or. lpetsc) then
            if (.not.(lallv.or.lpena.or.lctgcp)) then
                call utmess('F', 'MECANONLINE3_90', sk=metres)
            endif
            if (lctgcp .and. .not.lldsp) then
                call utmess('F', 'MECANONLINE3_88')
            endif
        endif
        if (reac_incr .eq. 0) then
            if (lmodim) then
                call utmess('F', 'CONTACT_88')
            endif
        endif
        if (.not.(l_matr_rigi_syme.or.lallv)) then
            call utmess('A', 'CONTACT_1')
        endif
        if ((l_vibr_mode.or.l_buckling) .and. lmodim) then
            call utmess('F', 'MECANONLINE5_14')
        endif
    endif
!
! - Contact (CONTINUE)
!
    if (l_cont_cont) then
        if (l_pilo .and. (.not.lxfem)) then
            call utmess('F', 'MECANONLINE3_92')
        endif
        if (l_line_search) then
            call utmess('F', 'MECANONLINE3_91')
        endif
        if (lamg) then
            call utmess('F', 'MECANONLINE3_97', sk=slvk(2))
        endif
        if (lpetsc .and. lmatdi) then
            call utmess('F', 'MECANONLINE3_98')
        endif
        if (lammo) then
            call utmess('F', 'MECANONLINE3_93')
        endif
    endif
!
! - Contact (XFEM)
!
    if (l_cont_xfem) then
        l_iden_rela = ds_contact%l_iden_rela
        if (l_iden_rela .and. l_mult_front) then
            call utmess('F', 'MECANONLINE3_99')
        endif
    endif
!
! - Contact (LAC)
!
    if (l_cont_lac) then
        l_iden_rela = ds_contact%l_iden_rela
        if (l_iden_rela .and. l_mult_front) then
            call utmess('F', 'MECANONLINE3_99')
        endif
    endif
!
! - Unilateral link
!
    if (l_unil) then
        if (l_pilo) then
            call utmess('F', 'MECANONLINE3_94')
        endif
        if (l_line_search) then
            call utmess('A', 'MECANONLINE3_95')
        endif
        if (lgcpc .or. lpetsc) then
            call utmess('F', 'MECANONLINE3_96', sk=slvk(1))
        endif
        if (.not.l_matr_rigi_syme) then
            call utmess('A', 'UNILATER_1')
        endif
    endif
!
! - Dirichlet undead loads
!
    if (l_diri_undead) then
        if (l_pilo) then
            call utmess('F', 'MECANONLINE5_42')
        endif
        if (l_line_search) then
            call utmess('F', 'MECANONLINE5_39')
        endif
        if (l_dyna) then
            call utmess('F', 'MECANONLINE5_40')
        endif
        if (reac_iter.ne.1) then
            call utmess('F', 'MECANONLINE5_41')
        endif
    endif
!
! - Post-treatment (buckling, ...)
!
    if (l_vibr_mode .or. l_buckling) then
        if (lgcpc .or. lpetsc) then
            call utmess('F', 'FACTOR_52', sk=slvk(1))
        endif
        if (leltc) then
            call utmess('F', 'MECANONLINE5_3')
        endif
    endif
!
! - Explicit solver
!
    if (lexpl) then
        if (l_cont) then
            call utmess('F', 'MECANONLINE5_22')
        endif
        if (l_unil) then
            call utmess('F', 'MECANONLINE5_23')
        endif
        if (l_grot_gdep) then
            call utmess('A', 'MECANONLINE5_24')
        endif
    endif
!
! - Dynamic
!
    if (l_dyna) then
        if (l_comp_rela) then
            call utmess('F', 'MECANONLINE5_53')
        endif
        if (l_pilo) then
            call utmess('F', 'MECANONLINE5_25')
        endif
        if (ltheta) then
            if (l_grot_gdep) then
                call utmess('F', 'MECANONLINE5_27')
            endif
        endif
        if (lxfem) then
            call utmess('F', 'MECANONLINE5_28')
        endif
        if (limpex) then
            call utmess('F', 'MECANONLINE5_33')
        endif
    endif
!
! - Continuation methods (PILOTAGE)
!
    if (l_pilo) then
        call getvtx('PILOTAGE', 'TYPE', iocc=1, scal=typilo)
        if (l_line_search) then
            if (typilo .eq. 'DDL_IMPO') then
                call utmess('F', 'MECANONLINE5_34')
            endif
        endif
        if ((matrix_pred.eq.'DEPL_CALCULE') .or. (matrix_pred .eq.'EXTRAPOLE')) then
            call utmess('F', 'MECANONLINE5_36')
        endif
        call dismoi('VARC_F_INST', mate, 'CHAM_MATER', repk=mfdet)
        if (mfdet .eq. 'OUI') then
            call utmess('F', 'CALCULEL2_58', nk=1, valk=mate(1:8))
        endif
    endif
    if (l_line_search) then
        if ((reli_meth.eq.'PILOTAGE') .and. (.not.l_pilo)) then
            call utmess('F', 'MECANONLINE5_35')
        endif
    endif
!
! - NEWTON_KRYLOV
!
    if (l_newt_krylov) then
        if (l_pilo) then
            call utmess('F', 'MECANONLINE5_48')
        endif
        if ((.not.lgcpc) .and. (.not.lpetsc)) then
            call utmess('F', 'MECANONLINE5_51')
        endif
    endif
!
! - ROM
!
    if (l_rom) then
        if (l_pilo) then
            call utmess('F', 'ROM2_6')
        endif
        if (l_line_search) then
            call utmess('F', 'ROM2_4')
        endif
        if (l_dyna) then
            call utmess('F', 'ROM2_7')
        endif
        if (l_cont) then
            call utmess('F', 'ROM2_8')
        endif
    endif
!
! - Energy
!
    if (l_energy) then
        if (lproj) then
            call utmess('F', 'MECANONLINE5_6')
        endif
        if (lmatdi) then
            call utmess('F', 'MECANONLINE5_8')
        endif
        if (leltc) then
            call utmess('F', 'MECANONLINE5_15')
        endif
    endif
!
! --- SI ON A BESOIN DE FACTORISER SIMULTANEMENT DEUX MATRICES AVEC LE SOLVEUR MUMPS ON LUI
!     SIGNALE AFIN QU'IL OPTIMISE AU MIEUX LA MEMOIRE POUR CHACUNES D'ELLES.
!     CE N'EST VRAIMENT UTILE QUE SI SOLVEUR/GESTION_MEMOIRE='AUTO'.
!
    if (l_mumps) then
        if (l_vibr_mode .or. l_buckling) then
            ASSERT(slvi(6) .ge. 0)
            slvi(6)=2
        endif
    endif
!
    call jedema()
!
end subroutine
