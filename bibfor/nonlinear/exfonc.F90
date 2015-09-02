subroutine exfonc(list_func_acti, ds_algopara, solver, sdcont_defi, sddyna,&
                  mate)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/getvtx.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
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
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: solver
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: mate
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Initializations
!
! No compatible functionnalities
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  ds_algopara      : datastructure for algorithm parameters
! In  solver           : datastructure for solver parameters 
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sddyna           : dynamic parameters datastructure
! In  mate             : name of material characteristics (field)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: reac_incr
    integer :: n1
    aster_logical :: lcont, lallv, lctcc, lctcd, lpena, leltc
    aster_logical :: lpilo, lreli, lmacr, lunil
    aster_logical :: lmvib, lflam, lexpl, lxfem, lmodim
    aster_logical :: lrcmk, lgcpc, lpetsc, lamg, lsyme, limpex
    aster_logical :: londe, ldyna, lgrot, ltheta, lnkry
    aster_logical :: lener, lproj, lmatdi, lldsp, lctgcp, lcomp
    character(len=24) :: typilo, metres
    character(len=16) :: reli_meth, matrix_pred
    character(len=3) :: mfdet, syme
    character(len=24), pointer :: slvk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    lxfem = isfonc(list_func_acti,'XFEM')
    lctcc = isfonc(list_func_acti,'CONT_CONTINU')
    lctcd = isfonc(list_func_acti,'CONT_DISCRET')
    lcont = isfonc(list_func_acti,'CONTACT')
    lunil = isfonc(list_func_acti,'LIAISON_UNILATER')
    lpilo = isfonc(list_func_acti,'PILOTAGE')
    lreli = isfonc(list_func_acti,'RECH_LINE')
    lmacr = isfonc(list_func_acti,'MACR_ELEM_STAT')
    lmvib = isfonc(list_func_acti,'MODE_VIBR')
    lflam = isfonc(list_func_acti,'CRIT_STAB')
    londe = ndynlo(sddyna,'ONDE_PLANE')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lexpl = isfonc(list_func_acti,'EXPLICITE')
    lgrot = isfonc(list_func_acti,'GD_ROTA')
    ltheta = ndynlo(sddyna,'THETA_METHODE')
    limpex = isfonc(list_func_acti,'IMPLEX')
    lnkry = isfonc(list_func_acti,'NEWTON_KRYLOV')
    lener = isfonc(list_func_acti,'ENERGIE')
    lproj = isfonc(list_func_acti,'PROJ_MODAL')
    lmatdi = isfonc(list_func_acti,'MATR_DISTRIBUEE')
    leltc = isfonc(list_func_acti,'ELT_CONTACT')
    lcomp = isfonc(list_func_acti,'RESI_COMP')
    lgcpc = isfonc(list_func_acti,'GCPC')
    lpetsc = isfonc(list_func_acti,'PETSC')
    lldsp = isfonc(list_func_acti,'LDLT_SP')
!
    call jeveuo(solver//'.SLVK', 'E', vk24=slvk)
    metres = slvk(1)
!
! - Get algorithm parameters
!
    reac_incr   = ds_algopara%reac_incr
    matrix_pred = ds_algopara%matrix_pred
    reli_meth   = ds_algopara%line_search%method
!
! --- TYPE DE SOLVEUR
!
    lrcmk = slvk(4) .eq. 'RCMK'
    lamg = ((slvk(2).eq.'ML') .or. (slvk(2).eq.'BOOMER'))
    call getvtx('SOLVEUR', 'SYME', iocc=1, scal=syme)
    lsyme = syme.eq.'OUI'
!
! --- CONTACT DISCRET
!
    if (lctcd) then
        lmodim = cfdisl(sdcont_defi,'MODI_MATR_GLOB')
        lallv = cfdisl(sdcont_defi,'ALL_VERIF')
        lpena = cfdisl(sdcont_defi,'CONT_PENA')
        lctgcp = cfdisl(sdcont_defi,'CONT_GCP')
        if (lpilo) then
            call utmess('F', 'MECANONLINE_43')
        endif
        if (lreli .and. (.not.lallv)) then
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
!       ON SUGGERE SYME='OUI' AVEC LE CONTACT DISCRET
        if (.not.(lsyme.or.lallv)) then
            call utmess('A', 'CONTACT_1')
        endif
        if ((lmvib.or.lflam) .and. lmodim) then
            call utmess('F', 'MECANONLINE5_14')
        endif
    endif
!
! --- CONTACT CONTINU
!
    if (lctcc) then
        if (lpilo .and. (.not.lxfem)) then
!         LEVEE D INTERDICTION TEMPORAIRE POUR X-FEM
            call utmess('F', 'MECANONLINE3_92')
        endif
        if (lreli) then
            call utmess('F', 'MECANONLINE3_91')
        endif
        if (lrcmk) then
            call utmess('F', 'MECANONLINE3_93', sk=slvk(1))
        endif
        if (lamg) then
            call utmess('F', 'MECANONLINE3_97', sk=slvk(2))
        endif
        if (lpetsc .and. lmatdi) then
            call utmess('F', 'MECANONLINE3_98')
        endif
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) then
        if (lpilo) then
            call utmess('F', 'MECANONLINE3_94')
        endif
        if (lreli) then
            call utmess('A', 'MECANONLINE3_95')
        endif
        if (lgcpc .or. lpetsc) then
            call utmess('F', 'MECANONLINE3_96', sk=slvk(1))
        endif
!       ON SUGGERE SYME='OUI' AVEC LIAISON_UNILATER
        if (.not.lsyme) then
            call utmess('A', 'UNILATER_1')
        endif
    endif
!
! --- CALCUL DE MODES/FLAMBEMENT: PAS GCPC/PETSC
!
    if (lmvib .or. lflam) then
        if (lgcpc .or. lpetsc) then
            call utmess('F', 'FACTOR_52', sk=slvk(1))
        endif
        if (leltc) then
            call utmess('F', 'MECANONLINE5_3')
        endif
    endif
!
! --- EXPLICITE
!
    if (lexpl) then
        if (lcont) then
            call utmess('F', 'MECANONLINE5_22')
        endif
        if (lunil) then
            call utmess('F', 'MECANONLINE5_23')
        endif
        if (lgrot) then
            call utmess('A', 'MECANONLINE5_24')
        endif
    endif
!
! --- DYNAMIQUE
!
    if (ldyna) then
        if (lcomp) then
            call utmess('F', 'MECANONLINE5_53')
        endif
        if (lpilo) then
            call utmess('F', 'MECANONLINE5_25')
        endif
        if (ltheta) then
            if (lgrot) then
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
! --- PILOTAGE
!
    if (lpilo) then
        call getvtx('PILOTAGE', 'TYPE', iocc=1, scal=typilo, nbret=n1)
        if (lreli) then
            if (typilo .eq. 'DDL_IMPO') then
                call utmess('F', 'MECANONLINE5_34')
            endif
        endif
        if ((matrix_pred.eq.'DEPL_CALCULE') .or. (matrix_pred .eq.'EXTRAPOLE')) then
            call utmess('F', 'MECANONLINE5_36')
        endif
!
!       --- VERIFICATION QUE LES VARIABLES DE COMMANDE NE DEPENDENT PAS DU TEMPS
        call dismoi('VARC_F_INST', mate, 'CHAM_MATER', repk=mfdet)
        if (mfdet .eq. 'OUI') then
            call utmess('F', 'CALCULEL2_58', nk=1, valk=mate(1:8))
        endif
!
    endif
    if (lreli) then
        if ((reli_meth.eq.'PILOTAGE') .and. (.not.lpilo)) then
            call utmess('F', 'MECANONLINE5_35')
        endif
    endif
!
! --- NEWTON_KRYLOV
!
    if (lnkry) then
        if (lpilo) then
            call utmess('F', 'MECANONLINE5_48')
        endif
        if ((.not.lgcpc) .and. (.not.lpetsc)) then
            call utmess('F', 'MECANONLINE5_51')
        endif
    endif
!
! --- ENERGIES
!
    if (lener) then
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
end subroutine
