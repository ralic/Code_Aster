subroutine nmdoet(model , compor, list_func_acti, nume_ddl   , sdpilo  ,&
                  sddyna, sdcriq, hval_algo     , l_acce_zero, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndloam.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdoin.h"
#include "asterfort/nmetl1.h"
#include "asterfort/nmetl2.h"
#include "asterfort/nmetl3.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: sdcriq
    character(len=24), intent(in) :: nume_ddl
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdpilo
    character(len=19), intent(in) :: hval_algo(*)
    integer, intent(in) :: list_func_acti(*)
    aster_logical, intent(out) :: l_acce_zero
    type(NL_DS_InOut), intent(inout) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Read initial state
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  compor           : name of <CARTE> COMPOR
! In  list_func_acti   : list of active functionnalities
! In  nume_ddl         : name of nume_ddl object (numbering equation)
! In  sddyna           : dynamic parameters datastructure
! In  hval_algo        : hat-variable for algorithms fields
! In  sdpilo           : continuation ("PILOTAGE") parameters datastructure
! In  sdcriq           : datastructure for quality indicators
! IO  ds_inout         : datastructure for input/output management
! Out l_acce_zero      : .true. if initial acceleration must been computed
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field
    character(len=24) :: field_type
    aster_logical :: l_stin_evol, l_state_init
    integer :: nb_equa, init_nume, iret, i, i_field
    character(len=8) :: calcri, stin_evol
    character(len=24) :: typpil, typsel
    character(len=19) :: depold
    character(len=24) :: champ1, champ2, dep2, dep1, errthm
    integer :: jinst, jerrt
    aster_logical :: l_pilo, lpiarc, l_cont_cont
    aster_logical :: l_expl_gene, l_reuse, l_erre_thm
    aster_logical :: l_zero, l_acti, l_ener
    real(kind=8) :: coefav, init_time
    real(kind=8), pointer :: plir(:) => null()
    character(len=24), pointer :: pltk(:) => null()
    real(kind=8), pointer :: vdep1(:) => null()
    real(kind=8), pointer :: vdep2(:) => null()
    real(kind=8), pointer :: depol(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    dep1 = '&&CNPART.CHP1'
    dep2 = '&&CNPART.CHP2'
    l_acce_zero  = .false.
    lpiarc       = .false.
    call dismoi('NB_EQUA', nume_ddl, 'NUME_DDL', repi=nb_equa)
!
! - Model can compute rigidity ?
!
    call dismoi('CALC_RIGI', model, 'MODELE', repk=calcri)
    if (calcri .ne. 'OUI') then
        call utmess('F', 'CALCULEL2_65', sk=model)
    endif
!
! - Parameters from input/output datastructure
!
    nb_field = ds_inout%nb_field
!
! - Active functionnalities
!
    l_pilo      = isfonc(list_func_acti,'PILOTAGE')
    l_cont_cont = isfonc(list_func_acti,'CONT_CONTINU')
    l_expl_gene = ndynlo(sddyna,'EXPL_GENE')
    l_reuse     = isfonc(list_func_acti,'REUSE')
    l_erre_thm  = isfonc(list_func_acti,'ERRE_TEMPS_THM')
    l_ener      = isfonc(list_func_acti,'ENERGIE')
!
! - Get previous displacement field
!
    call nmchex(hval_algo, 'SOLALG', 'DEPOLD', depold)
!
! - Does ETAT_INIT (initial state) exist ?
!
    l_state_init = isfonc(list_func_acti,'ETAT_INIT')
!
! - PILOTAGE LONGUEUR D'ARC AVEC ANGL_INCR_DEPL: IL FAUT LES DEUX
! - DERNIERS DEPLACEMENTS POUR QUE CA MARCHE (CHAMP DEPOLD)
!
    if (l_pilo) then
        call jeveuo(sdpilo(1:19)//'.PLTK', 'L', vk24=pltk)
        typpil = pltk(1)
        typsel = pltk(6)
        lpiarc = .false.
        if (typpil .eq. 'LONG_ARC' .or. typpil .eq. 'SAUT_LONG_ARC') then
            if (typsel(1:14) .eq. 'ANGL_INCR_DEPL') then
                lpiarc = .true.
            endif
        endif
    endif
!
! - Get name of result datastructure in ETAT_INIT
!
    l_stin_evol = ds_inout%l_stin_evol
    stin_evol   = ds_inout%stin_evol
!
! - ALARME SI CONTACT CONTINU AVEC UN CONCEPT REENTRANT
!
    if (l_cont_cont) then
        if (l_reuse) then
            if (.not.isfonc(list_func_acti,'CONTACT_INIT')) then
                call utmess('A', 'MECANONLINE4_14')
            endif
        else if (l_stin_evol) then
            if (.not.isfonc(list_func_acti,'CONTACT_INIT')) then
                call utmess('A', 'MECANONLINE4_15')
            endif
        endif
    endif
!
! - Initial storing index and time
!
    call nmdoin(ds_inout)
    init_time = ds_inout%init_time
    init_nume = ds_inout%init_nume
!
! - Print
!
    if (l_state_init) then
        call utmess('I', 'ETATINIT_10')
        if (l_ener) then
            call utmess('I', 'ETATINIT_5')
        endif
        if (l_stin_evol) then
            call utmess('I', 'ETATINIT_11', sk = stin_evol, sr = init_time, si = init_nume)
        else
            if (init_nume.eq.-1) then
                call utmess('I', 'ETATINIT_20')
            else
                call utmess('I', 'ETATINIT_12', sr = init_time)
            endif
        endif
    else
        if (l_reuse) then
            call utmess('A', 'ETATINIT_1')
        else
            call utmess('I', 'ETATINIT_20')
        endif
    endif
!
! - Loop on fields
!
    do i_field = 1, nb_field
!
! ----- Read field for ETAT_INIT - From results datastructure
!
        if (l_stin_evol) then
            call nmetl1(i_field, ds_inout)
        endif
!
! ----- Read field for ETAT_INIT - Field by field
!
        call nmetl2(i_field, ds_inout)
!
! ----- Read field for ETAT_INIT - Some checks
!
        call nmetl3(model, compor, i_field, ds_inout)
    end do
!
! - VERIFICATION COMPATIBILITE PILOTAGE
!
    if (l_stin_evol .and. lpiarc) then
        call rsexch(' ', stin_evol, 'DEPL', init_nume, champ1,&
                    iret)
        call rsexch(' ', stin_evol, 'DEPL', init_nume-1, champ2,&
                    iret)
        if (iret .ne. 0) then
            call utmess('F', 'MECANONLINE4_47', sk=stin_evol)
        endif
        call vtcopy(champ1, dep1, 'F', iret)
        call vtcopy(champ2, dep2, 'F', iret)
        call jeveuo(dep1(1:19)//'.VALE', 'L', vr=vdep1)
        call jeveuo(dep2(1:19)//'.VALE', 'L', vr=vdep2)
        call jeveuo(depold(1:19)//'.VALE', 'E', vr=depol)
        do i = 1, nb_equa
            depol(i) = vdep1(i) - vdep2(i)
        end do
        call jeveuo(sdpilo(1:19)//'.PLIR', 'E', vr=plir)
        call rsadpa(stin_evol, 'L', 1, 'COEF_MULT', init_nume,&
                    0, sjv=jinst, istop=0)
        coefav = zr(jinst)
        if (coefav .ne. 0.d0 .and. coefav .ne. r8vide()) then
            plir(6) = coefav
        endif
    endif
!
! - LECTURE DES INDICATEURS D'ERREUR EN TEMPS EN THM
!
    if (l_stin_evol .and. l_erre_thm) then
        errthm = sdcriq(1:19)//'.ERRT'
        call jeveuo(errthm, 'E', jerrt)
        call rsadpa(stin_evol, 'L', 1, 'ERRE_TPS_LOC', init_nume,&
                    0, sjv=jinst, istop=0)
        zr(jerrt-1+1) = zr(jinst)
        call rsadpa(stin_evol, 'L', 1, 'ERRE_TPS_GLOB', init_nume,&
                    0, sjv=jinst, istop=0)
        zr(jerrt-1+2) = zr(jinst)
!
    endif
!
! - CAS DE LA DYNAMIQUE: VITESSE ET ACCELERATION INITIALES
!
    do i_field = 1, nb_field
        field_type = ds_inout%field(i_field)%type
        l_zero     = ds_inout%field(i_field)%init_type.eq.'ZERO'
        l_acti     = ds_inout%l_field_acti(i_field)
        if (field_type .eq. 'VITE') then
            if (l_zero .and. l_acti) then
                call utmess('I', 'MECANONLINE4_22')
            endif
        endif
        if (field_type .eq. 'ACCE') then
            if (l_zero .and. l_acti) then
                call utmess('I', 'MECANONLINE4_23')
                l_acce_zero = .true.
            endif
        endif
    end do
!
! - PROJECTION MODALE EN EXPLICITE
!
    if (l_expl_gene) then
        call ndloam(sddyna, stin_evol, l_stin_evol, init_nume)
    endif
!
    call jedema()
end subroutine
