subroutine nmdoet(model    , compor, list_func_acti, nume_ddl , sdpilo     ,&
                  sddyna   , sdcriq, sdinout       , hval_algo, l_acce_zero,&
                  inst_init)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
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
    character(len=24), intent(in) :: sdinout
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdpilo
    character(len=19), intent(in) :: hval_algo(*)
    integer, intent(in) :: list_func_acti(*)
    aster_logical, intent(out) :: l_acce_zero
    real(kind=8), intent(out) :: inst_init
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
! In  sdinout          : datastructure for input/output parameters
! Out l_acce_zero      : .true. if initial acceleration must been computed
! Out inst_init        : time for initial state (R8VIDE if not defined)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdinout_lcha, sdinout_info
    character(len=24), pointer :: v_sdinout_lcha(:) => null()
    integer, pointer :: v_sdinout_info(:) => null()
    integer :: zioch, nb_field
    character(len=24) :: field_name_resu
    aster_logical :: l_init_evol, l_init_state
    integer :: nb_equa, nocc, nume_init, iret, i, i_field
    character(len=8) :: calcri, result
    character(len=16) :: keywf
    character(len=24) :: evol
    character(len=24) :: typpil, typsel
    character(len=19) :: depold
    character(len=24) :: champ1, champ2, dep2, dep1, errthm
    integer :: jinst, jerrt
    aster_logical :: l_pilo, lpiarc, l_cont_cont
    aster_logical :: l_expl_gene, l_reuse, l_erre_thm
    aster_logical :: lzero, l_ener
    real(kind=8) :: coefav
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
    keywf        = 'ETAT_INIT'
    call dismoi('NB_EQUA', nume_ddl, 'NUME_DDL', repi=nb_equa)
!
! - Model can compute rigidity ?
!
    call dismoi('CALC_RIGI', model, 'MODELE', repk=calcri)
    if (calcri .ne. 'OUI') then
        call utmess('F', 'CALCULEL2_65', sk=model)
    endif
!
! - Access to input/output datastructure
!
    sdinout_lcha = sdinout(1:19)//'.LCHA'
    sdinout_info = sdinout(1:19)//'.INFO'
    call jeveuo(sdinout_lcha, 'L', vk24 = v_sdinout_lcha)
    call jeveuo(sdinout_info, 'L', vi   = v_sdinout_info)
    nb_field = v_sdinout_info(1)
    zioch    = v_sdinout_info(4)
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
    l_init_state = isfonc(list_func_acti,'ETAT_INIT')
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
    call getvid(keywf, 'EVOL_NOLI', iocc=1, scal=evol, nbret=nocc)
    ASSERT(nocc.le.1)
    l_init_evol = nocc .gt. 0
!
! - ALARME SI CONTACT CONTINU AVEC UN CONCEPT REENTRANT
!
    if (l_cont_cont) then
        if (l_reuse) then
            if (.not.isfonc(list_func_acti,'CONTACT_INIT')) then
                call utmess('A', 'MECANONLINE4_14')
            endif
        else if (l_init_evol) then
            if (.not.isfonc(list_func_acti,'CONTACT_INIT')) then
                call utmess('A', 'MECANONLINE4_15')
            endif
        endif
    endif
!
! - Initial storing index and time
!
    call nmdoin(evol, l_init_evol, inst_init, nume_init)
!
! - Print
!
    if (l_init_state) then
        call utmess('I', 'ETATINIT_10')
        if (l_ener) then
            call utmess('I', 'ETATINIT_5')
        endif
        if (l_init_evol) then
            call utmess('I', 'ETATINIT_11', sk = evol, sr = inst_init, si = nume_init)
        else
            if (nume_init.eq.-1) then
                call utmess('I', 'ETATINIT_20')
            else
                call utmess('I', 'ETATINIT_12', sr = inst_init)
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
! ----- Initial state defined by EVOL_NOLI results 
!
        result = evol(1:8)
!
! ----- Read field for ETAT_INIT - From results datastructure
!
        if (l_init_evol) then
            call nmetl1(result, nume_init, sdinout, i_field)
        endif
!
! ----- Read field for ETAT_INIT - Field by field
!
        call nmetl2(keywf, sdinout, i_field)
!
! ----- Read field for ETAT_INIT - Some checks
!
        call nmetl3(model  , compor      , l_init_evol, result, nume_init,&
                    sdinout, l_init_state, i_field)
    end do
!
! - VERIFICATION COMPATIBILITE PILOTAGE
!
    if (l_init_evol .and. lpiarc) then
        call rsexch(' ', result, 'DEPL', nume_init, champ1,&
                    iret)
        call rsexch(' ', result, 'DEPL', nume_init-1, champ2,&
                    iret)
        if (iret .ne. 0) then
            call utmess('F', 'MECANONLINE4_47', sk=evol)
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
        call rsadpa(result, 'L', 1, 'COEF_MULT', nume_init,&
                    0, sjv=jinst, istop=0)
        coefav = zr(jinst)
        if (coefav .ne. 0.d0 .and. coefav .ne. r8vide()) then
            plir(6) = coefav
        endif
    endif
!
! - LECTURE DES INDICATEURS D'ERREUR EN TEMPS EN THM
!
    if (l_init_evol .and. l_erre_thm) then
        errthm = sdcriq(1:19)//'.ERRT'
        call jeveuo(errthm, 'E', jerrt)
        call rsadpa(result, 'L', 1, 'ERRE_TPS_LOC', nume_init,&
                    0, sjv=jinst, istop=0)
        zr(jerrt-1+1) = zr(jinst)
        call rsadpa(result, 'L', 1, 'ERRE_TPS_GLOB', nume_init,&
                    0, sjv=jinst, istop=0)
        zr(jerrt-1+2) = zr(jinst)
!
    endif
!
! - CAS DE LA DYNAMIQUE: VITESSE ET ACCELERATION INITIALES
!
    do i_field = 1, nb_field
        field_name_resu = v_sdinout_lcha(zioch*(i_field-1)+1 )
        lzero = v_sdinout_lcha(zioch*(i_field-1)+4 ).eq.'ZERO'
        if (field_name_resu .eq. 'VITE') then
            if (lzero) then
                call utmess('I', 'MECANONLINE4_22')
            endif
        endif
        if (field_name_resu .eq. 'ACCE') then
            if (lzero) then
                call utmess('I', 'MECANONLINE4_23')
                l_acce_zero = .true.
            endif
        endif
    end do
!
! - PROJECTION MODALE EN EXPLICITE
!
    if (l_expl_gene) then
        result = evol(1:8)
        call ndloam(sddyna, result, l_init_evol, nume_init)
    endif
!
    call jedema()
end subroutine
