subroutine nmdoet(model , compor, list_func_acti, nume_ddl, sdpilo     ,&
                  sddyna, sdcriq, sd_inout      , solalg  , l_acce_zero,&
                  inst_0)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/infdbg.h"
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
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_inout
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdpilo
    character(len=19), intent(in) :: solalg(*)
    integer, intent(in) :: list_func_acti(*)
    aster_logical, intent(out) :: l_acce_zero
    real(kind=8), intent(out) :: inst_0
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
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDPILO : SD DE PILOTAGE
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDCRIQ : SD CRITERE QUALITE
! In  sd_inout         : datastructure for input/output parameters
! Out l_acce_zero      : .true. if initial acceleration must been computed
! Out inst_0           : time for initial state (R8VIDE if not defined)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, nb_field
    character(len=24) :: field_name_resu
    aster_logical :: l_init_evol, l_init_state
    integer :: neq, nocc, nume_store_0, iret, i, i_field
    character(len=8) :: calcri, result
    character(len=16) :: keyword_fact
    character(len=24) :: evol
    character(len=24) :: typpil, typsel
    character(len=19) :: depold
    character(len=24) :: champ1, champ2, dep2, dep1, errthm
    integer :: jinst, jerrt
    aster_logical :: lpilo, lpiarc, lctcc
    aster_logical :: lexge, lreuse, lerrt
    aster_logical :: lzero, lener
    real(kind=8) :: coefav
    integer :: ifm, niv
    real(kind=8), pointer :: plir(:) => null()
    character(len=24), pointer :: pltk(:) => null()
    real(kind=8), pointer :: vdep1(:) => null()
    real(kind=8), pointer :: vdep2(:) => null()
    real(kind=8), pointer :: depol(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! - Initializations
!
    dep1 = '&&CNPART.CHP1'
    dep2 = '&&CNPART.CHP2'
    l_acce_zero  = .false.
    lpiarc       = .false.
    keyword_fact = 'ETAT_INIT'
    call dismoi('NB_EQUA', nume_ddl, 'NUME_DDL', repi=neq)
!
! - ON VERIFIE QUE LE MODELE SAIT CALCULER UNE RIGIDITE
!
    call dismoi('CALC_RIGI', model, 'MODELE', repk=calcri)
    if (calcri .ne. 'OUI') then
        call utmess('F', 'CALCULEL2_65', sk=model)
    endif
!
! - Access to input/output datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    io_info = sd_inout(1:19)//'.INFO'
    call jeveuo(io_lcha, 'L', vk24 = v_io_para)
    call jeveuo(io_info, 'L', vi   = v_io_info)
    nb_field = v_io_info(1)
    zioch    = v_io_info(4)
!
! - FONCTIONNALITES ACTIVEES
!
    lpilo = isfonc(list_func_acti,'PILOTAGE')
    lctcc = isfonc(list_func_acti,'CONT_CONTINU')
    lexge = ndynlo(sddyna,'EXPL_GENE')
    lreuse = isfonc(list_func_acti,'REUSE')
    lerrt = isfonc(list_func_acti,'ERRE_TEMPS_THM')
    lener = isfonc(list_func_acti,'ENERGIE')
!
! - EXTRACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
!
! - PILOTAGE LONGUEUR D'ARC AVEC ANGL_INCR_DEPL: IL FAUT LES DEUX
! - DERNIERS DEPLACEMENTS POUR QUE CA MARCHE (CHAMP DEPOLD)
!
    if (lpilo) then
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
! - PAS D'ETAT INITIAL EN PRESENCE D'UN CONCEPT REENTRANT
!
    call getfac(keyword_fact, nocc)
    ASSERT(nocc.le.1)
    l_init_state = nocc.gt.0
    if (l_init_state) then
        call utmess('I', 'ETATINIT_10')
        if (lener) then
            call utmess('I', 'ETATINIT_5')
        endif
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> LECTURE ETAT INITIAL'
        endif
    else
        if (lreuse) then
            call utmess('A', 'ETATINIT_1')
        else
            call utmess('I', 'ETATINIT_20')
        endif
    endif
!
! - CONCEPT EVOL_NOLI DONNE DANS ETAT_INIT
!
    call getvid(keyword_fact, 'EVOL_NOLI', iocc=1, scal=evol, nbret=nocc)
    ASSERT(nocc.le.1)
    l_init_evol = nocc .gt. 0
!
! - ALARME SI CONTACT CONTINU AVEC UN CONCEPT REENTRANT
!
    if (lctcc) then
        if (lreuse) then
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
! - INSTANT INITIAL
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... INSTANT INITIAL'
    endif
    call nmdoin(evol, l_init_evol, inst_0, nume_store_0)
    if (niv .ge. 2) then
        if (inst_0 .eq. r8vide()) then
            write (ifm,*) '<MECANONLINE> ...... NON DEFINI PAR ETAT_INIT'
        else
            write (ifm,*) '<MECANONLINE> ...... VALEUR    : ',inst_0
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
            call nmetl1(result, nume_store_0, sd_inout, i_field)
        endif
!
! ----- Read field for ETAT_INIT - Field by field
!
        call nmetl2(keyword_fact, sd_inout, i_field)
!
! ----- Read field for ETAT_INIT - Some checks
!
        call nmetl3(model   , compor, l_init_evol, result, nume_store_0,&
                    sd_inout, l_init_state, i_field)
!
    end do
!
! - VERIFICATION COMPATIBILITE PILOTAGE
!
    if (l_init_evol .and. lpiarc) then
        call rsexch(' ', result, 'DEPL', nume_store_0, champ1,&
                    iret)
        call rsexch(' ', result, 'DEPL', nume_store_0-1, champ2,&
                    iret)
        if (iret .ne. 0) then
            call utmess('F', 'MECANONLINE4_47', sk=evol)
        endif
        call vtcopy(champ1, dep1, 'F', iret)
        call vtcopy(champ2, dep2, 'F', iret)
        call jeveuo(dep1(1:19)//'.VALE', 'L', vr=vdep1)
        call jeveuo(dep2(1:19)//'.VALE', 'L', vr=vdep2)
        call jeveuo(depold(1:19)//'.VALE', 'E', vr=depol)
        do i = 1, neq
            depol(i) = vdep1(i) - vdep2(i)
        end do
        call jeveuo(sdpilo(1:19)//'.PLIR', 'E', vr=plir)
        call rsadpa(result, 'L', 1, 'COEF_MULT', nume_store_0,&
                    0, sjv=jinst)
        coefav = zr(jinst)
        if (coefav .ne. 0.d0 .and. coefav .ne. r8vide()) then
            plir(6) = coefav
        endif
    endif
!
! - LECTURE DES INDICATEURS D'ERREUR EN TEMPS EN THM
!
    if (l_init_evol .and. lerrt) then
        errthm = sdcriq(1:19)//'.ERRT'
        call jeveuo(errthm, 'E', jerrt)
        call rsadpa(result, 'L', 1, 'ERRE_TPS_LOC', nume_store_0,&
                    0, sjv=jinst)
        zr(jerrt-1+1) = zr(jinst)
        call rsadpa(result, 'L', 1, 'ERRE_TPS_GLOB', nume_store_0,&
                    0, sjv=jinst)
        zr(jerrt-1+2) = zr(jinst)
!
    endif
!
! - CAS DE LA DYNAMIQUE: VITESSE ET ACCELERATION INITIALES
!
    do i_field = 1, nb_field
        field_name_resu = v_io_para(zioch*(i_field-1)+1 )
        lzero = v_io_para(zioch*(i_field-1)+4 ).eq.'ZERO'
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
    if (lexge) then
        result = evol(1:8)
        call ndloam(sddyna, result, l_init_evol, nume_store_0)
    endif
!
    call jedema()
end subroutine
