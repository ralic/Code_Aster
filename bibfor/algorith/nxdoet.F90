subroutine nxdoet(model    , nume_ddl, l_reuse, l_stat, sd_inout,&
                  type_init, inst_0)
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
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdoin.h"
#include "asterfort/nmetl1.h"
#include "asterfort/nmetl2.h"
#include "asterfort/nmetnc.h"
#include "asterfort/ntetl3.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=24), intent(in) :: model
    aster_logical, intent(in) :: l_reuse
    character(len=24), intent(in) :: nume_ddl
    character(len=24), intent(in) :: sd_inout
    real(kind=8), intent(out) :: inst_0
    integer, intent(out) :: type_init
    aster_logical, intent(out) :: l_stat
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Init
!
! Read initial state
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  nume_ddl         : name of nume_ddl object (numbering equation)
! In  sd_inout         : datastructure for input/output parameters
! In  l_reuse          : .true. if reuse results datastructure
! Out inst_0           : time for initial state (R8VIDE if not defined)
! Out type_init        : type of initialization
!              -1 : PAS D'INITIALISATION. (VRAI STATIONNAIRE)
!               0 : CALCUL STATIONNAIRE
!               1 : VALEUR UNIFORME
!               2 : CHAMP AUX NOEUDS
!               3 : RESULTAT D'UN AUTRE CALCUL
! Out l_stat           : .true. if stationnary
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, nb_field
    character(len=8) :: calcri, result
    character(len=24) :: answer, evol
    character(len=24) :: field_name_resu, field_name_algo, field_algo
    integer :: i_field
    character(len=16) :: keyword_fact
    integer :: i, neq, nocc, nume_store_0
    real(kind=8) :: tempct
    integer :: ifm, niv
    aster_logical :: l_init_evol, l_init_state
    real(kind=8), pointer :: vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! - Initializations
!
    type_init = -2
    inst_0   = r8vide()
    l_init_evol = .false.
    l_stat = .false.
    l_init_state = .false.
    keyword_fact = 'ETAT_INIT'
!
    call dismoi('NB_EQUA', nume_ddl, 'NUME_DDL', repi=neq)
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
! - ON VERIFIE QUE LE MODELE SAIT CALCULER UNE RIGIDITE
!
    call dismoi('CALC_RIGI', model, 'MODELE', repk=calcri)
    if (calcri .ne. 'OUI') then
        call utmess('F', 'CALCULEL2_65', sk=model)
    endif
!
! - PAS D'ETAT INITIAL EN PRESENCE D'UN CONCEPT REENTRANT
!
    call getfac(keyword_fact, nocc)
    ASSERT(nocc.le.1)
    l_init_state = nocc.gt.0
    if (l_init_state) then
        if (niv .ge. 2) then
            write (ifm,*) '<THERNONLINE> LECTURE ETAT INITIAL'
        endif
    else
        if (l_reuse) then
            call utmess('A', 'ETATINIT_1')
        else
            call utmess('I', 'ETATINIT_20')
        endif
    endif
!
! - CONCEPT EVOL_THER DONNE DANS ETAT_INIT
!
    call getvid(keyword_fact, 'EVOL_THER', iocc=1, scal=evol, nbret=nocc)
    ASSERT(nocc.le.1)
    l_init_evol = nocc.gt.0
!
! - INSTANT INITIAL
!
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> ... INSTANT INITIAL'
    endif
    call nmdoin(evol, l_init_evol, inst_0, nume_store_0)
    if (niv .ge. 2) then
        if (inst_0 .eq. r8vide()) then
            write (ifm,*) '<THERNONLINE> ...... NON DEFINI PAR ETAT_INIT'
        else
            write (ifm,*) '<THERNONLINE> ...... VALEUR    : ',inst_0
            write (ifm,*) '<THERNONLINE> ...... NUME_ORDRE: ',nume_store_0
        endif
    endif
!
! - PAS DE PRECISION --> C'EST UN CALCUL STATIONNAIRE
!
    call getfac(keyword_fact, nocc)
    ASSERT(nocc.le.1)
    if (nocc .eq. 0) then
        l_stat    = .true.
        type_init = -1
        goto 99
    endif
!
! - Loop on fields
!
    do i_field = 1, nb_field
!
! ----- Initial state defined by EVOL_THER results 
!
        result = evol(1:8)
!
! ----- Read field for ETAT_INIT
!
        if (l_init_evol) then
!
! --------- Read field for ETAT_INIT - From results datastructure
!
            type_init = 3
            call nmetl1(result, nume_store_0, sd_inout, i_field)
        else
!
! --------- Name of field (type) in results datastructure
!
            field_name_resu = v_io_para(zioch*(i_field-1)+1 )
!
! --------- Name of field in algorithm
!
            field_name_algo = v_io_para(zioch*(i_field-1)+6 )
            call nmetnc(field_name_algo, field_algo)
!
            if (field_name_resu .eq. 'TEMP') then
                call jeveuo(field_algo(1:19)//'.VALE', 'E', vr=vale)
!
! ------------- Initial temperature: from field
!
                call getvid(keyword_fact, 'CHAM_NO', iocc=1, nbret=nocc)
                if (nocc .eq. 1) then
                    type_init = 2
                    call nmetl2(keyword_fact, sd_inout, i_field)
                endif
!
! ------------- Initial temperature: stationnary computation
!
                call getvtx(keyword_fact, 'STATIONNAIRE', iocc=1, scal=answer, nbret=nocc)
                if (nocc .gt. 0) then
                    if (answer(1:3) .eq. 'OUI') then
                        l_stat = .true.
                        type_init = 0
                        v_io_para(zioch*(i_field-1)+4) = 'STATIONNAIRE'
                    endif
                endif
!
! ------------- Initial temperature: constant
!
                call getvr8(keyword_fact, 'VALE', iocc=1, scal=tempct, nbret=nocc)
                if (nocc .gt. 0) then
                    type_init = 1
                    do i = 1, neq
                        vale(i) = tempct
                        v_io_para(zioch*(i_field-1)+4) = 'VALE'
                    end do
                endif
!
            else
                call nmetl2(keyword_fact, sd_inout, i_field)
            endif
        endif
!
! ----- Read field for ETAT_INIT - Some checks
!
        call ntetl3(result, sd_inout, i_field, tempct)
    end do
!
 99 continue
!
    call jedema()
!
end subroutine
