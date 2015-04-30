subroutine nmdini(keywf  , list_inst     , inst_init, l_inst_init, tole,&
                  nb_inst, l_init_noexist, nume_ini )
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utacli.h"
#include "asterfort/utmess.h"
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
    character(len=16), intent(in) :: keywf
    character(len=19), intent(in) :: list_inst
    real(kind=8), intent(in) :: tole
    real(kind=8), intent(in) :: inst_init
    aster_logical, intent(in) :: l_inst_init
    integer, intent(in) :: nb_inst
    aster_logical, intent(out) :: l_init_noexist
    integer, intent(out) :: nume_ini
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Time discretization datastructure
!
! Index of initial time
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword
! In  list_inst        : list of times from INCREMENT/LIST_INST
! In  tole             : tolerance to search time
! In  inst_init        : initial time if ETAT_INIT
! In  l_inst_init      : .true. if initial time in ETAT_INIT
! In  nb_inst          : number of time steps in list
! Out l_init_noexist   : .true. if initial time doesn't exist in list of times
! Out nume_ini         : index of initial time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n1, n2, i_inst
    real(kind=8) :: inst, ins, dt, dtmin
    real(kind=8), pointer :: v_list_inst(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nume_ini       = 0
    l_init_noexist = .false.
!
! - Acces to list of times
!
    call jeveuo(list_inst, 'L', vr = v_list_inst)
!
! - Get keywords
!
    call getvis(keywf, 'NUME_INST_INIT', iocc=1, scal=nume_ini, nbret=n1)
    call getvr8(keywf, 'INST_INIT'     , iocc=1, scal=inst    , nbret=n2)
!
! - No NUME_INST_INIT/INST_INIT
!
    if ((n1+n2 .eq. 0) .and. (.not.l_inst_init)) then
        nume_ini = 0
!
! - INCREMENT/INST_INIT or ETAT_INIT/INST_INIT
!
    else if (n1 .eq. 0) then
        if (n2 .eq. 0) then
!
! --------- ETAT_INIT/INST_INIT
!
            inst = inst_init
            call utacli(inst, v_list_inst, nb_inst, tole, nume_ini)
!
! --------- Not found: get nearest time before
!
            if (nume_ini .lt. 0) then
                l_init_noexist = .true.
                dtmin          = inst - v_list_inst(1)
                ins            = v_list_inst(1)
                do i_inst = 1, nb_inst-1
                    dt = inst - v_list_inst(1+i_inst)
                    if (dt .le. 0.d0) then
                        goto 45
                    endif
                    if (dt .lt. dtmin) then
                        dtmin = dt
                        ins = v_list_inst(1+i_inst)
                    endif
                end do
 45             continue
                inst = ins
            endif
        endif
        call utacli(inst, v_list_inst, nb_inst, tole, nume_ini)
        if (nume_ini .lt. 0) then
            call utmess('F', 'DISCRETISATION_89', sr=inst)
        endif
    endif
!
! - Checks
!
    ASSERT(nume_ini.ge.0)
    ASSERT(nume_ini.le.nb_inst)
!
end subroutine
