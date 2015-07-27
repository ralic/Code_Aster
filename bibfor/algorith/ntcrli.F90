subroutine ntcrli(inst_init, list_inst, sddisc)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/r8vide.h"
#include "asterfort/deprecated_command.h"
#include "asterfort/diinst.h"
#include "asterfort/getvr8.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrlm.h"
#include "asterfort/nmcrls.h"
#include "asterfort/nmcrpc.h"
#include "asterfort/nmdifi.h"
#include "asterfort/nmdini.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: list_inst
    real(kind=8), intent(in) :: inst_init
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Datastructures
!
! Time discretization datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! In  inst_init        : initial time if ETAT_INIT
! In  list_inst        : list of times from INCREMENT/LIST_INST
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nume_ini, nume_end, nume_inst
    integer :: nb_inst_new, nb_inst
    real(kind=8) :: tole
    real(kind=8) :: dtmin, dt0
    aster_logical :: l_init_noexist, l_inst_init
    character(len=24) :: list_inst_info
    character(len=24) :: list_inst_ditr
    character(len=16) :: list_inst_type, keywf
    character(len=24) :: sddisc_bcle
    integer, pointer :: v_sddisc_bcle(:) => null()
    character(len=19) :: list_inst_work
    real(kind=8), pointer :: v_list_work(:) => null()
    character(len=24) :: sddisc_dini
    integer, pointer :: v_sddisc_dini(:) => null()
    character(len=24) :: sddisc_lipo
    character(len=24) :: sddisc_ditr
    character(len=24) :: sddisc_linf
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> ... CREATION SD DISCRETISATION'
    endif
!
! - Initializations
!
    l_init_noexist = .false.
    keywf          = 'INCREMENT'
    list_inst_work = '&&NTCRLI.PROVLI'
!
! - Create loops object
! --- 1 - Newton (ITERAT)
! --- 2 - Time stepping (NUME_INST)
! --- 3 - Fixed loops (NIVEAU)
!
    sddisc_bcle = sddisc(1:19)//'.BCLE'
    call wkvect(sddisc_bcle, 'V V I', 3, vi = v_sddisc_bcle)
!
! - Type of list_inst
!
    call gettco(list_inst, list_inst_type)
!
! - Create list of times and information vector
!
    if (list_inst_type .eq. 'LISTR8_SDASTER') then
        call nmcrlm(list_inst, sddisc, list_inst_work)
        call deprecated_command('LIST_INST')
    else if (list_inst_type.eq.'LIST_INST') then
        sddisc_linf    = sddisc(1:19)//'.LINF'
        list_inst_info = list_inst(1:8)//'.LIST.INFOR'
        list_inst_ditr = list_inst(1:8)//'.LIST.DITR'
        call jedup1(list_inst_ditr, 'V', list_inst_work)
        call jedup1(list_inst_info, 'V', sddisc_linf)
    endif
!
! - Get parameters
!
    call utdidt('L', sddisc, 'LIST', 'DTMIN',&
                valr_ = dtmin)
    call utdidt('L', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst)
!
! - Acces to list of times
!
    call jeveuo(list_inst_work, 'L', vr = v_list_work)
!
! - Get parameters
!
    call getvr8(keywf, 'PRECISION', iocc=1, scal=tole)
    tole = abs(dtmin) * tole
!
! - Have an initial time in ETAT_INIT ?
!
    if (inst_init .eq. r8vide()) then
        l_inst_init = .false.
    else
        l_inst_init = .true.
    endif
!
! - Index of initial time
!
    call nmdini(keywf  , list_inst_work, inst_init, l_inst_init, tole,&
                nb_inst, l_init_noexist, nume_ini)
!
! - Index of final time
!
    call nmdifi(keywf, list_inst_work, tole, nb_inst, nume_end)
!
! - Check
!
    if (nume_ini .ge. nume_end) then
        call utmess('F', 'DISCRETISATION_92')
    endif
!
! - Resize list of times
!
    call nmcrls(sddisc   , list_inst_work, nume_ini, nume_end, l_init_noexist,&
                inst_init, nb_inst_new   , dtmin)
!
! - Create object for subdividing time steps
!
    sddisc_dini = sddisc(1:19)//'.DINI'
    call wkvect(sddisc_dini, 'V V I', nb_inst_new, vi = v_sddisc_dini)
    do nume_inst = 1, nb_inst_new
        v_sddisc_dini(nume_inst) = 1
    end do
!
! - Save parameters
!
    dt0 = diinst(sddisc,1) - diinst(sddisc,0)
    call utdidt('E', sddisc, 'LIST', 'DT-',&
                valr_ = dt0)
    call utdidt('E', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst_new)
    call utdidt('E', sddisc, 'LIST', 'DTMIN',&
                valr_ = dtmin)
!
! - Save object of time steps
!
    sddisc_ditr = sddisc(1:19)//'.DITR'
    sddisc_lipo = sddisc(1:19)//'.LIPO'
    call jedupo(sddisc_ditr, 'V', sddisc_lipo, .false._1)
!
! - Clean
!
    call jedetr(list_inst_work)
!
end subroutine
