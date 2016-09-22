subroutine cme_getpara(option      ,&
                       model       , cara_elem, mate, compor_mult,&
                       v_list_load8, nb_load  ,&
                       rigi_meca   , mass_meca,&
                       time        , time_incr, nh       ,&
                       sigm        , strx     , disp)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/utmess.h"
#include "asterfort/get_load8.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=16), intent(out) :: option
    character(len=8), intent(out) :: model
    character(len=8), intent(out) :: cara_elem
    character(len=24), intent(out) :: mate
    character(len=24), intent(out) :: compor_mult
    character(len=8), intent(out), pointer :: v_list_load8(:)
    integer, intent(out) :: nb_load
    character(len=19), intent(out) :: rigi_meca
    character(len=19), intent(out) :: mass_meca
    real(kind=8), intent(out) :: time
    real(kind=8), intent(out) :: time_incr
    integer, intent(out) :: nh
    character(len=8), intent(out) :: sigm
    character(len=8), intent(out) :: strx
    character(len=8), intent(out) :: disp
!
! --------------------------------------------------------------------------------------------------
!
! CALC_MATR_ELEM
!
! Get parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc, ier
    character(len=8) :: chmate, answer
!
! --------------------------------------------------------------------------------------------------
!
    option       = ' '
    rigi_meca    = ' '
    mass_meca    = ' '
    time         = 0.d0
    time_incr    = 0.d0
    nh           = 0
    model        = ' '
    cara_elem    = ' '
    mate         = ' '
    chmate       = ' '
    compor_mult  = ' '
    v_list_load8 => null()
    nb_load      = 0
    sigm         = ' '
    strx         = ' '
    disp         = ' '
!
! - Get parameters
!
    call getvtx(' ', 'OPTION'   , scal=option, nbret=nocc)
    ASSERT(nocc.eq.1)
    call getvid(' ', 'RIGI_MECA', scal=rigi_meca, nbret=nocc)
    if (nocc .eq. 0) then
        rigi_meca = ' '
    endif
    call getvid(' ', 'MASS_MECA', scal=mass_meca, nbret=nocc)
    if (nocc .eq. 0) then
        mass_meca = ' '
    endif
    call getvr8(' ', 'INST', scal=time, nbret=nocc)
    if (nocc .eq. 0) then
        time = 0.d0
    endif
    call getvr8(' ', 'INCR_INST', scal=time_incr, nbret=nocc)
    if (nocc .eq. 0) then
        time_incr = 0.d0
    endif
    call getvis(' ', 'MODE_FOURIER', scal=nh, nbret=nocc)
    if (nocc .eq. 0) then
        nh = 0
    endif
    call getvid(' ', 'MODELE', scal=model, nbret=nocc)
    ASSERT(nocc.eq.1)
    call getvid(' ', 'CARA_ELEM', scal=cara_elem, nbret=nocc)
    call dismoi('EXI_RDM', model, 'MODELE', repk=answer)
    if ((nocc.eq.0) .and. (answer.eq.'OUI')) then
        call utmess('A', 'MECHANICS1_39')
        cara_elem = ' '
    endif
    call getvid(' ', 'CHAM_MATER', scal=chmate, nbret=nocc)
    call dismoi('BESOIN_MATER', model, 'MODELE', repk=answer)
    if ( (nocc.eq.0) .and. (answer .eq.'OUI')) then
        call utmess('A', 'MECHANICS1_40')
        chmate = ' '
    endif
    if (chmate .ne. ' ') then
        call rcmfmc(chmate, mate)
    else
        mate = ' '
    endif
    call get_load8(model, v_list_load8, nb_load)
! - For multifibers
    compor_mult = mate(1:8)//'.COMPOR'
!
    if (option.eq.'RIGI_GEOM') then
        call getvid(' ', 'SIEF_ELGA', scal=sigm, nbret=nocc)
        if (nocc .ne. 0) then
            call chpver('F', sigm, 'ELGA', 'SIEF_R', ier)
        endif
        call getvid(' ', 'STRX_ELGA', scal=strx, nbret=nocc)
        if (nocc .ne. 0) then
            call chpver('F', strx, 'ELGA', 'STRX_R', ier)
        endif
        call getvid(' ', 'DEPL', scal=disp, nbret=nocc)
        if (nocc .ne. 0) then
            call chpver('F', disp, 'NOEU', 'DEPL_R', ier)
        endif
    endif
!
end subroutine
