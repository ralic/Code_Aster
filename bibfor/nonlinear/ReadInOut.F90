subroutine ReadInOut(phenom, result, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/gcucon.h"
#include "asterc/getfac.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
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
    character(len=4), intent(in) :: phenom
    character(len=8), intent(in) :: result
    type(NL_DS_InOut), intent(inout) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE and THER_* - Input/output management
!
! Read parameters for input/output management
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom           : phenomenon (MECA/THER/ACOU)
! In  result           : name of result datastructure (EVOL_NOLI)
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: keywf, init_keyw, result_type
    character(len=8) :: stin_evol, field, criterion, answer
    real(kind=8) :: precision, user_time, stin_time, temp_init
    integer :: nocc, didi_nume, i_field, user_nume, iret
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<           > . Read parameters input/output management'
    endif
!
! - Initializations
!
    keywf = 'ETAT_INIT'
    if (phenom.eq.'MECA') then
        result_type = 'EVOL_NOLI'
    elseif (phenom.eq.'THER') then
        result_type = 'EVOL_THER'
    else
        ASSERT(.false.)
    endif
!
! - Set name of result datastructure
!
    ds_inout%result = result
    if (getexm(keywf,result_type) .ne. 1) then
        goto 99
    endif
!
! - Does ETAT_INIT (initial state) exist ?
!
    call getfac(keywf, nocc)
    ASSERT(nocc.le.1)
    ds_inout%l_state_init = nocc.gt.0
!
! - Is REUSE ?
!
    call gcucon(result, result_type, iret)
    ds_inout%l_reuse = iret.gt.0
!
! - Get name of result datastructure in ETAT_INIT
!
    call getvid(keywf, result_type, iocc=1, scal=stin_evol, nbret=nocc)
    if (nocc.ge.1) then
        ds_inout%stin_evol   = stin_evol
        ds_inout%l_stin_evol = .true.
    endif
!
! - For DIDI loads
!
    if (phenom.eq.'MECA') then
        call getvis(keywf, 'NUME_DIDI', iocc=1, scal=didi_nume, nbret=nocc)
        if (nocc.ge.1) then
            ds_inout%didi_nume = didi_nume
        endif
    endif
!
! - For thermics
!
    if (phenom.eq.'THER') then
        call getvtx(keywf, 'STATIONNAIRE', iocc=1, scal=answer   , nbret=nocc)
        if (nocc.eq.1) then
            ds_inout%l_init_stat = .true._1
        endif
        call getvr8(keywf, 'VALE'        , iocc=1, scal=temp_init, nbret=nocc)
        if (nocc.eq.1) then
            ds_inout%l_init_vale = .true._1
            ds_inout%temp_init   = temp_init
        endif
    endif
!
! - Read fields
!
    do i_field = 1, ds_inout%nb_field
        init_keyw = ds_inout%field(i_field)%init_keyw
        print *, "ReadInOut:", i_field, '>', init_keyw, '<'
        if (getexm(keywf,init_keyw) .eq. 1 .and. ds_inout%field(i_field)%l_read_init) then
            call getvid(keywf, init_keyw, scal = field, iocc=1, nbret=nocc)
            if (nocc.eq.1) then
                ds_inout%l_field_read(i_field)     = .true._1
                ds_inout%field(i_field)%field_read = field
            endif
        endif
    end do
!
! - Get parameters to read initial state
!
    call getvr8(keywf, 'PRECISION'     , iocc=1, scal=precision, nbret = nocc)
    if (nocc.eq.0) then
        ds_inout%precision = 1.d-6
    else
        ds_inout%precision = precision
    endif
    call getvtx(keywf, 'CRITERE'       , iocc=1, scal=criterion, nbret = nocc)
    if (nocc.eq.0) then
        ds_inout%criterion = 'RELATIF'
    else
        ds_inout%criterion = criterion
    endif
    call getvr8(keywf, 'INST'          , iocc=1, scal=user_time, nbret = nocc)
    ds_inout%user_time   = user_time
    ds_inout%l_user_time = nocc.gt.0
    call getvis(keywf, 'NUME_ORDRE'    , iocc=1, scal=user_nume, nbret = nocc)
    ds_inout%user_nume   = user_nume
    ds_inout%l_user_nume = nocc.gt.0
    call getvr8(keywf, 'INST_ETAT_INIT', iocc=1, scal=stin_time, nbret = nocc)
    ds_inout%stin_time   = stin_time
    ds_inout%l_stin_time = nocc.gt.0
!
 99 continue
!
end subroutine
