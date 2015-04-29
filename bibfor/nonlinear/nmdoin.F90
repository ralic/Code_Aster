subroutine nmdoin(evol_noli, l_init_evol, inst_init, nume_init)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
#include "asterfort/rs_gettime.h"
#include "asterfort/rs_getlast.h"
#include "asterfort/rs_getnume.h"
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
    character(len=24), intent(in) :: evol_noli
    aster_logical, intent(in) :: l_init_evol
    integer, intent(out) :: nume_init
    real(kind=8), intent(out) :: inst_init
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Time management
!
! Initial storing index and time
!
! --------------------------------------------------------------------------------------------------
!
! In  evol_noli        : name of result datastructure in ETAT_INIT
! In  l_init_evol      : .true. if result datastructure in ETAT_INIT
! Out nume_init        : initial storing index
!                        -1 if not defined
! Out inst_init        : initial time
!                        r8vide if not defined
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nume_last, nume_user
    integer :: n1, n2, iret
    real(kind=8) :: prec, inst_user, inst_last, inst_etat_init
    character(len=8) :: criter
    character(len=16) :: keyw_fact
!
! --------------------------------------------------------------------------------------------------
!
    inst_init = r8vide()
    nume_init = -1
    keyw_fact = 'ETAT_INIT'
!
! - Initial time search
!
    if (l_init_evol) then
!
! ----- Read storing index and time by user
!
        call getvr8(keyw_fact, 'INST'      , iocc=1, scal=inst_user, nbret=n1)
        call getvis(keyw_fact, 'NUME_ORDRE', iocc=1, scal=nume_user, nbret=n2)
!
! ----- No storing index/time by user => last one in results datastructure
!
        if (n1+n2 .eq. 0) then
            call rs_getlast(evol_noli, nume_last, inst_last)
            nume_init = nume_last
            inst_init = inst_last
        endif
!
! ----- Time by user => get storing index
!
        if (n1 .ne. 0) then
            inst_init = inst_user
            call getvr8(keyw_fact, 'PRECISION', iocc=1, scal=prec)
            call getvtx(keyw_fact, 'CRITERE', iocc=1, scal=criter)
            call rs_getnume(evol_noli, inst_init, criter, prec, nume_init, iret)           
            if (iret .eq. 0) then
                call utmess('F', 'ETATINIT_3', sk=evol_noli)
            endif
            if (iret .eq. 2) then
                call utmess('F', 'ETATINIT_4', sk=evol_noli)
            endif
            ASSERT(iret.eq.1)
        endif
!
! ----- Storing index by user => get time
!
        if (n2 .ne. 0) then
            nume_init = nume_user
            call rs_gettime(evol_noli, nume_init, inst_init)
        endif
    endif
!
! - Initial time defined by user
!
    call getvr8(keyw_fact, 'INST_ETAT_INIT', iocc=1, scal=inst_etat_init, nbret=n2)
    if (n2 .ne. 0) then
        inst_init = inst_etat_init
    endif
!
end subroutine
