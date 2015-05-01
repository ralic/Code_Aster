subroutine nmdoin(evol_noli, l_etat_init, inst_init, nume_init)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/rs_getlast.h"
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
    aster_logical, intent(in) :: l_etat_init
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
! In  evol_noli        : name of result datastructure in initial state
! In  l_reuse          : .true. if result datastructure in initial state
! Out nume_init        : initial storing index
!                        -1 if not defined
! Out inst_init        : initial time
!                        r8vide if not defined
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ibid, nume_last, nume, tnum(1)
    integer :: n1, n2, n3
    integer :: jinst
    real(kind=8) :: prec, inst, inst_last
    complex(kind=8) :: c16bid
    character(len=8) :: k8bid, criter
    character(len=16) :: keyw_fact
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    inst_init = r8vide()
    nume_init = -1
    keyw_fact = 'ETAT_INIT'
!
! - Initial time search
!
    if (l_etat_init) then
!
! ----- Read storing index and time by user
!
        call getvr8(keyw_fact, 'INST'      , iocc=1, scal=inst, nbret=n1)
        call getvis(keyw_fact, 'NUME_ORDRE', iocc=1, scal=nume, nbret=n2)
!
! ----- No storing index/time by user => last one in results datastructure
!
        if (n1+n2 .eq. 0) then
            call rs_getlast(evol_noli, nume_last, inst_last)
            nume_init = nume_last
            inst_init = inst_last
        endif
!
! ----- Time by user
!
        if (n1 .ne. 0) then
            inst_init = inst
            call getvr8(keyw_fact, 'PRECISION', iocc=1, scal=prec, nbret=ibid)
            call getvtx(keyw_fact, 'CRITERE', iocc=1, scal=criter, nbret=ibid)
            call rsorac(evol_noli, 'INST', ibid, inst_init, k8bid,&
                        c16bid, prec, criter, tnum, 1,&
                        n3)
            nume_init=tnum(1)            
            if (n3 .eq. 0) then
                call utmess('F', 'ETATINIT_3', sk=evol_noli)
            endif
            if (n3 .lt. 0) then
                call utmess('F', 'ETATINIT_4', sk=evol_noli)
            endif
        endif
!
! ----- Storing index by user
!
        if (n2 .ne. 0) then
            nume_init = nume
            call rsadpa(evol_noli, 'L', 1, 'INST', nume_init,&
                        0, sjv=jinst, styp=k8bid)
            inst_init = zr(jinst)
        endif
    endif
!
! - Initial time defined by user
!
    call getvr8(keyw_fact, 'INST_ETAT_INIT', iocc=1, scal=inst, nbret=n2)
    if (n2 .ne. 0) then
        inst_init = inst
    endif
!
    call jedema()
!
end subroutine
