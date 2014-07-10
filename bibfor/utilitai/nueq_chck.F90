subroutine nueq_chck(prof_chnoz, nb_equaz, l_error, l_subs)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
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
    character(len=*), intent(in) :: prof_chnoz
    integer, optional, intent(out) :: nb_equaz
    logical, optional, intent(in) :: l_error
    logical, optional, intent(in) :: l_subs
!
! --------------------------------------------------------------------------------------------------
!
! Check prof_chno
!
! --------------------------------------------------------------------------------------------------
!
! In  prof_chno   : name of PROF_CHNO
! Out nb_equa     : number of equations
! In  l_error     : emits explicit message if present
! In  l_subs      : exclude non-unit numbering (excluding substructing) if present
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: prof_chno
    character(len=24) :: nueq, deeq
    integer :: len_v, nb_equa, i_equa
    integer, pointer :: p_nueq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prof_chno = prof_chnoz
    nueq = prof_chno//'.NUEQ'
    deeq = prof_chno//'.DEEQ'
    call jelira(deeq, 'LONMAX', len_v)
    call jelira(nueq, 'LONMAX', nb_equa)
    if (len_v.ne.2*nb_equa) then
        if (present(l_error)) then
            call utmess('F','CHAMPS_20')
        else
            ASSERT(.false.)       
        endif
    endif
    if (present(nb_equaz)) then
        nb_equaz = nb_equa
    endif
    if (present(l_subs)) then
        call jeveuo(nueq,'L',vi = p_nueq)
        do i_equa = 1, nb_equa
            ASSERT(p_nueq(i_equa).eq.i_equa)
        end do
    endif
end subroutine
