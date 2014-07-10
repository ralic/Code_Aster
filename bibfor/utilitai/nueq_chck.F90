subroutine nueq_chck(prof_chnoz, nb_equaz)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! Check prof_chno
!
! --------------------------------------------------------------------------------------------------
!
! In  prof_chno   : name of PROF_CHNO
! Out nb_equa     : number of equations
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: prof_chno
    character(len=24) :: nueq, deeq
    integer :: len_v, nb_equa
!
! --------------------------------------------------------------------------------------------------
!
    prof_chno = prof_chnoz
    nueq = prof_chno//'.NUEQ'
    deeq = prof_chno//'.DEEQ'
    call jelira(deeq, 'LONMAX', len_v)
    call jelira(nueq, 'LONMAX', nb_equa)
    ASSERT(len_v.eq.2*nb_equa)
    if (present(nb_equaz)) then
        nb_equaz = nb_equa
    endif
end subroutine
