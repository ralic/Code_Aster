subroutine comp_meca_incr(rela_comp, defo_comp, type_comp, l_etat_init)
!
    implicit none
!
#include "asterf_types.h"
#include "asterc/lccree.h"
#include "asterc/lctest.h"
#include "asterc/lcdiscard.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(out) :: type_comp
    aster_logical, optional, intent(in) :: l_etat_init
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Select type of comportment (incremental or total)
!
! --------------------------------------------------------------------------------------------------
!
! In  l_etat_init : .true. if initial state is defined
! In  rela_comp   : comportement RELATION
! In  defo_comp   : type of deformation
! Out type_comp   : type of comportment (incremental or total)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=16) :: rela_code_py
!
! --------------------------------------------------------------------------------------------------
!
    call lccree(1, rela_comp, rela_code_py)
    call lctest(rela_code_py, 'PROPRIETES', 'COMP_ELAS', iret)
    call lcdiscard(rela_code_py)
    if (iret .eq. 0) then
        type_comp = 'COMP_INCR'
    else
        type_comp = 'COMP_ELAS'
        if (present(l_etat_init)) then
            if (l_etat_init) then
                type_comp = 'COMP_INCR'
            endif
        endif
        if (defo_comp .eq. 'PETIT_REAC') then
            type_comp = 'COMP_INCR'
        endif
    endif
!
end subroutine
