subroutine comp_meca_elas(comp_elas, nb_cmp, l_etat_init)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
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
    character(len=19), intent(in) :: comp_elas
    integer, intent(in) :: nb_cmp
    aster_logical, intent(in) :: l_etat_init
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Set ELASTIQUE COMPOR
!
! --------------------------------------------------------------------------------------------------
!
! In  comp_elas   : name of ELAS <CARTE> COMPOR
! In  nb_cmp      : number of components in ELAS <CARTE> COMPOR
! In  l_etat_init : .true. if initial state is defined
!
! --------------------------------------------------------------------------------------------------
!
    integer :: icmp
    character(len=16), pointer :: p_compelas_valv(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(nb_cmp .ge. 6)
!
! - Access <CARTE>
!
    call jeveuo(comp_elas(1:19)//'.VALV', 'E', vk16 = p_compelas_valv)
!
! - Init <CARTE>
!
    do icmp = 1, 20
        p_compelas_valv(icmp) = 'VIDE'
    enddo
!
! - Set for ELASTIQUE
!
    p_compelas_valv(1) = 'ELAS'
    p_compelas_valv(2) = '1'
    p_compelas_valv(3) = 'PETIT'
    if (l_etat_init) then
        p_compelas_valv(4) = 'COMP_INCR'
    else
        p_compelas_valv(4) = 'COMP_ELAS'
    endif
    p_compelas_valv(5) = 'ANALYTIQUE'
    write (p_compelas_valv(6) ,'(I16)') 1
! 99999 = Not affected
    write (p_compelas_valv(12),'(I16)') 99999
    write (p_compelas_valv(17) ,'(I16)') 1
    write (p_compelas_valv(18) ,'(I16)') 1
    write (p_compelas_valv(19) ,'(I16)') 1
    write (p_compelas_valv(20) ,'(I16)') 1
!
! - Create <CARTE>
!
    call nocart(comp_elas, 1, nb_cmp)
!
end subroutine
