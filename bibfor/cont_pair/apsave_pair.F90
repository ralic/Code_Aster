subroutine apsave_pair(i_zone      , elem_slav_nume,&
                       nb_pair     , list_pair     ,&
                       nb_pair_zone, list_pair_zone)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, intent(in) :: i_zone
    integer, intent(in) :: elem_slav_nume
    integer, intent(in) :: nb_pair
    integer, intent(in) :: list_pair(:)
    integer, intent(inout) :: nb_pair_zone
    integer, pointer, intent(inout) :: list_pair_zone(:)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Save current contact pairs
!
! --------------------------------------------------------------------------------------------------
!
! In  i_zone           : index of contact zone
! In  elem_slav_nume   : current index of slave element
! In  nb_pair          : number of contact pairs to add
! In  list_pair        : list of contact pairs to add
! IO  nb_pair_zone     : number of contact elements
! IO  list_pair_zone   : list of contact elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_pair
!
! --------------------------------------------------------------------------------------------------
!
! ----- Add new pairs
! 
    do i_pair = 1, nb_pair
        list_pair_zone(3*nb_pair_zone+3*(i_pair-1)+1) = elem_slav_nume    
        list_pair_zone(3*nb_pair_zone+3*(i_pair-1)+2) = list_pair(i_pair)
        list_pair_zone(3*nb_pair_zone+3*(i_pair-1)+3) = i_zone
    end do
!
! - New number of contact pairs
!
    nb_pair_zone = nb_pair_zone+nb_pair      
!
end subroutine
