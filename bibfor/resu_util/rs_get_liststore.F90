subroutine rs_get_liststore(result_, nb_store, v_list_store_)
!
implicit none
!
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: result_
    integer, intent(out) :: nb_store
    integer, pointer, optional, intent(out) :: v_list_store_(:)
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get list of storing index in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! Out nb_store         : number of storing index in results datastructure
! Out v_list_store     : pointer to list of storing index in results datastructure
!
! Warning : if v_list_store is required, don't forget to allocate object before use it
! First call: get nb_store
! Second call: get list_store
! 
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    integer :: i_dummy, tord(1)
    character(len=8) :: k8_dummy
    real(kind=8) :: r8_dummy
    complex(kind=8) :: c16_dummy
!
! --------------------------------------------------------------------------------------------------
!
    nb_store = 0
    result   = result_
    call rsorac(result   , 'LONUTI', 0       , r8_dummy, k8_dummy,&
                c16_dummy, r8_dummy, k8_dummy, tord    , 1       ,&
                i_dummy)
    nb_store = tord(1)
    if (nb_store .eq. 0) then
        call utmess('F', 'RESULT1_3', sk = result)
    endif
    if (present(v_list_store_)) then
        call rsorac(result   , 'TOUT_ORDRE', 0       , r8_dummy     , k8_dummy,&
                    c16_dummy, r8_dummy    , k8_dummy, v_list_store_, nb_store,&
                    i_dummy)
    endif
!
end subroutine
