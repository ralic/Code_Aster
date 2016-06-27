subroutine apstoc(ds_contact, nb_pair, list_pair)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/wkvect.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jedetr.h"
#include "asterfort/assert.h"
#include "asterfort/as_deallocate.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    type(NL_DS_Contact), intent(inout) :: ds_contact
    integer, intent(in):: nb_pair
    integer, pointer, intent(inout) :: list_pair(:)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Save pairing information in sdappa data structure
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_contact       : datastructure for contact management
! In  nb_pair          : number of pairs in contact zone
! IO  list_pair        : list of pairs in contact zone  
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_pair
    character(len=19) :: sdappa
    integer, pointer :: v_sdappa_apli(:) => null()
    character(len=24) :: sdappa_apli
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    sdappa_apli = sdappa(1:19)//'.APLI'
    call jedetr(sdappa_apli)
    call wkvect(sdappa_apli,'V V I', 3*nb_pair, vi = v_sdappa_apli)
    do i_pair = 1, 3*nb_pair
        v_sdappa_apli(i_pair) = list_pair(i_pair)        
    end do
    AS_DEALLOCATE(vi=list_pair)
    ds_contact%nb_cont_pair = nb_pair
    call jedema()
end subroutine           
