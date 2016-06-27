subroutine mmptch(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/jerazo.h"
#include "asterfort/jelira.h"
#include "asterfort/jeexin.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Fill pairing datastructure (MPI management)
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: sdappa
    character(len=24) :: sdappa_tgno, sdappa_tgel    
    character(len=24) :: sdappa_mpib, sdappa_mpic
    integer :: iret, length
    character(len=16), pointer :: valk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    length=cfdisi(ds_contact%sdcont_defi,'NNOCO' )
    call jerazo(sdappa_tgno,6*length,1)
    length=0
    call jelira(sdappa_tgel, 'LONT', length)
    call jerazo(sdappa_tgel, length ,1)
    sdappa_mpib = sdappa(1:19)//'.MPIB'
    sdappa_mpic = sdappa(1:19)//'.MPIC'
    call jeexin(sdappa_mpib,iret)
    if (iret .eq. 0) then
        call wkvect(sdappa_mpib,'V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call wkvect(sdappa_mpic,'V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
    else 
        call jeveuo(sdappa_mpib, 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call jeveuo(sdappa_mpic, 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
    endif
end subroutine
