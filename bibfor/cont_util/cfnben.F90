subroutine cfnben(sdcont_defi, enti_indx, enti_type, enti_nb_, enti_jdec_)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
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
    character(len=24), intent(in) :: sdcont_defi
    character(len=6), intent(in) :: enti_type
    integer, intent(in) :: enti_indx
    integer, optional, intent(out) :: enti_nb_
    integer, optional, intent(out) :: enti_jdec_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Access to connectivity tables
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  enti_type        : type of entity
!                        'CONINV' - Inverse connectivity => enti_indx is a node
!                        'CONNEX' - Direct connectivity => enti_indx is an element
! Out enti_nb          : number of entities attached
! Out enti_jdec        : shift in contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: enti_nb, enti_jdec
    character(len=24) :: sdcont_pnomaco
    integer, pointer :: p_sdcont_pnomaco(:) => null()
    character(len=24) :: sdcont_pmanoco
    integer, pointer :: p_sdcont_pmanoco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    enti_nb   = 0
    enti_jdec = 0
!
! - Access to contact datastructure
!
    sdcont_pnomaco = sdcont_defi(1:16)//'.PNOMACO'
    sdcont_pmanoco = sdcont_defi(1:16)//'.PMANOCO'
    call jeveuo(sdcont_pnomaco, 'L', vi = p_sdcont_pnomaco)
    call jeveuo(sdcont_pmanoco, 'L', vi = p_sdcont_pmanoco)
!
! - Get
!
    if (enti_type .eq. 'CONNEX') then
        enti_nb   = p_sdcont_pnomaco(enti_indx+1) - p_sdcont_pnomaco(enti_indx)
        enti_jdec = p_sdcont_pnomaco(enti_indx)
    else if (enti_type.eq.'CONINV') then
        enti_nb   = p_sdcont_pmanoco(enti_indx+1) - p_sdcont_pmanoco(enti_indx)
        enti_jdec = p_sdcont_pmanoco(enti_indx)
    else
        ASSERT(.false.)
    endif
!
    if (present(enti_nb_)) then
        enti_nb_ = enti_nb
    endif
    if (present(enti_jdec_)) then
        enti_jdec_ = enti_jdec
    endif    
!
!
end subroutine
