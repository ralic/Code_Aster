subroutine cflema(sdcont_defi , nb_cont_surf, nb_cont_elem0, v_list_elem, v_poin_elem,&
                  nb_cont_elem)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    integer, intent(in) :: nb_cont_surf
    integer, intent(in) :: nb_cont_elem0
    integer, intent(inout) :: nb_cont_elem
    integer, pointer, intent(out) :: v_poin_elem(:)
    integer, pointer, intent(out) :: v_list_elem(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress multiple elements - Create list of double elements in the same contact surface
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_elem0    : number of elements of contact (before detection of multiple element)
! IO  nb_cont_elem     : number of elements of contact (after detection of multiple element)
! Out v_list_elem      : pointer to list of non-double elements
! Out v_poin_elem      : pointer to pointer of contact surface
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdecma
    integer :: i_surf, i_elem, ii, elem_nume_1, elem_nume_2, k
    integer :: nb_elem_elim, nb_elem
    integer, pointer :: v_elem_indx(:) => null()
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_elem_elim = 0
!
! - Datastructure for contact definition
!
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    call jeveuo(sdcont_mailco, 'E', vi = v_sdcont_mailco)
!
! - Temporary vectors
!
    AS_ALLOCATE(vi=v_elem_indx, size=nb_cont_elem)
    AS_ALLOCATE(vi=v_poin_elem, size=nb_cont_surf+1)
!
! - Double-element detection
!
    do i_surf = 1, nb_cont_surf
        v_poin_elem(i_surf+1) = v_poin_elem(i_surf)
        call cfnbsf(sdcont_defi, i_surf, 'MAIL', nb_elem, jdecma)
        do 20 i_elem = 1, nb_elem
            elem_nume_1 = v_sdcont_mailco(jdecma+i_elem)
            do ii = 1, i_elem - 1
                elem_nume_2 = v_sdcont_mailco(jdecma+ii)
                if (elem_nume_1 .eq. elem_nume_2) then
                    v_elem_indx(jdecma+i_elem) = 1
                    v_poin_elem(i_surf+1) = v_poin_elem(i_surf+1) + 1
                    nb_elem_elim = nb_elem_elim + 1
                    goto 20
                endif
            end do
20      continue
    end do
!
! - Non-suppressed elements vector
!
    nb_cont_elem = nb_cont_elem0 - nb_elem_elim
    AS_ALLOCATE(vi=v_list_elem, size=nb_cont_elem)
!
! - Copy list of non-suppressed elements
!
    k = 0
    do i_elem = 1, nb_cont_elem0
        if (v_elem_indx(i_elem) .eq. 0) then
            k = k + 1
            v_list_elem(k) = v_sdcont_mailco(i_elem)
        endif
    end do
    ASSERT(k.eq.nb_cont_elem)
!
! - Clean
!
    AS_DEALLOCATE(vi=v_elem_indx)
!
end subroutine
