subroutine vrcomp_chck_cmp(mesh, nb_elem,&
                           compor_curr, compor_curr_r, compor_prev_r, &
                           vari_r, comp_comb_2,&
                           ligrel_curr, ligrel_prev,&
                           no_same_spg, no_same_cmp, l_modif_vari)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_elem
    character(len=*), intent(in)  :: compor_curr
    character(len=19), intent(in) :: compor_curr_r
    character(len=19), intent(in) :: compor_prev_r
    character(len=19), intent(in) :: vari_r
    character(len=48), intent(in) :: comp_comb_2
    character(len=19), intent(in) :: ligrel_curr
    character(len=19), intent(in) :: ligrel_prev
    logical, intent(out) :: no_same_spg
    logical, intent(out) :: no_same_cmp
    logical, intent(out) :: l_modif_vari
!
! --------------------------------------------------------------------------------------------------
!
! Check compatibility of comportments
!
! Check if elements have the same number of internal variables and Gauss-subpoints
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh          : name of mesh
! In  nb_elem       : number of elements for current comportment
! In  compor_curr   : current comportment
! In  compor_curr_r : reduced field for current comportment
! In  compor_prev_r : reduced field for previous comportment
! In  vari_r        : reduced field for internal variable
! In  comp_comb_2   : list of comportments can been mixed with all other ones
! In  ligrel_curr   : current LIGREL
! In  ligrel_prev   : previous LIGREL
! Out no_same_spg   : .true. if note the same number of Gauss-subpoints
! Out no_same_cmp   : .true. if note the same number of components
! Out l_modif_vari  : .true. to change the structure of internal variables field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iad1, iad2, iadp, iadm
    integer :: i_elem, k, vali(3)
    logical :: elem_in_curr, elem_in_prev
    integer :: idx_comb_prev, idx_comb_curr
    logical :: all_is_zero
    integer :: nb_pg_prev, nb_spg_prev, nb_cmp_prev
    integer :: nb_spg_curr, nb_cmp_curr
    character(len=16) :: rela_comp_prev, rela_comp_curr
    character(len=8) :: name_elem
    character(len=19) :: dcel
    integer, pointer :: repm(:) => null()
    integer, pointer :: repp(:) => null()
    integer :: jdceld,  jdcell
    integer, pointer :: dcelv(:) => null()
    character(len=8), pointer :: dcelk(:) => null()
    integer :: jcoppl, jcoppd
    character(len=16), pointer :: coppv(:) => null()
    character(len=8), pointer :: coppk(:) => null()
    integer :: jcopmd, jcopml
    character(len=16), pointer :: copmv(:) => null()
    integer :: jce2d,  jce2l
    real(kind=8), pointer :: ce2v(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    l_modif_vari = .false.
    no_same_cmp  = .false.
    no_same_spg  = .false.
!
! - Access to LIGREL
!
    call jeveuo(ligrel_curr//'.REPE', 'L', vi=repp)
    call jeveuo(ligrel_prev//'.REPE', 'L', vi=repm)
!
! - Acces to reduced field on internal variables
!
    call jeveuo(vari_r//'.CESD', 'L', jce2d)
    call jeveuo(vari_r//'.CESV', 'L', vr=ce2v)
    call jeveuo(vari_r//'.CESL', 'L', jce2l)
!
! - Acces to reduced CARTE DCEL_I (see CESVAR) on current comportement
!
    dcel = compor_curr
    call jeveuo(dcel//'.CESD', 'L', jdceld)
    call jeveuo(dcel//'.CESV', 'L', vi=dcelv)
    call jeveuo(dcel//'.CESL', 'L', jdcell)
    call jeveuo(dcel//'.CESK', 'L', vk8=dcelk)
!
! - Acces to reduced CARTE on current comportement
!
    call jeveuo(compor_curr_r//'.CESD', 'L', jcoppd)
    call jeveuo(compor_curr_r//'.CESV', 'L', vk16=coppv)
    call jeveuo(compor_curr_r//'.CESL', 'L', jcoppl)
    call jeveuo(compor_curr_r//'.CESK', 'L', vk8=coppk)
!
! - Acces to reduced CARTE on previous comportement
!
    if (compor_prev_r.ne.' ') then
        call jeveuo(compor_prev_r//'.CESD', 'L', jcopmd)
        call jeveuo(compor_prev_r//'.CESV', 'L', vk16=copmv)
        call jeveuo(compor_prev_r//'.CESL', 'L', jcopml)
    endif
!
! - Check on mesh
!  
    do i_elem = 1, nb_elem
        elem_in_prev = repm(2*(i_elem-1)+1).gt.0
        elem_in_curr = repp(2*(i_elem-1)+1).gt.0
        call cesexi('C', jdceld, jdcell, i_elem, 1,&
                    1, 1, iad1)
        call cesexi('C', jdceld, jdcell, i_elem, 1,&
                    1, 2, iad2)
        call cesexi('C', jcoppd, jcoppl, i_elem, 1,&
                    1, 1, iadp)
!
! ----- No comportment on this element -> next element
!
        if (iad1 .le. 0) then
            goto 40
        endif
!
! ----- Number of Gauss points/components
!
        ASSERT(iad2.gt.0)
        nb_spg_curr = dcelv(iad1)
        nb_cmp_curr = dcelv(iad2)
        nb_pg_prev  = zi(jce2d-1+5+4*(i_elem-1)+1)
        nb_spg_prev = zi(jce2d-1+5+4*(i_elem-1)+2)
        nb_cmp_prev = zi(jce2d-1+5+4*(i_elem-1)+3)
!
! ----- Check number of Gauss sub-points
!
        if (nb_spg_curr .ne. 0 .and. nb_spg_prev .ne. 0) then
            if (nb_spg_curr .ne. nb_spg_prev) then
                call jenuno(jexnum(mesh//'.NOMMAI', i_elem), name_elem)
                vali(1) = nb_spg_prev
                vali(2) = nb_spg_curr
                call utmess('I', 'COMPOR2_52', sk=name_elem, ni=2, vali=vali)
                no_same_spg = .true.
                goto 40
            endif
        endif
!
! ----- Check number of components
!
        if (nb_cmp_curr .ne. nb_cmp_prev) then
!
! --------- This element appears or disappears -> no problem
!
            if ((nb_cmp_prev.eq.0) .or. (nb_cmp_curr.eq.0)) then
                l_modif_vari = .true.
                goto 40
            endif
!
! --------- Current comportement can been mixed -> no problem
!
            ASSERT(iadp.gt.0)
            rela_comp_curr = coppv(iadp)
            idx_comb_curr  = index(comp_comb_2, rela_comp_curr)
            if (idx_comb_curr.gt.0) then
                l_modif_vari = .true.
                goto 40
            endif
!
! --------- Previous comportement can been mixed -> no problem
!
            if (compor_prev_r.ne.' ') then
!
! ------------- Easy to check
!
                call cesexi('C', jcopmd, jcopml, i_elem, 1,&
                            1, 1, iadm)
                ASSERT(iadm.gt.0)
                rela_comp_prev = copmv(iadm)
                idx_comb_prev  = index(comp_comb_2, rela_comp_prev)
                if (idx_comb_prev.gt.0) then
                    l_modif_vari = .true.
                    goto 40
                endif
            else
!
! ------------- Not easy to check: only one component and all is zero
!
                if (nb_cmp_prev .eq. 1) then
                    call cesexi('C', jce2d, jce2l, i_elem, 1,&
                                1, 1, iad2)
                    ASSERT(iad2.gt.0)
                    all_is_zero = .true.
                    do k = 1, nb_pg_prev*nb_spg_curr
                        if (ce2v(iad2+k-1) .ne. 0.d0) then
                            all_is_zero = .false.
                        endif
                    end do
                    if (all_is_zero) then
                        l_modif_vari = .true.
                        goto 40
                    endif
                endif
            endif
        endif
!
! ----- Not the same number of components
!
        if (nb_cmp_curr .ne. nb_cmp_prev) then
            l_modif_vari = .true.
            no_same_cmp  = .true.
            call jenuno(jexnum(mesh//'.NOMMAI', i_elem), name_elem)
            vali(1) = nb_cmp_prev
            vali(2) = nb_cmp_curr
            call utmess('I', 'COMPOR2_53', sk=name_elem, ni=2, vali=vali)
        endif
!
 40     continue
    end do
!
end subroutine
