subroutine xrela_elim(mesh, sdcont_defi, sd_iden_rela)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeecra.h"
#include "asterfort/jexnum.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/cfdisi.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(out) :: sd_iden_rela
!
! --------------------------------------------------------------------------------------------------
!
! XFEM
!
! Transform linear relations into datastructure for elimination
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! Out sd_iden_rela     : name of object for identity relations between dof
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbddl, nb_term_maxi
    parameter  (nbddl=12, nb_term_maxi=6)
    character(len=8) :: ddlc(nbddl)
    integer :: node_nume(nb_term_maxi)
    character(len=8) :: node_name(nb_term_maxi), cmp_name(nb_term_maxi)
!
    character(len=8) :: old_node_name, old_cmp_name
    character(len=8) :: new_node_name, new_cmp_name
    integer :: iret
    integer :: nb_crack, nb_dim, nb_edge, nb_iden_rela, nb_iden_term, nb_iden_dof, nb_term
    integer :: nb_rela_init
    integer :: i_rela, i_dim, i_edge, i_crack, i_term, i_rela_find, i_rela_idx, i_rela_old
    aster_logical :: l_mult_crack, l_rela_find
    character(len=14) :: sdline_crack
    character(len=24) :: sdline
    character(len=8), pointer :: list_rela(:) => null()
    character(len=24), pointer :: v_sdline(:) => null()
    integer, pointer :: v_rela_node(:) => null()
    integer, pointer :: v_rela_cmp(:) => null()
    character(len=8), pointer :: v_sdiden_term(:) => null()
    integer, pointer :: v_sdiden_info(:) => null()
    integer, pointer :: v_sdiden_dime(:) => null()
!
    data ddlc /'LAGS_C','LAGS_F1','LAGS_F2',&
               'LAG2_C','LAG2_F1','LAG2_F2',&
               'LAG3_C','LAG3_F1','LAG3_F2',&
               'LAG4_C','LAG4_F1','LAG4_F2'/
!
! --------------------------------------------------------------------------------------------------
!
    nb_rela_init = 0
    nb_iden_rela = 0
    nb_iden_term = 0
    nb_iden_dof  = 0
    nb_dim       = cfdisi(sdcont_defi,'NDIM' )
!
! - Get datastructure for linear relations from DEFI_CONTACT
!
    sdline = sdcont_defi(1:16)//'.XNRELL'
    call jeexin(sdline, iret)
    if (iret.eq.0) then
        goto 999
    endif
!
! - Informations about linear relations from DEFI_CONTACT
!
    call jelira(sdline, 'LONMAX', nb_crack)
    call jeveuo(sdline, 'L', vk24 = v_sdline)
!
! - Initial number of linear relations
!
    do i_crack = 1, nb_crack
!
! ----- Current crack
!
        sdline_crack = v_sdline(i_crack)(1:14)
!
! ----- Number of edges to eliminate
!
        call jeexin(sdline_crack, iret)
        if (iret.ne.0) then
            call jelira(sdline_crack, 'LONMAX', nb_edge)
            nb_edge      = nb_edge/2
            nb_rela_init = nb_rela_init + nb_dim*nb_edge
        endif
    end do
!
! - End of treatment if no relations found
!
    if (nb_rela_init .eq. 0) then
        goto 999
    end if
!
! - Create working vector
!
    AS_ALLOCATE(vk8=list_rela, size=nb_rela_init*nb_term_maxi*2)
!
! - Eliminate double

    i_rela = 0
    do i_crack = 1, nb_crack
!
! ----- Current crack
!
        sdline_crack = v_sdline(i_crack)(1:14)
!
! ----- Number of edges
!
        call jelira(sdline_crack, 'LONMAX', nb_edge)
        nb_edge = nb_edge/2
!
! ----- Access to nodes
!
        call jeveuo(sdline_crack, 'L', vi = v_rela_node)
!
! ----- For multi-cracks
!
        call jeexin(sdline_crack(1:14)//'_LAGR', iret)
        if (iret .eq. 0) then
            l_mult_crack = .false.
        else
            l_mult_crack = .true.
            call jeveuo(sdline_crack(1:14)//'_LAGR', 'L', vi = v_rela_cmp)
        endif
!
! ----- Loop on edges
!
        do i_edge = 1, nb_edge
!
! --------- Get nodes of linear relation
!
            node_nume(1) = v_rela_node(2*(i_edge-1)+1)
            node_nume(2) = v_rela_node(2*(i_edge-1)+2)
            call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_name(1))
            call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_name(2))
!
! --------- Set linear relation
!
            do i_dim = 1, nb_dim
!
! ------------- Get components name
!
                if (l_mult_crack) then
                    cmp_name(1) = ddlc(3*(v_rela_cmp(2*(i_edge-1)+1)-1)+i_dim)
                    cmp_name(2) = ddlc(3*(v_rela_cmp(2*(i_edge-1)+2)-1)+i_dim)
                else
                    cmp_name(1) = ddlc(i_dim)
                    cmp_name(2) = ddlc(i_dim)
                endif
!
! ------------- Looking for previous relations
!
                l_rela_find = .false.
                i_rela_find = 0
                do i_rela_old = 1, i_rela
                    do i_term = 1, nb_term_maxi
                        old_node_name = list_rela(2*nb_term_maxi*(i_rela_old-1)+2*(i_term-1)+1)
                        old_cmp_name  = list_rela(2*nb_term_maxi*(i_rela_old-1)+2*(i_term-1)+2)
                        if ((old_node_name.eq.node_name(1)).and.(old_cmp_name.eq.cmp_name(1))) then
                            l_rela_find   = .true.
                            i_rela_find   = i_rela_old
                            new_node_name = node_name(2)
                            new_cmp_name  = cmp_name(2)
                            goto 20
                        endif
                        if ((old_node_name.eq.node_name(2)).and.(old_cmp_name.eq.cmp_name(2))) then
                            l_rela_find   = .true.
                            i_rela_find   = i_rela_old
                            new_node_name = node_name(1)
                            new_cmp_name  = cmp_name(1)
                            goto 20
                        endif
                    end do
                end do
!
! ------------- Existing relation
!
 20             continue
                if (l_rela_find) then
                    i_rela_idx = 0
                    do i_term = 1, nb_term_maxi
                        node_name(i_term) = list_rela(2*nb_term_maxi*(i_rela_find-1)+2*(i_term-1)+1)
                        if (node_name(i_term).ne.' ') then
                            i_rela_idx = i_rela_idx + 1
                        endif
                    end do
                    if (i_rela_idx.ge.nb_term_maxi) then
                        call utmess('F','XFEM_53')
                    endif
                    old_node_name = list_rela(2*nb_term_maxi*(i_rela_find-1)+2*(i_rela_idx-1)+1)
                    old_cmp_name  = list_rela(2*nb_term_maxi*(i_rela_find-1)+2*(i_rela_idx-1)+2)
                    if ((old_node_name.ne.new_node_name).and.&
                        (old_cmp_name.ne.new_cmp_name)) then
                        i_rela_idx = i_rela_idx + 1
                        list_rela(2*nb_term_maxi*(i_rela_find-1)+2*(i_rela_idx-1)+1) = new_node_name
                        list_rela(2*nb_term_maxi*(i_rela_find-1)+2*(i_rela_idx-1)+2) = new_cmp_name
                        nb_iden_term = nb_iden_term + 1
                    endif
                endif
!
! ------------- New relation
!
                if (.not.l_rela_find) then
                    i_rela = i_rela + 1
                    list_rela(2*nb_term_maxi*(i_rela-1)+1) = node_name(1)
                    list_rela(2*nb_term_maxi*(i_rela-1)+2) = cmp_name(1)    
                    list_rela(2*nb_term_maxi*(i_rela-1)+3) = node_name(2)
                    list_rela(2*nb_term_maxi*(i_rela-1)+4) = cmp_name(2)
                    nb_iden_term = nb_iden_term + 2            
                endif
            end do
        end do
    end do
!
! - Total number of linear relations
!
    nb_iden_rela = i_rela
!
! - Create object for identity relations - Informations
!
    sd_iden_rela = '&&IDENRELA'
    call wkvect(sd_iden_rela(1:19)//'.INFO', 'V V I', 4, vi = v_sdiden_info)
    call wkvect(sd_iden_rela(1:19)//'.DIME', 'V V I', nb_iden_rela, vi = v_sdiden_dime)
!
! - Create object for identity relations - Collection
!
    call jecrec(sd_iden_rela(1:19)//'.COLL', 'V V K8', 'NU', 'CONTIG', 'VARIABLE',&
                nb_iden_rela)
    call jeecra(sd_iden_rela(1:19)//'.COLL', 'LONT', ival=nb_iden_term*2)
!
! - Set objects
!
    do i_rela = 1, nb_iden_rela
!
! ----- Terms of relation
!
        nb_term = 0
        do i_term = 1, nb_term_maxi
            node_name(i_term) = list_rela(2*nb_term_maxi*(i_rela-1)+2*(i_term-1)+1)
            cmp_name(i_term)  = list_rela(2*nb_term_maxi*(i_rela-1)+2*(i_term-1)+2)
            if (node_name(i_term).ne.' ') then
                nb_term = nb_term + 1
            endif
        end do
!
! ----- Create object in collection
!
        call jecroc(jexnum(sd_iden_rela(1:19)//'.COLL', i_rela))
        call jeecra(jexnum(sd_iden_rela(1:19)//'.COLL', i_rela), 'LONMAX',nb_term*2)
        call jeveuo(jexnum(sd_iden_rela(1:19)//'.COLL', i_rela), 'E', vk8 = v_sdiden_term)
!
! ----- Set object in collection
!
        ASSERT(nb_term.ge.2)
        v_sdiden_dime(i_rela) = nb_term
        do i_term = 1, nb_term
            v_sdiden_term(2*(i_term-1)+1) = node_name(i_term)
            v_sdiden_term(2*(i_term-1)+2) = cmp_name(i_term)
        end do
    end do
!
! - Total number of same terms
!
    nb_iden_dof  = nb_iden_term-nb_iden_rela
!
    v_sdiden_info(1) = nb_iden_rela
    v_sdiden_info(2) = nb_iden_term
    v_sdiden_info(3) = nb_iden_dof
!
    AS_DEALLOCATE(vk8=list_rela)
!
999 continue

end subroutine
