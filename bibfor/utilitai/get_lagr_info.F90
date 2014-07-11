subroutine get_lagr_info(prof_chnoz, i_equa , idx_gd, nb_node_lagr, list_node_lagr,&
                         nume_cmpz , ligrelz)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/exisdg.h"
#include "asterfort/nbec.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: prof_chnoz
    integer, intent(in) :: i_equa
    integer, intent(in) :: idx_gd
    integer, intent(out) :: nb_node_lagr
    integer, pointer, intent(out) :: list_node_lagr(:)
    integer, optional, intent(out) :: nume_cmpz
    character(len=*), optional, intent(out) :: ligrelz
!
! --------------------------------------------------------------------------------------------------
!
! Get info about Lagrange dof
!
! --------------------------------------------------------------------------------------------------
!
! In  prof_chno      : name of PROF_CHNO
! In  i_equa         : index of equation for lagrange dof
! In  idx_gd         : index of GRANDEUR in catalog
! Out nb_node_lagr   : number of nodes linked to lagrange dof
! Out list_node_lagr : pointer to list of nodes linked to lagrange dof
! Out nume_cmp       : global component index in GRANDEUR for physical node link to Lagrange
! Out ligrel         : name of LIGREL for non-physical node (Lagrange)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: lili, prno, nueq
    integer :: nume_ligr, nume_node_lagr
    character(len=19) :: prof_chno, ligrel
    integer :: i_cmp, i_nueq, length, idx_node
    integer :: i_elem, i_node, i_ligr, ico, jprno, i_cmp_max
    integer :: ideb, ncmp
    integer :: nb_node, nb_elem, nb_ligr, nb_node_elem, nb_ec, nb_cmp_max, nume_cmp, nume_cmp_loca
    logical :: l_find
    integer, pointer :: p_prno(:) => null()
    integer, pointer :: p_nema(:) => null()
    integer, pointer :: p_nueq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nume_cmp     = 0
    nb_node_lagr = 0
    prof_chno = prof_chnoz
    lili = prof_chno(1:19)//'.LILI'
    prno = prof_chno(1:19)//'.PRNO'
    nueq = prof_chno(1:19)//'.NUEQ'
    call jelira(prno, 'NMAXOC', nb_ligr)
    call jeveuo(nueq, 'L', vi = p_nueq)
    call jelira(jexnum('&CATA.GD.NOMCMP', idx_gd), 'LONMAX', nb_cmp_max)
    nb_ec = nbec(idx_gd)
!
! - Find LIGREL linked to Lagrange dof
!
    l_find = .false.
    do i_ligr = 2, nb_ligr
        call jelira(jexnum(prno, i_ligr), 'LONMAX', length)
        if (length .gt. 0) then
            call jeveuo(jexnum(prno, i_ligr), 'L', vi = p_prno)
            nb_node = length/(nb_ec+2)
            do i_node = 1, nb_node
                ideb = p_prno((i_node-1)*(nb_ec+2)+1)
                ncmp = p_prno((i_node-1)*(nb_ec+2)+2)
                do i_cmp = 1, ncmp
                    i_nueq = p_nueq(ideb-1+i_cmp)
                    if (i_equa .eq. i_nueq) then
                        l_find = .true.
                        nume_node_lagr = i_node
                        nume_ligr      = i_ligr
                        nume_cmp_loca  = i_cmp
                        goto 40
                    endif
                end do
            end do
        endif
    end do
40  continue
    ASSERT(l_find)
    ASSERT(nume_node_lagr.ne.0)
    ASSERT(nume_ligr.gt.1)
!
! - From local to global component index
!
    if (nume_node_lagr.gt.0) then
        ico=0
        call jeveuo(jexnum(prno, nume_ligr), 'L', jprno)
        do i_cmp_max = 1, nb_cmp_max
            if (exisdg(zi(jprno-1+(nume_node_lagr-1)*(nb_ec+2)+3),i_cmp_max)) then
                ico=ico+1
                if (ico .eq. nume_cmp_loca) then
                    nume_cmp = i_cmp_max
                    goto 60
                endif
            endif
        end do
 60     continue
    endif
!
! - Get element linked to Lagrange dof
!
    call jenuno(jexnum(lili, nume_ligr), ligrel)
    call jelira(ligrel//'.NEMA', 'NMAXOC', nb_elem)
!
! - Number of nodes
!
    do i_elem = 1, nb_elem
        call jelira(jexnum(ligrel//'.NEMA', i_elem), 'LONMAX', length)
        nb_node_elem = length - 1
        if (length .ne. 0) then
            call jeveuo(jexnum(ligrel//'.NEMA', i_elem), 'L', vi = p_nema)
            l_find = .false.
            do i_node = 1, nb_node_elem
                if (p_nema(i_node) .eq. -nume_node_lagr) then
                    l_find = .true.
                endif
            end do
            if (l_find) then
                do i_node = 1, nb_node_elem
                    if (p_nema(i_node).gt.0) then
                        nb_node_lagr = nb_node_lagr + 1
                    endif
                end do
            endif
        endif
    end do
!
    AS_ALLOCATE(vi=list_node_lagr, size = nb_node_lagr)
!
! - List of nodes
!
    idx_node = 0
    do i_elem = 1, nb_elem
        call jelira(jexnum(ligrel//'.NEMA', i_elem), 'LONMAX', length)
        nb_node_elem = length - 1
        if (length .ne. 0) then
            call jeveuo(jexnum(ligrel//'.NEMA', i_elem), 'L', vi = p_nema)
            l_find = .false.
            do i_node = 1, nb_node_elem
                if (p_nema(i_node) .eq. -nume_node_lagr) then
                    l_find = .true.
                endif
            end do
            if (l_find) then
                do i_node = 1, nb_node_elem
                    if (p_nema(i_node).gt.0) then
                        idx_node = idx_node+1
                        list_node_lagr(idx_node) = p_nema(i_node)
                    endif
                end do
            endif
        endif
    end do
    ASSERT(idx_node.eq.nb_node_lagr)
!
    if (present(nume_cmpz)) then
        nume_cmpz  = nume_cmp
    endif
    if (present(ligrelz)) then
        ligrelz    = ligrel
    endif
!
end subroutine
