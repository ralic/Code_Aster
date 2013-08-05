subroutine char_read_mesh(mesh, keywordfact, iocc ,list_node, nb_node, &
                          list_elem, nb_elem )
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keywordfact
    integer, intent(in)  :: iocc
    integer, intent(out) :: nb_node
    character(len=24), intent(in) :: list_node
    integer, intent(out) :: nb_elem
    character(len=24), intent(in) :: list_elem
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Read mesh affectation
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh         : name of mesh
! In  keywordfact  : factor keyword to read
! In  iocc         : factor keyword index in AFFE_CHAR_MECA
! In  list_node    : list of nodes read
! Out nb_node      : number of nodes read
! In  list_elem    : list of elements read
! Out nb_elem      : number of elements read
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: typmcl(5), moclm(5)
    character(len=24) :: list_lect, list_excl
    integer :: nb_mocl
    integer :: nb_lect, nb_excl, nb_elim
    integer :: num_lect, num_excl
    integer :: jlect, jexcl, jnode
    integer :: i_lect, i_excl, i_node
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    list_lect = '&&LIST_LECT'
    list_excl = '&&LIST_EXCL'
    nb_node = 0
    nb_elem = 0
    nb_lect = 0
    nb_excl = 0
!
! - Read nodes
! 
    nb_mocl = 0  
    if (getexm(keywordfact,'TOUT') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'TOUT'
        typmcl(nb_mocl) = 'TOUT'
    endif
    if (getexm(keywordfact,'GROUP_MA') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'GROUP_MA'
        typmcl(nb_mocl) = 'GROUP_MA'
    endif
    if (getexm(keywordfact,'MAILLE') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'MAILLE'
        typmcl(nb_mocl) = 'MAILLE'
    endif
    if (getexm(keywordfact,'GROUP_NO') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'GROUP_NO'
        typmcl(nb_mocl) = 'GROUP_NO'
    endif
    if (getexm(keywordfact,'NOEUD') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'NOEUD'
        typmcl(nb_mocl) = 'NOEUD'
    endif
    if (nb_mocl.ne.0) then
        call reliem(' ', mesh, 'NU_NOEUD', keywordfact, iocc ,&
                    nb_mocl, moclm, typmcl, list_lect, nb_lect)
    endif
!
! - Read elements
!
    nb_mocl = 0  
    if (getexm(keywordfact,'TOUT') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'TOUT'
        typmcl(nb_mocl) = 'TOUT'
    endif
    if (getexm(keywordfact,'GROUP_MA') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'GROUP_MA'
        typmcl(nb_mocl) = 'GROUP_MA'
    endif
    if (getexm(keywordfact,'MAILLE') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'MAILLE'
        typmcl(nb_mocl) = 'MAILLE'
    endif
    if (nb_mocl.ne.0) then
        call reliem(' ', mesh, 'NU_MAILLE', keywordfact, iocc ,&
                    nb_mocl, moclm, typmcl, list_elem, nb_elem)
    endif
!
! - Read nodes exludes
! 
    nb_mocl = 0  
    if (getexm(keywordfact,'SANS_GROUP_MA') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'SANS_GROUP_MA'
        typmcl(nb_mocl) = 'GROUP_MA'
    endif
    if (getexm(keywordfact,'SANS_MAILLE') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'SANS_MAILLE'
        typmcl(nb_mocl) = 'MAILLE'
    endif
    if (getexm(keywordfact,'SANS_GROUP_NO') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'SANS_GROUP_NO'
        typmcl(nb_mocl) = 'GROUP_NO'
    endif
    if (getexm(keywordfact,'SANS_NOEUD') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl)  = 'SANS_NOEUD'
        typmcl(nb_mocl) = 'NOEUD'
    endif
    if (nb_mocl.ne.0) then
        call reliem(' ', mesh, 'NU_NOEUD', keywordfact, iocc ,&
                    nb_mocl, moclm, typmcl, list_excl, nb_excl)
    endif
!
! - Exclusion of nodes in initial list
!
    nb_elim = 0
    if (nb_lect.ne.0) call jeveuo(list_lect,'E',jlect)
    if (nb_excl.ne.0) then
        call jeveuo(list_excl,'L',jexcl)
        do i_excl = 1, nb_excl
            num_excl = zi(jexcl-1+i_excl)
            do i_lect = 1, nb_lect
                num_lect = zi(jlect-1+i_lect)
                if (num_excl.eq.num_lect) then
                    nb_elim = nb_elim + 1
                    zi(jlect-1+i_lect) = 0
                endif
            end do
        end do
    endif
    nb_node = nb_lect - nb_elim
!
! - Final list of nodes
!
    i_node = 0
    if ((nb_node.ne.0).and.(nb_lect.ne.0)) then
        call wkvect(list_node,'V V I',nb_node,jnode)
        do i_lect = 1, nb_lect
            num_lect = zi(jlect-1+i_lect)
            if (num_lect.ne.0) then
                i_node= i_node + 1
                zi(jnode-1+i_node) = num_lect
            endif
        end do
        ASSERT(i_node.eq.nb_node)
    endif
!
    call jedetr(list_lect)
    call jedetr(list_excl)

    call jedema()
end subroutine
