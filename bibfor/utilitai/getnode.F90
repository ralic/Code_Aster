subroutine getnode(mesh, keywordfact, iocc, suffix, stop_void,&
                   list_node, nb_node, model)
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
#include "asterfort/utmess.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=8), intent(in) :: suffix
    character(len=1), intent(in) :: stop_void
    integer, intent(out) :: nb_node
    character(len=24), intent(in) :: list_node
    character(len=8), intent(in), optional :: model
!
! --------------------------------------------------------------------------------------------------
!
! Read mesh affectation - Nodes
!
! --------------------------------------------------------------------------------------------------
!
! Create list of elements:
!  - read MAILLE/GROUP_MA/TOUT/NOEU_GROUP_NO keywords
!  - remove by SANS_MAILLE/SANS_GROUP_MA/SANS_NOEUD/SANS_GROUP_NO keywords
!  - can use <SUFFIX> to enhance keyword. For instance:
!           suffix = '_1': GROUP_MA -> GROUP_MA_1
!                          MAILLE -> MAILLE_1
!                          SANS_GROUP_MA -> SANS_GROUP_MA_1
!                          SANS_MAILLE -> SANS_MAILLE_1
!                          GROUP_NO -> GROUP_NO_1
!                          NOEUD -> NOEUD_1
!                          SANS_GROUP_NO -> SANS_GROUP_NO_1
!                          SANS_NOEUD -> SANS_NOEUD_1
!           WARNING ->     TOUT -> TOUT
!  - can stop or alarm if no elements in final list
!
! In  mesh         : name of mesh
! In  keywordfact  : factor keyword to read
! In  iocc         : factor keyword index in AFFE_CHAR_MECA
! In  suffix       : suffix for read
! In  stop_void    : if nb_elem == 0
!                      'F' - Error
!                      'A' - Error
!                      ' ' - Nothing
! In  list_node    : list of nodes read
! Out nb_node      : number of nodes read
! In  model        : <optional> check elements belongs to model
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: moclm(5)
    character(len=16) :: typmcl(5)
    character(len=24) :: list_lect, list_excl
    character(len=24) :: keyword
    character(len=8) :: model_name
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
    nb_lect = 0
    nb_excl = 0
    model_name = ' '
    if (present(model)) model_name = model
!
! - Read nodes
!
    nb_mocl = 0
    if (getexm(keywordfact,'TOUT') .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = 'TOUT'
        typmcl(nb_mocl) = 'TOUT'
    endif
    keyword = 'GROUP_MA'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'GROUP_MA'
    endif
    keyword = 'MAILLE'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'MAILLE'
    endif
    keyword = 'GROUP_NO'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'GROUP_NO'
    endif
    keyword = 'NOEUD'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'NOEUD'
    endif
    if (nb_mocl .ne. 0) then
        call reliem(model_name, mesh, 'NU_NOEUD', keywordfact, iocc,&
                    nb_mocl, moclm, typmcl, list_lect, nb_lect)
    endif
!
! - Read nodes excludes
!
    nb_mocl = 0
    keyword = 'SANS_GROUP_MA'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'GROUP_MA'
    endif
    keyword = 'SANS_MAILLE'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'MAILLE'
    endif
    keyword = 'SANS_GROUP_NO'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'GROUP_NO'
    endif
    keyword = 'SANS_NOEUD'//suffix
    if (getexm(keywordfact,keyword) .eq. 1) then
        nb_mocl = nb_mocl + 1
        moclm(nb_mocl) = keyword
        typmcl(nb_mocl) = 'NOEUD'
    endif
    if (nb_mocl .ne. 0) then
        call reliem(' ', mesh, 'NU_NOEUD', keywordfact, iocc,&
                    nb_mocl, moclm, typmcl, list_excl, nb_excl)
    endif
!
! - Exclusion of nodes in initial list
!
    nb_elim = 0
!
    if (nb_lect .ne. 0) call jeveuo(list_lect, 'E', jlect)
    if (nb_excl .ne. 0) then
        call jeveuo(list_excl, 'L', jexcl)
        do i_excl = 1, nb_excl
            num_excl = zi(jexcl-1+i_excl)
            do i_lect = 1, nb_lect
                num_lect = zi(jlect-1+i_lect)
                if (num_excl .eq. num_lect) then
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
    if ((nb_node.ne.0) .and. (nb_lect.ne.0)) then
        call wkvect(list_node, 'V V I', nb_node, jnode)
        do i_lect = 1, nb_lect
            num_lect = zi(jlect-1+i_lect)
            if (num_lect .ne. 0) then
                i_node= i_node + 1
                zi(jnode-1+i_node) = num_lect
            endif
        end do
        ASSERT(i_node.eq.nb_node)
    endif
!
! - If no nodes
!
    if (stop_void .ne. ' ' .and. nb_node .eq. 0) then
        call utmess(stop_void, 'UTILITY_4', sk=keywordfact)
    endif
!
    call jedetr(list_lect)
    call jedetr(list_excl)
!
    call jedema()
end subroutine
