subroutine char_nb_ligf(mesh  , keywordfact, type_late, nb_elem_late, nb_noel_maxi,&
                        suffix)
!
    implicit none
!
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getelem.h"
#include "asterfort/getnode.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
    character(len=16), intent(in) :: keywordfact
    character(len=8), intent(in) :: mesh
    character(len=4), intent(in) :: type_late
    integer, intent(out) :: nb_elem_late
    integer, intent(out) :: nb_noel_maxi
    character(len=*), intent(in), optional :: suffix
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Count number of late elements
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh         : name of mesh
! In  keywordfact  : factor keyword to read
! In  type_late    : type of "late" entity
!                    'Node' -> create POI1 "Late" element
!                    'Elem'
! Out nb_late_elem : number of "late" elements
! Out nb_noel_maxi : maximum number of nodes on "late" elements
! In  suffix       : <optional> suffix for read
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbocc, iocc, ielem, nume_type, nume_elem, nb_noel
    character(len=8) :: suffix_local
    character(len=24) :: list_node
    character(len=24) :: list_elem
    integer, pointer :: p_list_elem(:) => null()
    integer :: nb_node, nb_elem
    integer, pointer :: p_mesh_typmail(:) => null()
    integer, pointer :: p_cata_nbno(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call getfac(keywordfact, nbocc)
    list_node = '&&LIST_NODE'
    list_elem = '&&LIST_ELEM'
    nb_elem_late = 0
    nb_noel_maxi = 0
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = p_mesh_typmail)
!
! - If suffix to keyword needed
!
    if (present(suffix)) then
        suffix_local = suffix
    else
        suffix_local = ' '
    endif
!
! - Get nodes or elements
!
    do iocc = 1, nbocc
        if (type_late.eq.'Node') then
            call getnode(mesh   , keywordfact          , iocc  , ' ', list_node, &
                         nb_node, suffix = suffix_local)
            nb_elem_late = nb_elem_late + nb_node
            nb_noel_maxi = 1
            call jedetr(list_node)
        elseif (type_late.eq.'Elem') then
            call getelem(mesh   , keywordfact          , iocc  , ' ', list_elem, &
                         nb_elem, suffix = suffix_local)
            nb_elem_late = nb_elem_late + nb_elem
            if (nb_elem.ne.0) then
                call jeveuo(list_elem, 'L', vi = p_list_elem)
            endif
            do ielem = 1, nb_elem
                nume_elem = p_list_elem(ielem)
                nume_type = p_mesh_typmail(nume_elem)
                call jeveuo(jexnum('&CATA.TM.NBNO', nume_type), 'L', vi = p_cata_nbno)
                nb_noel      = p_cata_nbno(1)
                nb_noel_maxi = max(nb_noel, nb_noel_maxi)
            end do
            call jedetr(list_elem)
        else
            ASSERT(.false.)
        endif
    end do
!
! - At least one
!
    if (type_late.eq.'Node') then
        if (nb_elem_late.eq.0) then
            nb_elem_late = 1
            nb_noel_maxi = 1
        endif
    endif
!
end subroutine
