subroutine equa_print(mesh         , i_equa   , type_equa, name_node   , name_cmp,&
                      name_cmp_lagr, name_subs, nume_link, nb_node_lagr, list_node_lagr,&
                      ligrel)
!
implicit none
!
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: mesh
    character(len=1), intent(in) :: type_equa
    integer, intent(in) :: i_equa
    character(len=8), intent(in) :: name_node
    character(len=8), intent(in) :: name_cmp
    character(len=8), intent(in) :: name_cmp_lagr
    character(len=8), intent(in) :: name_subs
    integer, intent(in) :: nume_link
    integer, intent(in) :: nb_node_lagr
    integer, pointer, intent(in) :: list_node_lagr(:)
    character(len=8), intent(in) :: ligrel
!
! --------------------------------------------------------------------------------------------------
!
! Print information about dof (node, component, etc.)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh           : name of the mesh
! In  i_equa         : index of equation
! In  type_equa      : type of dof 
!                 / 'A' : physical dof (node+component)
!                 / 'B' : Lagrange dof (boundary condition) simple given boundary condition
!                 / 'C' : Lagrange dof (boundary condition) linear relation
!                 / 'D' : generalized dof - Substructuring
!                 / 'E' : generalized dof - Links
! In  name_node      : name of the node
! In  name_cmp       : name of the component
! In  name_cmp_lagr  : name of the component if Lagrange simple given boundary condition
! In  name_subs      : name of substructure (generalized dof)
! In  nume_link      : index of kinematic link (generalized dof)
! In  nb_node_lagr   : number of nodes linked to lagrange dof
! In  list_node_lagr : pointer to list of nodes linked to lagrange dof
! In  ligrel         : name of LIGREL for non-physical node (Lagrange)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: valk(2), name_node_lagr
    integer :: i_node, nume_node, vali(2)
!
! --------------------------------------------------------------------------------------------------
!

!
! - Physical dof
!
    if (type_equa.eq.'A') then
        valk(1) = name_node
        valk(2) = name_cmp
        call utmess('I','FACTOR2_1', nk = 2, valk = valk)
    endif
!
! - Non-Physical dof (Lagrange)
!
    if (type_equa.eq.'B') then
        valk(1) = name_node
        valk(2) = name_cmp_lagr
        call utmess('I','FACTOR2_2', nk = 2, valk = valk)
    endif
!
! - Non-Physical dof (Lagrange) - LIAISON_DDL
!
    if (type_equa.eq.'C') then
        call utmess('I','FACTOR2_3', sk = ligrel)
        do i_node = 1, nb_node_lagr
            nume_node = abs(list_node_lagr(i_node))
            call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node_lagr)
            call utmess('I','FACTOR2_4', sk = name_node_lagr)
        end do
    endif
!
! - Generalized dof - Substructuring
!
    if (type_equa.eq.'D') then
        call utmess('I','FACTOR2_5', sk = name_subs, si = i_equa)
    endif
!
! - Generalized dof - Kinematic link
!
    if (type_equa.eq.'E') then
        vali(1) = nume_link
        vali(2) = i_equa
        call utmess('I','FACTOR2_6', ni = 2, vali = vali)
    endif
!
end subroutine
