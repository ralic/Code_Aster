subroutine rgndas(nume_ddlz, i_equa , l_print, type_equaz, name_nodez,&
                  name_cmpz, ligrelz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/get_equa_info.h"
#include "asterfort/equa_print.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=*), intent(in) :: nume_ddlz
    integer, intent(in) :: i_equa
    logical, intent(in) :: l_print
    character(len=1), optional, intent(out) :: type_equaz
    character(len=*), optional, intent(out) :: name_nodez
    character(len=*), optional, intent(out) :: name_cmpz
    character(len=*), optional, intent(out) :: ligrelz
!
! --------------------------------------------------------------------------------------------------
!
! Get and/or print information about dof (node, component, etc.)
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_ddl      : name of numbering (NUME_DDL)
! In  i_equa        : index of equation
! In  l_print       : .true. to print equation information
! Out type_equa      : type of dof 
!                 / 'A' : physical dof (node+component)
!                 / 'B' : Lagrange dof (boundary condition) simple given boundary condition
!                 / 'C' : Lagrange dof (boundary condition) linear relation
!                 / 'D' : generalized dof - Substructuring
!                 / 'E' : generalized dof - Links
! Out name_node      : name of the node
! Out name_cmp       : name of the component
! Out ligrel         : name of LIGREL for non-physical node (Lagrange)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: idx_gd, iexi
    character(len=1) :: type_equa
    character(len=8) :: mesh, modl_gene, ligrel
    integer :: nume_node, nume_cmp, nume_cmp_lagr, nume_subs, nume_link
    character(len=19) :: prof_gene
    character(len=14) :: nume_ddl
    character(len=8) :: name_node, name_cmp, name_cmp_lagr, name_subs
    character(len=8), pointer :: p_cata_nomcmp(:) => null()
    character(len=24), pointer :: p_refe(:) => null()
    integer :: nb_node_lagr
    integer, pointer:: list_node_lagr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nume_ddl  = nume_ddlz
    prof_gene = nume_ddl(1:14)//'.NUME'
    ligrel    = ' '
    name_node = ' '
    name_cmp  = ' '
    name_cmp_lagr = ' '
    name_subs = ' '
    nb_node_lagr = 0

    call dismoi('NOM_MAILLA', nume_ddlz, 'NUME_DDL', repk=mesh)
    call dismoi('NUM_GD_SI', nume_ddlz, 'NUME_DDL', repi=idx_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', idx_gd), 'L', vk8 = p_cata_nomcmp)
!
! - Get information about dof
!
    call get_equa_info(nume_ddlz    , i_equa   , type_equa, nume_node   , nume_cmp,&
                       nume_cmp_lagr, nume_subs, nume_link, nb_node_lagr, list_node_lagr,&
                       ligrel)
!
! - Physical dof
!
    if (type_equa.eq.'A') then
        call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
        name_cmp = p_cata_nomcmp(nume_cmp)
    endif
!
! - Non-Physical dof (Lagrange)
!
    if (type_equa.eq.'B') then
        call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
        name_cmp = p_cata_nomcmp(nume_cmp)
        ASSERT(name_cmp.eq.'LAGR')
        name_cmp_lagr = p_cata_nomcmp(nume_cmp_lagr)
    endif
!
! - Non-Physical dof (Lagrange) - LIAISON_DDL
!
    if (type_equa.eq.'C') then
!
    endif
!
! - Generalized dof - Substructuring
!
    if (type_equa.eq.'D') then
        name_cmp = 'GEN'
        call jeexin(prof_gene//'.REFE', iexi)
        if (iexi .gt. 0) then
            call jeveuo(prof_gene//'.REFE', 'L', vk24 = p_refe)
        else
            call jeveuo(prof_gene//'.REFN', 'L', vk24 = p_refe)
        endif
        modl_gene = p_refe(1)(1:8)
        call jeexin(modl_gene//'      .MODG.SSNO', iexi)
        if (iexi .gt. 0) then
            call jenuno(jexnum(modl_gene//'      .MODG.SSNO', nume_subs), name_subs)
        else
            name_subs = 'UNFOUND'
        endif
        name_node = name_subs
    endif
!
! - Generalized dof - Kinematic link
!
    if (type_equa.eq.'E') then
        name_node = 'TAR'
        name_cmp  = 'LAG'
    endif
!
! - Print equation
!
    if (l_print) then
        call equa_print(mesh         , i_equa   , type_equa, name_node   , name_cmp,&
                        name_cmp_lagr, name_subs, nume_link, nb_node_lagr, list_node_lagr,&
                        ligrel)
    endif
!
    if (present(name_nodez)) then
        name_nodez = name_node
    endif
    if (present(name_cmpz)) then
        name_cmpz  = name_cmp
    endif
    if (present(type_equaz)) then
        type_equaz = type_equa
    endif
    if (present(ligrelz)) then
        ligrelz = ligrel
    endif
!
    if (nb_node_lagr.gt.0) then
        AS_DEALLOCATE(vi=list_node_lagr)
    endif
!
end subroutine
