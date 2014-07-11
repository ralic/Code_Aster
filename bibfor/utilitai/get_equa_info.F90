subroutine get_equa_info(nume_ddlz     , i_equa    , type_equa , nume_nodez  , nume_cmpz,&
                         nume_cmp_lagrz, nume_subsz, nume_linkz, nb_node_lagr, list_node_lagr,&
                         ligrelz)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/nbec.h"
#include "asterfort/exisdg.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/get_lagr_info.h"
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
    character(len=*), intent(in) :: nume_ddlz
    integer, intent(in) :: i_equa
    character(len=*), intent(out) :: type_equa
    integer, optional, intent(out) :: nume_nodez
    integer, optional, intent(out) :: nume_cmpz
    integer, optional, intent(out) :: nume_subsz
    integer, optional, intent(out) :: nume_linkz
    integer, optional, intent(out) :: nume_cmp_lagrz
    integer, optional, intent(out) :: nb_node_lagr
    integer, optional, pointer, intent(out) :: list_node_lagr(:)
    character(len=*), optional, intent(out) :: ligrelz
!
! --------------------------------------------------------------------------------------------------
!
! Get information about dof (node, component, etc.)
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_ddl       : name of numbering (NUME_DDL)
! In  i_equa         : index of equation
! Out type_equa      : type of dof 
!                 / 'A' : physical dof (node+component)
!                 / 'B' : Lagrange dof (boundary condition) simple given boundary condition
!                 / 'C' : Lagrange dof (boundary condition) linear relation
!                 / 'D' : generalized dof - Substructuring
!                 / 'E' : generalized dof - Links
! Out nume_node      : global node index in mesh
! Out nume_cmp       : global component index in GRANDEUR
! Out nume_cmp_lagr  : global component index in GRANDEUR for node linked to Lagrange node
! Out nume_subs      : index of substructure (generalized dof)
! Out nume_link      : index of kinematic link (generalized dof)
! Out nb_node_lagr   : number of nodes linked to lagrange dof
! Out list_node_lagr : pointer to list of nodes linked to lagrange dof
! Out ligrel         : name of LIGREL for non-physical node (Lagrange)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: lili, nueq, orig, desc, deeq
    integer :: nume_node, nume_cmp, nume_subs, nume_link, nume_cmp_lagr
    character(len=19) :: prof_chno, ligrel
    character(len=14) :: nume_ddl
    integer :: iexi, isst
    integer :: idx_gd
    integer :: ino, icmp
    logical :: l_gene
    integer, pointer :: p_nueq(:) => null()
    integer, pointer :: p_desc(:) => null()
    integer, pointer :: p_deeq(:) => null()
    integer, pointer :: p_orig(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nume_ddl  = nume_ddlz
    type_equa = '?'
    nume_node = 0
    nume_cmp  = 0
    nume_subs = 0
    nume_link = 0
    nume_cmp_lagr = 0
!
! - Get name of prof_chno
!
    call dismoi('PROF_CHNO', nume_ddl, 'NUME_DDL', repk=prof_chno)
!
! - PROF_CHNO or PROF_GENE ?
!
    call jeexin(prof_chno//'.DESC', iexi)
    l_gene = (iexi.gt.0)
!
! - Objects in PROF_CHNO/PROF_GENE
!
    deeq = prof_chno(1:19)//'.DEEQ'
    lili = prof_chno(1:19)//'.LILI'
    desc = prof_chno(1:19)//'.DESC'
    orig = prof_chno(1:19)//'.ORIG'
    nueq = prof_chno(1:19)//'.NUEQ'
!
    call jeveuo(deeq, 'L', vi = p_deeq)
    call jeveuo(nueq, 'L', vi = p_nueq)
!
    if (l_gene) then
        call jeveuo(desc, 'L', vi = p_desc)
        ASSERT(p_desc(1).eq.2)
        isst      = p_deeq(2*(i_equa-1)+2)
        if (isst .gt. 0) then
            type_equa = 'D'
            call jeveuo(jexnum(orig, 1), 'L', vi = p_orig)
            nume_subs = p_orig(isst)
        else
            type_equa = 'E'
            isst = -isst
            call jeveuo(jexnum(orig, 2), 'L', vi = p_orig)
            nume_link = p_orig(isst)
        endif
    else
        call dismoi('NUM_GD_SI', nume_ddl, 'NUME_DDL', repi=idx_gd)
!
        ino  = p_deeq(2*(i_equa-1)+1)
        icmp = p_deeq(2*(i_equa-1)+2)
!
! ----- Physical node
!
        if (ino .gt. 0 .and. icmp .gt. 0) then
            type_equa = 'A'
            nume_node = ino
            nume_cmp  = icmp
            goto 70
        endif
!
! ----- Non-Physical node (Lagrange)
!
        if (ino .gt. 0 .and. icmp .lt. 0) then
            type_equa = 'B'
            call get_lagr_info(prof_chno, i_equa, idx_gd, nb_node_lagr, list_node_lagr,&
                               nume_cmp)
            ASSERT(nb_node_lagr.eq.1)
            nume_node     = list_node_lagr(1)
            goto 70
        endif
!
! ----- Non-Physical node (Lagrange) - LIAISON_DDL
!
        if (ino .eq. 0 .and. icmp .eq. 0) then
            type_equa = 'C'
            call get_lagr_info(prof_chno, i_equa, idx_gd, nb_node_lagr, list_node_lagr,&
                               ligrelz = ligrel)
            goto 70
        endif
    endif
!
 70 continue
!
    if (present(nume_nodez)) then
        nume_nodez = nume_node
    endif
    if (present(nume_cmpz)) then
        nume_cmpz  = nume_cmp
    endif
    if (present(nume_cmp_lagrz)) then
        nume_cmp_lagrz = nume_cmp_lagr
    endif
    if (present(nume_subsz)) then
        nume_subsz = nume_subs
    endif
    if (present(nume_linkz)) then
        nume_linkz = nume_link
    endif
    if (present(ligrelz)) then
        ligrelz    = ligrel
    endif
!
end subroutine
