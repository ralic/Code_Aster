subroutine select_dof(list_equa, tabl_equa , list_idx_dof,&
                      nume_ddlz, chamnoz   ,&
                      nb_nodez , list_nodez,&
                      nb_cmpz  , list_cmpz)
!
implicit none
!
#include "asterc/indik8.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
#include "asterfort/exisdg.h"
#include "asterfort/nueq_chck.h"
#include "asterfort/nbec.h"
#include "asterfort/select_dof_gene.h"
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
    integer, pointer, optional, intent(inout) :: list_equa(:)
    integer, pointer, optional, intent(inout) :: tabl_equa(:,:)
    integer, pointer, optional, intent(inout) :: list_idx_dof(:)
    character(len=*), optional, intent(in) :: nume_ddlz
    character(len=*), optional, intent(in) :: chamnoz
    integer, optional, intent(in) :: nb_nodez
    integer, optional, pointer, intent(in) :: list_nodez(:)
    integer, optional, intent(in) :: nb_cmpz
    character(len=8), optional, pointer, intent(in) :: list_cmpz(:)
!
! --------------------------------------------------------------------------------------------------
!
! Select dof from list of nodes and components
!
! --------------------------------------------------------------------------------------------------
!
! Output:
!    list_equa    : vector on complete numbering [1:nb_equa]
!                   for ieq =  [1:nb_equa]
!                      list_equa[ieq] = 0 if node+component not present
!                      list_equa[ieq] = 1 if node+component is present
!    tabl_equa    : table on complete numbering [1:nb_equa, 1:nb_cmp]
!                   for ieq = [1:nb_equa]
!                      for icmp = [1:nb_cmp]
!                         tabl_equa[ieq,icmp] = 0 if node+component not present
!                         tabl_equa[ieq,icmp] = 1 if node+component is present
!    list_idx_dof : vector on list of components [1:nb_cmp]
!                   for icmp = [1:nb_cmp]
!                      list_idx_dof[icmp] = 0   if node+component not present
!                      list_idx_dof[icmp] = ieq if node+component present
!
! IO  list_equa     : list of equations
! IO  tabl_equa     : table of equations by components
! IO  list_idx_dof  : list of index of dof
! In  nume_ddl      : name of numbering (NUME_DDL)
! In  chamno        : name of nodal field (CHAMNO)
! In  nb_node       : number of nodes
! In  list_node     : list of nodes (absolute index in mesh)
! In  nb_cmp        : number of components
! In  list_cmp      : list of components (name)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: desc_gran(10)
    character(len=24) :: lili, prno, nueq
    character(len=24) :: lili_name
    integer :: i_ligr_mesh
    character(len=8) :: name_cmp, mesh
    character(len=19) :: prof_chno, nume_equl, prof_gene
    character(len=14) :: nume_ddl
    integer :: iexi
    logical :: l_matr_dist, l_prof_gene
    integer :: node_nume, idx_gd, length_prno
    integer :: i_equ, i_node, i_cmp, i_dof,  i_cmp_glob, i_ec
    integer :: nb_node, nb_ec, nb_cmp, nb_cmp_gd, nb_node_mesh, nb_cmp_node
    integer, pointer :: cmp_sele(:) => null()
    integer, pointer :: node_sele(:) => null()
    integer, pointer :: v_prno(:) => null()
    integer, pointer :: v_nueq(:) => null()
    integer, pointer :: v_nugl(:) => null()
    character(len=8), pointer :: p_cata_cmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prof_chno   = ' '
    nume_ddl = ' '
    l_matr_dist = .false.
!
! - Check output parameters
!
    ASSERT(EXCLUS2(tabl_equa, list_idx_dof))
    ASSERT(EXCLUS2(tabl_equa, list_equa))
    ASSERT(EXCLUS2(list_equa, list_idx_dof))
!
! - Check input parameters
!
    if (present(list_idx_dof)) then
        ASSERT(present(nb_nodez))
        ASSERT(present(list_nodez))
        ASSERT(nb_nodez.eq.1)
    endif
!
! - Get name prof_chno
!
    if (present(nume_ddlz)) then
        nume_ddl = nume_ddlz
        ASSERT(.not.present(chamnoz))
        call dismoi('PROF_CHNO', nume_ddl, 'NUME_DDL', repk=prof_chno)
    elseif (present(chamnoz)) then
        ASSERT(.not.present(nume_ddlz))
        call dismoi('PROF_CHNO', chamnoz  , 'CHAM_NO', repk=prof_chno)
    else
        ASSERT(.false.)
    endif
!
! - Check if nume_ddl is correct (Distributed matrix)
!
    if (present(nume_ddlz)) then
        nume_equl   = nume_ddl//'.NUML'
        call jeexin(nume_equl(1:19)//'.NUGL', iexi)
        l_matr_dist = iexi.ne.0
        if (l_matr_dist) then
            call jeveuo(nume_equl(1:19)//'.NUGL', 'L', vi = v_nugl)
        endif
    endif
!
! - Get GRANDEUR informations
!
    idx_gd = 0
    nb_ec  = 0
    if (present(nume_ddlz)) then
        call dismoi('NUM_GD_SI', nume_ddl, 'NUME_DDL', repi=idx_gd)
    elseif (present(chamnoz)) then
        call dismoi('NUM_GD'   , chamnoz  , 'CHAM_NO' , repi=idx_gd)
    else
        ASSERT(.false.)
    endif
    ASSERT(idx_gd.ne.0)
    nb_ec = nbec(idx_gd)
    ASSERT(nb_ec.le.10)
!
! - Access to catalog
!
    call jelira(jexnum('&CATA.GD.NOMCMP', idx_gd), 'LONMAX', nb_cmp_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', idx_gd), 'L', vk8 = p_cata_cmp)
!
! - Number of components
!
    if (present(list_cmpz)) then
        ASSERT(present(nb_cmpz))
        nb_cmp = nb_cmpz
    else
        nb_cmp = nb_cmp_gd
    endif
!
! - PROF_CHNO or PROF_GENE ?
!
    call jeexin(prof_chno//'.DESC', iexi)
    l_prof_gene = (iexi.gt.0)
    if (l_prof_gene) then
        prof_gene = prof_chno
        call select_dof_gene(prof_gene, nb_cmp, p_cata_cmp, list_cmpz, list_equa,&
                             tabl_equa)
        goto 99
    endif
!
! - Objects in PROF_CHNO
!
    lili      = prof_chno(1:19)//'.LILI'
    prno      = prof_chno(1:19)//'.PRNO'
    nueq      = prof_chno(1:19)//'.NUEQ'
    call jeveuo(nueq, 'L', vi = v_nueq)
!
! - Get mesh
!
    mesh = ' '
    if (present(nume_ddlz)) then
        call dismoi('NOM_MAILLA', nume_ddl, 'NUME_DDL', repk=mesh)
    elseif (present(chamnoz)) then
        call dismoi('NOM_MAILLA', chamnoz  , 'CHAM_NO' , repk=mesh)
    else
        ASSERT(.false.)
    endif
!
! - Get number of nodes
!
    nb_node = 0
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
    if (present(list_nodez)) then
        nb_node = nb_nodez
    else
        nb_node = nb_node_mesh
    endif
!
! - Create index for components
!
    AS_ALLOCATE(vi=cmp_sele, size = nb_cmp_gd)
    do i_cmp = 1, nb_cmp
        if (present(list_cmpz)) then
            name_cmp = list_cmpz(i_cmp)
        else
            name_cmp = p_cata_cmp(i_cmp)
        endif
        i_cmp_glob = indik8(p_cata_cmp, name_cmp, 1, nb_cmp_gd)
        if (i_cmp_glob.ne.0) then
            cmp_sele(i_cmp_glob) = i_cmp
        endif
    end do
!
! - Create index for nodes
!
    AS_ALLOCATE(vi=node_sele, size = nb_node)
    if (present(list_nodez)) then
        do i_node = 1, nb_node
            node_nume = list_nodez(i_node)
            node_sele(i_node) = node_nume
        end do
    else
        do i_node = 1, nb_node
            node_nume = i_node
            node_sele(i_node) = node_nume
        end do
    endif
!
! - Get PRNO object for mesh
!
    i_ligr_mesh = 1
    call jenuno(jexnum(lili, i_ligr_mesh), lili_name)
    ASSERT(lili_name .eq. '&MAILLA')
    call jeveuo(jexnum(prno//'.PRNO', i_ligr_mesh), 'L', vi = v_prno)
    call jelira(jexnum(prno, i_ligr_mesh), 'LONMAX', length_prno)
    ASSERT(length_prno/(nb_ec+2).eq.nb_node_mesh)
!
! - Loop on nodes
!
    if (nb_node.ne.0) then
        do i_node = 1, nb_node
            node_nume   = node_sele(i_node)
            i_dof       = v_prno((nb_ec+2)*(node_nume-1)+1) - 1
            nb_cmp_node = v_prno((nb_ec+2)*(node_nume-1)+2)
            desc_gran(1:10) = 0
            if (nb_cmp_node.ne.0) then
                do i_ec = 1, nb_ec
                    desc_gran(i_ec) = v_prno((nb_ec+2)*(node_nume-1)+2+i_ec)
                end do
            endif
            do i_cmp_glob = 1, nb_cmp_gd
                if (exisdg(desc_gran,i_cmp_glob)) then
                    i_dof      = i_dof + 1
                    i_cmp      = cmp_sele(i_cmp_glob)
                    if (i_cmp.ne.0) then
                        i_equ = v_nueq(i_dof)
                        if (present(list_idx_dof)) then
                            list_idx_dof(i_cmp) = i_equ
                        elseif (present(list_equa)) then
                            list_equa(i_equ) = 1
                        elseif (present(tabl_equa)) then
                            tabl_equa(i_equ, i_cmp) = 1
                        endif
                    endif
                endif
            end do
        end do
    endif
!
    AS_DEALLOCATE(vi=cmp_sele)
    AS_DEALLOCATE(vi=node_sele)
!
99  continue
end subroutine
