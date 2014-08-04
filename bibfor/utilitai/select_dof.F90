subroutine select_dof(list_equa, tabl_equa , list_idx_dof,&
                      nume_ddlz, prof_chnoz, chamnoz     , gran_name   ,&
                      only_mesh, all_cmp   ,&
                      nb_nodez , list_node ,&
                      nb_cmpz  , list_cmp)
!
implicit none
!
#include "jeveux.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, pointer, optional, intent(inout) :: list_equa(:)
    integer, pointer, optional, intent(inout) :: tabl_equa(:,:)
    integer, pointer, optional, intent(inout) :: list_idx_dof(:)
    character(len=*), optional, intent(in) :: nume_ddlz
    character(len=*), optional, intent(in) :: prof_chnoz
    character(len=*), optional, intent(in) :: chamnoz
    character(len=8), optional, intent(in) :: gran_name
    logical, optional, intent(in) :: only_mesh
    logical, optional, intent(in) :: all_cmp
    integer, optional, intent(in) :: nb_nodez
    integer, optional, pointer, intent(in) :: list_node(:)
    integer, optional, intent(in) :: nb_cmpz
    character(len=8), optional, pointer, intent(in) :: list_cmp(:)
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
! In  prof_chno     : name of profile (PROF_CHNO)
! In  chamno        : name of nodal field (CHAMNO)
! In  gran_name     : name of GRANDEUR if PROF_CHNO (automatic if CHAMNO/NUME_DDL)
! In  only_mesh     : .true. if find component on physical nodes only (more efficient)
! In  nb_node       : number of nodes
! In  list_node     : list of nodes (absolute index in mesh) 
! In  all_cmp       : .true. to select equations for all components on nodes
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
    character(len=19) :: prof_chno, chamno, nume_equl, prof_gene
    character(len=14) :: nume_ddl
    integer :: iexi
    logical :: l_matr_dist, l_prof_gene
    integer :: node_nume, idx_gd
    integer :: i_equa, i_node, i_cmp, i_decal_cmp, i_ligr, i_equa_l, i_node_eq, i_cmp_gd, i_ec
    integer :: nb_node, nb_ec, nb_cmp, nb_cmp_gd, nb_ligr, nb_cmp_node
    integer, pointer :: cata_to_cmp(:) => null()
    integer, pointer :: v_nueq(:) => null()
    integer, pointer :: v_nugl(:) => null()
    integer, pointer :: v_prno(:) => null()
    character(len=8), pointer :: p_cata_cmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prof_chno   = ' '
    chamno      = ' '
    nume_ddl    = ' '
    l_matr_dist = .false.
!
! - Check components parameters
!
    if (.not.present(all_cmp)) then
        ASSERT(present(nb_cmpz))
        nb_cmp   = nb_cmpz
        ASSERT(present(list_cmp))
    endif
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
        ASSERT(present(list_node))
        ASSERT(nb_nodez.eq.1)
    endif
!
! - Get name of nume_ddl/prof_chno/chamno
!
    if (present(nume_ddlz)) then
        nume_ddl = nume_ddlz
        ASSERT(.not.present(prof_chnoz))
        ASSERT(.not.present(chamnoz))
        call dismoi('PROF_CHNO', nume_ddl, 'NUME_DDL', repk=prof_chno)
    elseif (present(prof_chnoz)) then
        prof_chno = prof_chnoz
        ASSERT(.not.present(nume_ddlz))
        ASSERT(.not.present(chamnoz))
        nume_ddl  = prof_chno(1:14)
    elseif (present(chamnoz)) then
        chamno = chamnoz
        ASSERT(.not.present(prof_chnoz))
        ASSERT(.not.present(nume_ddlz))
        call dismoi('PROF_CHNO', chamno   , 'CHAM_NO', repk=prof_chno)
    else
        ASSERT(.false.)
    endif
!
! - Check if nume_ddl is correct (Distributed matrix)
!
    if (.not.present(chamnoz)) then
        call jeexin(nume_ddl(1:14)//'.NSLV', iexi)
        if (iexi.gt.0) then
            nume_equl   = nume_ddl//'.NUML'
            call jeexin(nume_equl(1:19)//'.NUGL', iexi)
            l_matr_dist = iexi.ne.0
            if (l_matr_dist) then
                call jeveuo(nume_equl(1:19)//'.NUGL', 'L', vi = v_nugl)
            endif
        endif
    endif
!
! - Get GRANDEUR informations
!
    idx_gd = 0
    nb_ec  = 0
    if (present(nume_ddlz)) then
        call dismoi('NUM_GD_SI', nume_ddl , 'NUME_DDL', repi=idx_gd)
    elseif (present(chamnoz)) then
        call dismoi('NUM_GD'   , chamno   , 'CHAM_NO' , repi=idx_gd)
    elseif (present(prof_chnoz)) then
        ASSERT(present(gran_name))
        call dismoi('NUM_GD_SI', gran_name, 'GRANDEUR', repi=idx_gd)
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
    if (present(all_cmp)) then
        nb_cmp = nb_cmp_gd
    endif
!
! - Objects in PROF_CHNO
!
    lili      = prof_chno(1:19)//'.LILI'
    prno      = prof_chno(1:19)//'.PRNO'
    nueq      = prof_chno(1:19)//'.NUEQ'
    call jeveuo(nueq, 'L', vi = v_nueq)
!
! - PROF_CHNO or PROF_GENE ?
!
    call jeexin(prof_chno//'.DESC', iexi)
    l_prof_gene = (iexi.gt.0)
    if (l_prof_gene) then
        prof_gene = prof_chno
        call select_dof_gene(prof_gene, nb_cmp, p_cata_cmp, list_cmp, list_equa,&
                             tabl_equa)
        goto 99
    endif
!
! - Get mesh
!
    mesh         = ' '
    if (.not.present(list_node)) then
        if (present(nume_ddlz)) then
            call dismoi('NOM_MAILLA', nume_ddl, 'NUME_DDL', repk=mesh)
        elseif (present(chamnoz)) then
            call dismoi('NOM_MAILLA', chamno  , 'CHAM_NO' , repk=mesh)
        elseif (present(prof_chnoz)) then
            mesh = ' '
        else
            ASSERT(.false.)
        endif
    endif
!
! - Get number of nodes
!
    nb_node = 0
    if (.not.present(prof_chnoz)) then
        if (present(list_node)) then
            nb_node = nb_nodez
        endif
    endif
!
! - Get GRANDEUR informations
!
    idx_gd = 0
    nb_ec  = 0
    if (present(nume_ddlz)) then
        call dismoi('NUM_GD_SI', nume_ddl , 'NUME_DDL', repi=idx_gd)
    elseif (present(chamnoz)) then
        call dismoi('NUM_GD'   , chamno   , 'CHAM_NO' , repi=idx_gd)
    elseif (present(prof_chnoz)) then
        ASSERT(present(gran_name))
        call dismoi('NUM_GD_SI', gran_name, 'GRANDEUR', repi=idx_gd)
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
! - Create index for components in GRANDEUR
!
    AS_ALLOCATE(vi=cata_to_cmp, size = nb_cmp_gd)
    do i_cmp = 1, nb_cmp
        if (present(all_cmp)) then
            name_cmp = p_cata_cmp(i_cmp)
        else
            name_cmp = list_cmp(i_cmp)
        endif
        i_cmp_gd              = indik8(p_cata_cmp, name_cmp, 1, nb_cmp_gd)
        if (i_cmp_gd.ne.0) then
            cata_to_cmp(i_cmp_gd) = i_cmp
        endif
    end do
!
! - Get PRNO object for mesh
!
    i_ligr_mesh = 1
    call jenuno(jexnum(lili, i_ligr_mesh), lili_name)
    ASSERT(lili_name .eq. '&MAILLA')
!
! - On all LIGREL or not ?
!
    if (present(only_mesh)) then
        nb_ligr = 1
    else
        call jelira(prno, 'NMAXOC', nb_ligr)
    endif
!
! - Loop on LIGRELs
!
    do i_ligr = 1, nb_ligr
!
! ----- Get number of nodes
!
        if (i_ligr.eq.i_ligr_mesh) then
            if (present(list_node)) then
                nb_node = nb_nodez
            else
                call jelira(jexnum(prno, i_ligr), 'LONMAX', nb_node)
                nb_node = nb_node/(nb_ec+2)
            endif
        else
            call jelira(jexnum(prno, i_ligr), 'LONMAX', nb_node)
            nb_node = nb_node/(nb_ec+2)
        endif
!
! ----- Loop on nodes
!
        if (nb_node.ne.0) then
            call jeveuo(jexnum(prno, i_ligr), 'L', vi = v_prno)

            do i_node = 1, nb_node
!
! ------------- Current node
!
                if (present(list_node)) then
                    node_nume = list_node(i_node)
                else
                    node_nume = i_node
                endif
                ASSERT(node_nume.gt.0)
!
! ------------- Get informations on current node
!
                i_node_eq   = v_prno(((nb_ec+2)*(node_nume-1)+1))
                nb_cmp_node = v_prno(((nb_ec+2)*(node_nume-1)+2))
!
! ------------- Vector containing active components on current node
!
                desc_gran(1:10) = 0
                if (nb_cmp_node.ne.0) then
                    do i_ec = 1, nb_ec
                        desc_gran(i_ec) = v_prno(((nb_ec+2)*(node_nume-1)+2+i_ec))
                    end do
                endif
!
! ------------- Loop on components to seek
!
                i_decal_cmp = 0
                do i_cmp_gd = 1, nb_cmp_gd
                    i_cmp = cata_to_cmp(i_cmp_gd)
                    if (exisdg(desc_gran, i_cmp_gd)) then
                        i_decal_cmp = i_decal_cmp + 1
                        if (i_cmp.ne.0) then
                            i_equa      = v_nueq(i_node_eq)-1+i_decal_cmp
                            if (l_matr_dist) then
                                i_equa_l = v_nugl(i_equa)
                            else
                                i_equa_l = i_equa
                            endif
                            if (present(list_idx_dof)) then
                                list_idx_dof(i_cmp) = i_equa_l
                            elseif (present(list_equa)) then
                                list_equa(i_equa_l) = 1
                            elseif (present(tabl_equa)) then
                                tabl_equa(i_equa_l, i_cmp) = 1
                            endif
                        endif
                    endif
                end do
            end do
        endif
    end do
!
    AS_DEALLOCATE(vi=cata_to_cmp)
!
99  continue
end subroutine
