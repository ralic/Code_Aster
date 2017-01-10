subroutine select_dof_3(chamnoz, nb_cmp_in, list_equa)
!
implicit none
!
#include "asterc/indik8.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/jexnum.h"
#include "asterfort/exisdg.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/dismoi.h"
#include "asterfort/nbec.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: chamnoz
    integer, intent(in) :: nb_cmp_in
    integer, pointer, intent(in) :: list_equa(:)
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
! In  chamno        : name of nodal field (CHAMNO)
! In  nb_cmp        : number of components
!
! --------------------------------------------------------------------------------------------------
!
    integer :: desc_gran(10)
    character(len=24) :: lili, prno, nueq
    character(len=24) :: lili_name
    integer :: i_ligr_mesh
    character(len=8) :: name_cmp, mesh
    character(len=19) :: prof_chno
    integer :: node_nume, idx_gd, length_prno
    integer :: i_node, i_cmp, i_dof, i_cmp_glob, i_ec, i_equ
    integer :: nb_node, nb_ec, nb_cmp, nb_cmp_gd, nb_node_mesh, nb_cmp_node, nb_cmp_verif
    integer, pointer :: cmp_sele(:) => null()
    integer, pointer :: v_prno(:) => null()
    integer, pointer :: v_nueq(:) => null()
    character(len=8), pointer :: p_cata_cmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prof_chno = ' '
!
! - Get name prof_chno
!
    call dismoi('PROF_CHNO', chamnoz  , 'CHAM_NO', repk=prof_chno)
!
! - Get GRANDEUR informations
!
    idx_gd = 0
    nb_ec  = 0
    call dismoi('NUM_GD'   , chamnoz  , 'CHAM_NO', repi=idx_gd)
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
    nb_cmp = nb_cmp_gd
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
    call dismoi('NOM_MAILLA', chamnoz  , 'CHAM_NO' , repk=mesh)
!
! - Get number of nodes
!
    nb_node = 0
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
    nb_node = nb_node_mesh
!
! - Create index for components
!
    AS_ALLOCATE(vi=cmp_sele, size = nb_cmp_gd)
    do i_cmp = 1, nb_cmp
        name_cmp   = p_cata_cmp(i_cmp)
        i_cmp_glob = indik8(p_cata_cmp, name_cmp, 1, nb_cmp_gd)
        if (i_cmp_glob .ne. 0) then
            cmp_sele(i_cmp_glob) = i_cmp
        endif
    end do
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
    do i_node = 1, nb_node
        node_nume   = i_node
        i_dof       = v_prno((nb_ec+2)*(node_nume-1)+1) - 1
        nb_cmp_node = v_prno((nb_ec+2)*(node_nume-1)+2)
        desc_gran(1:10) = 0
        if (nb_cmp_node .ne. 0) then
            do i_ec = 1, nb_ec
                desc_gran(i_ec) = v_prno((nb_ec+2)*(node_nume-1)+2+i_ec)
            end do
        endif
        nb_cmp_verif = 0
        do i_cmp_glob = 1, nb_cmp_gd
            if (exisdg(desc_gran, i_cmp_glob)) then
                i_dof      = i_dof + 1
                i_cmp      = cmp_sele(i_cmp_glob)
                if (i_cmp .ne. 0) then
                    i_equ = v_nueq(i_dof)
                    nb_cmp_verif = nb_cmp_verif +1
                    list_equa(nb_cmp_in*(node_nume-1)+nb_cmp_verif ) = i_equ

                endif
            endif
        end do
        ASSERT(nb_cmp_verif .eq. nb_cmp_in .or. nb_cmp_verif .eq. 0)
    end do
!
    AS_DEALLOCATE(vi=cmp_sele)
!
end subroutine
