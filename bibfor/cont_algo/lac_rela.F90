subroutine lac_rela(mesh, ds_contact, iden_rela, l_iden_rela)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeecra.h"
#include "asterfort/jexnum.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/cfdisi.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: iden_rela
    aster_logical, intent(out) :: l_iden_rela
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Transform linear relations into datastructure for elimination of LAGS_C
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  iden_rela        : name of object for identity relations between dof
! Out l_iden_rela      : .true. if have linear relation to suppress
!
! --------------------------------------------------------------------------------------------------
!
    integer :: node_nume(4)
    character(len=8) :: node_name(4), cmp_name
    integer :: iret
    integer :: nt_patch, nb_node, nb_iden_rela, nb_iden_term, nb_iden_dof
    integer :: i_rela, i_node, i_patch
    character(len=24) :: mesh_patch
    character(len=8), pointer :: v_sdiden_term(:) => null()
    integer, pointer :: v_sdiden_info(:) => null()
    integer, pointer :: v_sdiden_dime(:) => null()
    integer, pointer :: v_mesh_patch(:) => null()
!
!
! --------------------------------------------------------------------------------------------------
!
    l_iden_rela  = .false.
    nb_iden_rela = 0
    nb_iden_term = 0
    cmp_name     = 'LAGS_C'
!
! - Get datastructure for linear relations from MESH
!
    mesh_patch = mesh//'.PATCH'
    call jeexin(mesh_patch, iret)
    ASSERT(iret.gt.0)
!
! - Get parameters
!
    nt_patch = ds_contact%nt_patch
!
! - Number of linear relations
!
    do i_patch = 1, nt_patch
!
! ----- Current patch
!
        call jeveuo(jexnum(mesh_patch, i_patch+1), 'L', vi = v_mesh_patch)
!
! ----- Total number of linear relations
!
        if (v_mesh_patch(1) .eq. 25 .or. v_mesh_patch(1) .eq. 26) then
            nb_iden_term = nb_iden_term + 4
            nb_iden_rela = nb_iden_rela + 1
        elseif (v_mesh_patch(1) .eq. 12) then
            nb_iden_term = nb_iden_term + 2
            nb_iden_rela = nb_iden_rela + 1                  
        else
            nb_iden_rela = nb_iden_rela + 0
        end if 
    end do
!
! - End of treatment if no relations found
!
    if (nb_iden_rela .eq. 0) then
        go to 999
    end if 
!
! - Create object for identity relations - Informations
!
    call wkvect(iden_rela(1:19)//'.INFO', 'V V I', 4, vi = v_sdiden_info)
    call wkvect(iden_rela(1:19)//'.DIME', 'V V I', nb_iden_rela, vi = v_sdiden_dime)
!
! - Create object for identity relations - Collection
!
    call jecrec(iden_rela(1:19)//'.COLL', 'V V K8', 'NU', 'CONTIG', 'VARIABLE',&
                nb_iden_rela)
    call jeecra(iden_rela(1:19)//'.COLL', 'LONT',ival=nb_iden_term*2)
    i_rela = 0

    do i_patch = 1, nt_patch
!
! ----- Current patch
!
        call jeveuo(jexnum(mesh_patch, i_patch+1), 'L', vi = v_mesh_patch)
!
! ----- Access to nodes
!
        if (v_mesh_patch(1) .eq. 25 .or. v_mesh_patch(1) .eq. 26) then
            nb_node = 4
        elseif (v_mesh_patch(1) .eq. 12) then
            nb_node = 2      
        else
            nb_node = 0
        end if 
!
! ----- Get nodes of linear relation
!
        if (nb_node .ne. 0) then
            do i_node = 1, nb_node
                node_nume(i_node) = v_mesh_patch(1+i_node)
                call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(i_node)), node_name(i_node))
            end do

! --------- New relation
!
            i_rela = i_rela + 1
!
! --------- Create object in collection
!
            call jecroc(jexnum(iden_rela(1:19)//'.COLL', i_rela))
            call jeecra(jexnum(iden_rela(1:19)//'.COLL', i_rela), 'LONMAX', nb_node*2)
            call jeveuo(jexnum(iden_rela(1:19)//'.COLL', i_rela),'E',vk8 = v_sdiden_term)
!
! --------- Set object in collection
!
            v_sdiden_dime(i_rela)=nb_node
            do i_node= 1, nb_node                    
                v_sdiden_term((i_node-1)*2+1) = node_name(i_node)
                v_sdiden_term((i_node-1)*2+2) = cmp_name
            end do
        end if
    end do
!
! - Total number of terms in identities
!
    nb_iden_term = nb_iden_term
!
! - Total number of same terms
!
    nb_iden_dof  = nb_iden_term-nb_iden_rela
!
    ASSERT(i_rela.eq.nb_iden_rela)
!
    v_sdiden_info(1) = nb_iden_rela
    v_sdiden_info(2) = nb_iden_term
    v_sdiden_info(3) = nb_iden_dof
!
999 continue
!
    l_iden_rela = nb_iden_rela.gt.0
!
end subroutine
