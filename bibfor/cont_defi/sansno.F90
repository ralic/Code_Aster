subroutine sansno(sdcont , keywf    , mesh     , sans, psans,&
                  nb_keyw, keyw_type, keyw_name)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/cfzone.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=16), intent(in) :: keywf
    character(len=24), intent(in) :: sans
    character(len=24), intent(in) :: psans
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_keyw
    character(len=16), intent(in) :: keyw_type(nb_keyw)
    character(len=16), intent(in) :: keyw_name(nb_keyw)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Save SANS_ list of nodes (contact or friction) in contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
! In  nb_keyw          : number of keywords to seek
! In  keyw_type        : type of keywords to seek
!                           / 'GROUP_MA'
!                           / 'GROUP_NO'
!                           / 'MAILLE'
!                           / 'NOEUD'
! In  keyw_name        : name of keywords to seek
! In  sans             : name of datastructure to save list of nodes from SANS_* keywords
! In  psans            : name of datastructure to save pointer/contact surf. list of nodes
!                        from SANS_* keywords
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_zone, i_node, i_surf, i_elim
    integer :: jdecno, node_nume, node_nume_elim
    integer :: nb_elim, nb_node, nb_node_elim, nt_node_elim
    integer :: work_vect_len
    integer, pointer :: v_psans(:) => null()
    integer, pointer :: v_sans(:) => null()
    integer :: nb_cont_zone, nb_cont_node
    integer, pointer :: v_vect_work(:) => null()
    character(len=24) :: list_elim
    integer, pointer :: v_list_elim(:) => null()
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
    character(len=24) :: sdcont_pzoneco
    integer, pointer :: v_sdcont_pzoneco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    list_elim = '&&SANSNO.SANSNO'
    nt_node_elim    = 0
!
! - Datastructure for contact definition
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    sdcont_noeuco  = sdcont_defi(1:16)//'.NOEUCO'
    sdcont_pzoneco = sdcont_defi(1:16)//'.PZONECO'
    call jeveuo(sdcont_noeuco , 'L', vi = v_sdcont_noeuco)
    call jeveuo(sdcont_pzoneco, 'L', vi = v_sdcont_pzoneco)
!
! - Parameters
!
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO')
    nb_cont_node = cfdisi(sdcont_defi,'NNOCO')
!
! - Working vector
!
    work_vect_len = nb_cont_zone*nb_cont_node
    AS_ALLOCATE(vi=v_vect_work, size=work_vect_len)
!
! - Create pointer/contact surf. list of nodes from SANS_* keywords
!
    call wkvect(psans, 'G V I', nb_cont_zone+1, vi = v_psans)
    v_psans(1) = 0
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Read list of nodes for SANS_* keyword
!
        call reliem(' ', mesh, 'NU_NOEUD', keywf, i_zone,&
                    nb_keyw, keyw_name, keyw_type, list_elim, nb_elim)
        if (nb_elim .ne. 0) then
            call jeveuo(list_elim, 'L', vi = v_list_elim)
        endif
!
! ----- Nodes belong to contact surface ?
!
        nb_node_elim = 0
        do 50 i_elim = 1, nb_elim
            node_nume_elim = v_list_elim(i_elim)
            call cfzone(sdcont_defi, i_zone, 'ESCL', i_surf)
            call cfnbsf(sdcont_defi, i_surf, 'NOEU', nb_node, jdecno)
            do i_node = 1, nb_node
                node_nume = v_sdcont_noeuco(jdecno+i_node)
                if (node_nume .eq. node_nume_elim) then
                    nb_node_elim = nb_node_elim + 1
                    v_vect_work(1+nt_node_elim-1+nb_node_elim) = node_nume_elim
                    goto 50
                endif
            end do
 50     continue
!
! ----- Update pointer
!
        nt_node_elim = nt_node_elim + nb_node_elim
        ASSERT(nt_node_elim.le.work_vect_len)
        v_psans(i_zone+1) = v_psans(i_zone) + nb_node_elim
    end do
!
! - Create datastructure
!
    if (nt_node_elim .eq. 0) then
        call wkvect(sans, 'G V I', 1     , vi = v_sans)
    else
        call wkvect(sans, 'G V I', nt_node_elim, vi = v_sans)
        do i_node = 1, nt_node_elim
            if (v_vect_work(i_node) .ne. 0) then
                v_sans(i_node) = v_vect_work(i_node)
            endif
        end do
    endif
!
! - Clean
!
    call jedetr(list_elim)
    AS_DEALLOCATE(vi=v_vect_work)
!
end subroutine
