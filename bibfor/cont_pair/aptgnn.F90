subroutine aptgnn(sdappa , mesh     , sdcont_defi, model_ndim, jdecno,&
                  nb_node, norm_type, norm_vect)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/asmpi_info.h"
#include "asterc/r8prem.h"
#include "asterfort/cfinvm.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmmron.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmtann.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
#ifdef _USE_MPI
#include "mpif.h"
#include "asterf_mpi.h"
#endif
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: model_ndim
    integer, intent(in) :: jdecno
    integer, intent(in) :: nb_node
    integer, intent(in) :: norm_type
    real(kind=8), intent(in) :: norm_vect(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents at each node by smoothing - On current zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  model_ndim       : dimension of model
! In  jdecno           : shift in contact datastructure for the beginning of nodes in contact zone
! In  nb_node          : number of nodes in contact zone
! In  norm_type        : type of normal => 'AUTO'(0)/'FIXE'(1)/'VECT_Y'(2)
! In  norm_vect        : vector if normal is 'FIXE' or 'VECT_Y'
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: node_name, elem_name, valk(2)
    mpi_int :: i_proc, nb_proc, mpicou
    integer :: nb_poin_mpi, nbr_poin_mpi, idx_start, idx_end
    integer :: elem_indx, elem_nume, node_indx(1), node_nume(1)
    integer :: node_nbelem, elem_nbnode
    integer :: jdeciv
    integer :: i_node, i_elem, i_node_curr, i_elem_node
    integer :: niverr
    aster_logical :: one_proc
    real(kind=8) :: tau1(3), tau2(3), normal(3), normn
    real(kind=8) :: tau1_node(3), tau2_node(3)
    real(kind=8) :: vnorm(3), noor
    character(len=24) :: sdappa_tgel, sdappa_tgno
    real(kind=8), pointer :: v_sdappa_tgel(:) => null()
    real(kind=8), pointer :: v_sdappa_tgno(:) => null()
    integer, pointer :: v_mesh_connex(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    one_proc=.false.
!
! - Acces to pairing datastructure
!
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    call jeveuo(sdappa_tgno, 'E', vr = v_sdappa_tgno)
!
! - Mpi informations
!
    call asmpi_comm('GET', mpicou)
    call asmpi_info(mpicou,rank=i_proc , size=nb_proc)
    if(one_proc)then
            nb_proc = 1
            i_proc= 0
    endif
    nb_poin_mpi  = int(nb_node/nb_proc)
    nbr_poin_mpi = nb_node-nb_poin_mpi*nb_proc
    idx_start    = 1+(i_proc)*nb_poin_mpi
    idx_end      = idx_start+nb_poin_mpi-1+nbr_poin_mpi*int((i_proc+1)/nb_proc)
!
! - Loop on nodes
!
    do  i_node= idx_start, idx_end
!
        normal(1:3) = 0.d0
        tau1_node(1:3) = 0.d0
        tau2_node(1:3) = 0.d0
!
! ----- Current node
!
        node_indx(1) = i_node+jdecno
        call cfnumn(sdcont_defi, 1, node_indx(1), node_nume(1))
        call jenuno(jexnum(mesh//'.NOMNOE', node_nume(1)), node_name)
!
! ----- Number of elements attached to node
!
        call cfnben(sdcont_defi, node_indx(1), 'CONINV', node_nbelem, jdeciv)
!
! ----- Loop on elements attached to node
!
        do i_elem = 1, node_nbelem
!
! --------- Get elements attached to current node
!
            call cfinvm(sdcont_defi, jdeciv, i_elem, elem_indx)
!
! --------- Index and name of element
!
            call cfnumm(sdcont_defi, elem_indx, elem_nume)
            call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
            valk(1) = elem_name
            valk(2) = node_name
!
! --------- Access to connectivity
!
            call jeveuo(jexnum(mesh//'.CONNEX', elem_nume), 'L', vi = v_mesh_connex)
!
! --------- Number of nodes
!
            call cfnben(sdcont_defi, elem_indx, 'CONNEX', elem_nbnode)
!
! --------- Get current index of node
!
            i_node_curr = 0
            do i_elem_node = 1, elem_nbnode
                if (v_mesh_connex(i_elem_node) .eq. node_nume(1)) then
                    i_node_curr = i_elem_node
                endif
            end do
            ASSERT(i_node_curr.ne.0)
!
! --------- Access to current tangent
!
            call jeveuo(jexnum(sdappa_tgel, elem_indx), 'L', vr = v_sdappa_tgel)
            tau1(1) = v_sdappa_tgel(6*(i_node_curr-1)+1)
            tau1(2) = v_sdappa_tgel(6*(i_node_curr-1)+2)
            tau1(3) = v_sdappa_tgel(6*(i_node_curr-1)+3)
            tau2(1) = v_sdappa_tgel(6*(i_node_curr-1)+4)
            tau2(2) = v_sdappa_tgel(6*(i_node_curr-1)+5)
            tau2(3) = v_sdappa_tgel(6*(i_node_curr-1)+6)
!
! --------- Compute normal
!
            call mmnorm(model_ndim, tau1, tau2, vnorm, noor)
            if (noor .le. r8prem()) then
                call utmess('F', 'APPARIEMENT_15', nk=2, valk=valk)
            endif
!
! --------- Add normal
!
            normal(1) = normal(1) + vnorm(1)
            normal(2) = normal(2) + vnorm(2)
            normal(3) = normal(3) + vnorm(3)
        end do
!
! ----- Mean square
!
        normal(1) = normal(1) / node_nbelem
        normal(2) = normal(2) / node_nbelem
        normal(3) = normal(3) / node_nbelem
        call normev(normal, normn)
        if (normn .le. r8prem()) then
            call utmess('F', 'APPARIEMENT_16', sk=node_name)
        endif
!
! ----- New local basis after smoothing
!
        call mmmron(model_ndim, normal, tau1_node, tau2_node)
!
! ----- For VECT_Y
!
        if (norm_type .eq. 2) then
            call dcopy(3, norm_vect, 1, tau2_node, 1)
            call provec(normal, tau2_node, tau1_node)
        endif
!
! ----- New local basis after smoothing
!
        call mmtann(model_ndim, tau1_node, tau2_node, niverr)
        if (niverr .ne. 0) then
            call utmess('F', 'APPARIEMENT_17', sk=node_name)
        endif
!
! ----- Save tangents
!
        v_sdappa_tgno(6*(node_indx(1)-1)+1) = tau1_node(1)
        v_sdappa_tgno(6*(node_indx(1)-1)+2) = tau1_node(2)
        v_sdappa_tgno(6*(node_indx(1)-1)+3) = tau1_node(3)
        v_sdappa_tgno(6*(node_indx(1)-1)+4) = tau2_node(1)
        v_sdappa_tgno(6*(node_indx(1)-1)+5) = tau2_node(2)
        v_sdappa_tgno(6*(node_indx(1)-1)+6) = tau2_node(3)
    end do
!
    call jedema()
!
end subroutine
