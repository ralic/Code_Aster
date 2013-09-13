subroutine calicp(load, mesh, ligrmo, vale_type)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/indik8.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/char_pair_node.h"
#include "asterfort/char_read_tran.h"
#include "asterfort/dismoi.h"
#include "asterfort/drz12d.h"
#include "asterfort/drz13d.h"
#include "asterfort/getnode.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
! Person in charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: load
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in) :: vale_type
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Keyword = 'LIAISON_COQUE'
!
! --------------------------------------------------------------------------------------------------
!
!
! In  mesh        : name of mesh
! In  load        : name of load
! In  ligrmo      : list of elements nume_node model
! In  vale_type   : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: k8dummy, poslag, model
    character(len=2) :: type_lagr
    character(len=16) :: keywordfact
    character(len=19) :: list_rela
    integer :: iocc, i_error, icoupl, ier
    integer :: ndim, nliai, n1, nbec
    character(len=8) :: cmp_name, nomg
    integer :: jnom, jprnm, nb_cmp
    integer :: cmp_index_dx, cmp_index_dy, cmp_index_dz
    integer :: cmp_index_drx, cmp_index_dry, cmp_index_drz
    real(kind=8) :: tran(3), cent(3), angl_naut(3)
    logical :: l_tran, l_cent, l_angl_naut
    character(len=8) :: suffix
    character(len=24) :: list_node_o1, list_node_o2, list_node_i1, list_node_i2
    integer :: nb_node, nb_node_1, nb_node_2
    integer :: j_node_o1, j_node_o2
    integer :: nume_node_1, nume_node_2
    character(len=24) :: list_pair
    integer :: j_list_pair
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'LIAISON_COQUE'
    call getfac(keywordfact, nliai)
    if (nliai .eq. 0) goto 999
!
! - Initializations
!
    list_rela = '&&CALISO.RLLISTE'
    list_node_i1 = '&&CALICP.LIST_NODE_I1'
    list_node_i2 = '&&CALICP.LIST_NODE_I2'
    list_node_o1 = '&&CALICP.LIST_NODE_O1'
    list_node_o2 = '&&CALICP.LIST_NODE_O2'
    list_pair = '&&CALICP.LIST_PAIR'
    type_lagr = '12'
    call wkvect(list_pair, 'V V I', 2, j_list_pair)
!
! - Type
!
    if (vale_type .eq. 'COMP') ASSERT(.false.)
!
! - Access to model
!
    model = ligrmo(1:8)
    call dismoi('F', 'DIM_GEOM', model, 'MODELE', ndim,&
                k8dummy, ier)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
        call utmess('F', 'CHARGES2_6')
    endif
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nb_cmp, k8dummy)
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8dummy, ier)
    ASSERT(nbec.le.10)
!
! - Index in DEPL_R <GRANDEUR> for DX, DY, DZ, DRX, DRY, DRZ
!
    cmp_name = 'DX'
    cmp_index_dx = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name = 'DY'
    cmp_index_dy = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name = 'DZ'
    cmp_index_dz = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name = 'DRX'
    cmp_index_drx = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name = 'DRY'
    cmp_index_dry = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name = 'DRZ'
    cmp_index_drz = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    ASSERT(cmp_index_dx.gt.0)
    ASSERT(cmp_index_dy.gt.0)
    ASSERT(cmp_index_dz.gt.0)
    ASSERT(cmp_index_drx.gt.0)
    ASSERT(cmp_index_dry.gt.0)
    ASSERT(cmp_index_drz.gt.0)
!
! - Loop on factor keyword
!
    do iocc = 1, nliai
!
! ----- Definition of position for lagrange multipliers
!
        call getvtx(keywordfact, 'NUME_LAGR', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .eq. 0) then
            type_lagr = '12'
        else
            call getvtx(keywordfact, 'NUME_LAGR', iocc=iocc, scal=poslag, nbret=n1)
            if (poslag .eq. 'APRES') then
                type_lagr = '22'
            else
                type_lagr = '12'
            endif
        endif
!
! ----- Read nodes - First list
!
        suffix = '_1'
        call getnode(mesh, keywordfact, iocc, suffix, 'F',&
                     list_node_i1, nb_node_1)
!
! ----- Read nodes - Second list
!
        suffix = '_2'
        call getnode(mesh, keywordfact, iocc, suffix, 'F',&
                     list_node_i2, nb_node_2)
!
        if (nb_node_1 .ne. nb_node_2) then
            call utmess('F', 'CHARGES2_8')
        endif
        nb_node = nb_node_1
!
! ----- Create output lists
!
        call wkvect(list_node_o1, 'V V I', nb_node, j_node_o1)
        call wkvect(list_node_o2, 'V V I', nb_node, j_node_o2)
!
! ----- Read transformation
!
        call char_read_tran(keywordfact, iocc, ndim, l_tran, tran,&
                            l_cent, cent, l_angl_naut, angl_naut)
!
! ----- Pairing the two lists with transformation
!
        call char_pair_node(mesh, cent, angl_naut, tran, nb_node,&
                            list_node_i1, list_node_i2, list_node_o1, list_node_o2, i_error)
        if (i_error .ne. 0) then
            call utmess('F', 'CHARGES2_9')
        endif
!
! ----- Compute linear relations
!
        do icoupl = 1, nb_node
            nume_node_1 = zi(j_node_o1-1+icoupl)
            nume_node_2 = zi(j_node_o2-1+icoupl)
            zi(j_list_pair-1+1) = nume_node_1
            zi(j_list_pair-1+2) = nume_node_2
            if (ndim .eq. 2) then
                call drz12d(mesh, ligrmo, vale_type, 2, list_pair,&
                            cmp_index_drz, type_lagr, list_rela)
            else if (ndim .eq. 3) then
                call drz13d(mesh, ligrmo, vale_type, 2, list_pair,&
                            cmp_index_dx, cmp_index_dy, cmp_index_dz, cmp_index_drx,&
                            cmp_index_dry, cmp_index_drz, type_lagr, list_rela)
            endif
        enddo
        call jedetr(list_node_i1)
        call jedetr(list_node_i2)
        call jedetr(list_node_o1)
        call jedetr(list_node_o2)
    end do
!
! - Final linear relation affectation
!
    call aflrch(list_rela, load)
!
    call jedetr(list_pair)
!
999  continue
!
    call jedema()
end subroutine
