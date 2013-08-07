subroutine caliso(load, mesh, ligrmo, vale_type)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterc/r8gaem.h"
#include "asterfort/aflrch.h"
#include "asterfort/armin.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/drz02d.h"
#include "asterfort/drz03d.h"
#include "asterfort/drz12d.h"
#include "asterfort/drz13d.h"
#include "asterfort/drzrot.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/ltnotb.h"
#include "asterfort/malino.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/getnode.h"
#include "asterfort/char_read_tran.h"
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
! person_in_charge: jacques.pellet at edf.fr, mickael.abbas at edf.fr
!
    character(len=8), intent(in)  :: load
    character(len=8), intent(in)  :: mesh
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in)  :: vale_type
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Keyword = 'LIAISON_SOLIDE'
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
    integer :: iocc, ier
    integer :: jnom, jprnm, n1
    integer :: i_no
    integer :: nb_cmp, nbec, ndim, nliai
    character(len=24) :: list_node
    integer :: jlino, numnoe
    integer :: nb_node
    character(len=2) :: type_lagr
    character(len=8) :: nomg, k8bid, poslag, model
    real(kind=8) :: dist_mini, dist
    character(len=8) :: cmp_name
    character(len=19) :: list_rela
    character(len=16) :: keywordfact
    integer :: iarg
    character(len=24) :: keywordexcl
    integer :: n_keyexcl
    integer :: cmp_index_dx, cmp_index_dy, cmp_index_dz
    integer :: cmp_index_drx, cmp_index_dry, cmp_index_drz
    logical :: l_rota_2d, l_rota_3d
    logical :: l_tran
    real(kind=8) :: tran(3)
    logical :: l_cent
    real(kind=8) :: cent(3)
    logical :: l_angl_naut
    real(kind=8) :: angl_naut(3)
    integer :: n_suffix
    character(len=8) :: list_suffix
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'LIAISON_SOLIDE'
    call getfac(keywordfact, nliai)
    if (nliai .eq. 0) goto 999
!
! - Initializations
!
    list_rela = '&&CALISO.RLLISTE'
    type_lagr = '12'
    l_rota_2d = .false.
    l_rota_3d = .false.
!
! - Type
!
    if (vale_type .eq. 'COMP') ASSERT(.false.)
!
! - Access to model
!
    model = ligrmo(1:8)
    call dismoi('F', 'DIM_GEOM', model, 'MODELE', ndim,&
                k8bid, ier)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    if (.not.(ndim.eq.2.or.ndim.eq.3)) call u2mess('F', 'CHARGES2_6')
!
! - Minimum distance
!
    dist = armin(mesh)
    ASSERT(dist.gt.0.d0)
!
! - Create list of excluded keywords for using in char_read_keyw
!
    keywordexcl = '&&CALISO.KEYWORDEXCL'
    n_suffix    = 0
    list_suffix = ' '
    call char_excl_keyw(keywordfact, n_suffix, list_suffix, keywordexcl, n_keyexcl)
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nb_cmp)
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
    ASSERT(nbec.le.10)
!
! - Index in DEPL_R <GRANDEUR> for DX, DY, DZ, DRX, DRY, DRZ
!
    cmp_name  = 'DX'
    cmp_index_dx = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name  = 'DY'
    cmp_index_dy = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name  = 'DZ'
    cmp_index_dz = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name  = 'DRX'
    cmp_index_drx = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name  = 'DRY'
    cmp_index_dry = indik8(zk8(jnom), cmp_name, 1, nb_cmp)
    cmp_name  = 'DRZ'
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
        call getvtx(keywordfact, 'NUME_LAGR', iocc, iarg, 0,&
                    k8bid, n1)
        if (n1 .eq. 0) then
            type_lagr = '12'
        else
            call getvtx(keywordfact, 'NUME_LAGR', iocc, iarg, 1,&
                        poslag, n1)
            if (poslag .eq. 'APRES') then
                type_lagr = '22'
            else
                type_lagr = '12'
            endif
        endif
!
! ----- Minimum distance
!
        call getvr8(keywordfact, 'DIST_MIN', iocc, iarg, 0,&
                    dist_mini, n1)
        if (n1 .eq. 0) dist_mini = dist*1.d-3
!
! ----- Read mesh affectation
!
        list_node = '&&CALISO.LIST_NODE'
        call getnode(mesh, keywordfact, iocc, list_suffix, 'F', &
                     list_node, nb_node)
        call jeveuo(list_node, 'L', jlino)
!
! ----- Only one node: nothing to do
!
        if (nb_node .eq. 1) then
            call u2mess('I', 'CHARGES2_17')
            goto 998
        endif
!
! ----- Read transformation
!
        call char_read_tran(keywordfact, iocc , ndim, l_tran, tran, &
                            l_cent, cent, l_angl_naut, angl_naut)
        ASSERT(.not.l_cent)
        ASSERT(.not.l_angl_naut)
!
! ----- Apply translation
!
        if (l_tran) then
            call drzrot(mesh, ligrmo, nb_node, list_node, type_lagr,&
                        tran, list_rela)
            goto 998
        endif
!
! ----- Model: 2D
!
        if (ndim .eq. 2) then
!
! --------- Is any node has DRZ dof ?
!
            l_rota_2d = .false.
            do i_no = 1,nb_node
                numnoe = zi(jlino+i_no-1)
                if (exisdg(zi(jprnm-1+(numnoe-1)*nbec+1),cmp_index_drz)) then
                    l_rota_2d = .true.
                    goto 40
                endif
            enddo
40          continue
!
! --------- Compute linear relations
!
            if (l_rota_2d) then
                call drz12d(mesh, ligrmo, vale_type, nb_node, list_node,&
                            cmp_index_drz, type_lagr, list_rela)
            else
                call drz02d(mesh, vale_type, dist_mini, nb_node, list_node,&
                            type_lagr, list_rela)
            endif
!
! ----- Model: 3D
!
        else if (ndim.eq.3) then
!
! --------- Is any node has rotation dofs ?
!
            l_rota_3d = .false.
            do i_no = 1,nb_node
                numnoe = zi(jlino+i_no-1)
                if (exisdg(zi(jprnm-1+(numnoe-1)*nbec+1),cmp_index_drx).and.&
                    exisdg(zi(jprnm-1+(numnoe-1)*nbec+1),cmp_index_dry).and.&
                    exisdg(zi(jprnm-1+(numnoe-1)*nbec+1),cmp_index_drz)) then
                    l_rota_3d = .true.
                    goto 50
                endif
            enddo
50          continue
!
! --------- Compute linear relations
!
            if (l_rota_3d) then
                call drz13d(mesh, ligrmo, vale_type, nb_node, list_node, &
                            cmp_index_dx, cmp_index_dy, cmp_index_dz, cmp_index_drx, cmp_index_dry,&
                            cmp_index_drz, type_lagr, list_rela)
            else
                call drz03d(mesh, vale_type, dist_mini, nb_node, list_node, &
                            type_lagr, list_rela)
            endif
        endif
998     continue
!
        call jedetr(list_node)
!
    end do
!
! - Final linear relation affectation
!
    call aflrch(list_rela, load)
!
    call jedetr(keywordexcl)
!
999 continue
    call jedema()
end subroutine
