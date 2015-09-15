subroutine cafaci(load, mesh, ligrmo, vale_type)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/agdual.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/canort.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/char_read_keyw.h"
#include "asterfort/char_read_val.h"
#include "asterfort/char_xfem.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getelem.h"
#include "asterfort/getnode.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xddlim.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
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
! Keyword = 'FACE_IMPO'
!
! --------------------------------------------------------------------------------------------------
!
!
! In  mesh      : name of mesh
! In  load      : name of load
! In  ligrmo    : list of elements in model
! In  vale_type : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n_max_keyword
    parameter (n_max_keyword=300)
    integer :: nbterm(n_max_keyword)
    real(kind=8) :: vale_real(n_max_keyword)
    complex(kind=8) :: vale_cplx(n_max_keyword)
    character(len=8) :: vale_func(n_max_keyword)
    character(len=16) :: keywordlist(n_max_keyword)
!
    integer :: iocc
    integer :: nbnoeu, jdirec
    integer :: idim, nume_node, nfaci
    integer :: ibid, ndim
    integer :: ino, jprnm, nbec
    integer :: nbcmp, inom
    real(kind=8) :: repe_defi(3)
    integer :: repe_type
    real(kind=8) :: coef_real_unit
    complex(kind=8) :: coef_cplx_unit
    integer :: i_keyword, n_keyword
    character(len=24) :: list_node, list_elem
    integer :: jlino, jlima
    integer :: nb_node, nb_elem
    character(len=2) :: lagr_type
    character(len=4) :: coef_type
    character(len=8) :: model, nomg
    character(len=8) :: name_node, dof_name
    character(len=16) :: keywordfact, keyword
    character(len=19) :: list_rela
    character(len=19) :: connex_inv
    character(len=19) :: ch_xfem_stat, ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno, ch_xfem_heav
    integer :: jnoxfl, jnoxfv
    aster_logical :: lxfem, l_ocmp
    aster_logical :: l_dtan, l_dnor
    integer :: val_nb_dnor, val_nb_dtan
    real(kind=8) :: val_r_dnor, val_r_dtan
    character(len=8) :: val_f_dnor, val_f_dtan
    complex(kind=8) :: val_c_dnor, val_c_dtan
    character(len=16) :: val_t_dnor, val_t_dtan
    character(len=24) :: keywordexcl
    integer :: n_keyexcl
    integer, pointer :: icompt(:) => null()
    real(kind=8), pointer :: tangent(:) => null()
    real(kind=8), pointer :: normale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'FACE_IMPO'
    call getfac(keywordfact, nfaci)
    if (nfaci .eq. 0) goto 999
!
! - Initializations
!
    list_rela = '&&CAFACI.RLLISTE'
    lagr_type = '12'
    coef_cplx_unit = (1.d0,0.d0)
    coef_real_unit = 1.d0
    dof_name = 'DEPL'
!
! - Model informations
!
    model = ligrmo(1:8)
    call dismoi('DIM_GEOM', model, 'MODELE', repi=ndim)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
! - Type of coefficients
!
    coef_type = 'REEL'
    if (vale_type .eq. 'COMP') then
        ASSERT(.false.)
    endif
!
! - Create list of excluded keywords for using nume_node char_read_keyw
!
    keywordexcl = '&&CAFACI.KEYWORDEXCL'
    call char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp)
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
!
! - Local coordinate system (dummy)
!
    call jelira(mesh//'.NOMNOE', 'NOMMAX', nbnoeu)
    call wkvect('&&CAFACI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
!
! - Xfem fields
!
    call char_xfem(mesh, model, lxfem, connex_inv, ch_xfem_stat,&
                   ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno, ch_xfem_heav)
    if (lxfem) then
        call jeveuo(ch_xfem_node//'.CNSL', 'L', jnoxfl)
        call jeveuo(ch_xfem_node//'.CNSV', 'L', jnoxfv)
    endif
!
! - Loop on factor keyword
!
    do iocc = 1, nfaci
!
! ----- Read mesh affectation
!
        list_node = '&&CAFACI.LIST_NODE'
        list_elem = '&&CAFACI.LIST_ELEM'
        call getnode(mesh, keywordfact, iocc, 'F', list_node,&
                     nb_node)
        call getelem(mesh, keywordfact, iocc, 'F', list_elem,&
                     nb_elem)
        call jeveuo(list_node, 'L', jlino)
        call jeveuo(list_elem, 'L', jlima)
!
! ----- Read affected components and their values
!
        call char_read_keyw(keywordfact, iocc, vale_type, n_keyexcl, keywordexcl,&
                            n_max_keyword, n_keyword, keywordlist, nbterm, vale_real,&
                            vale_func, vale_cplx)
!
! ----- Detection of DNOR, DTAN and others
!
        call char_read_val(keywordfact, iocc, 'DNOR', vale_type, val_nb_dnor,&
                           val_r_dnor, val_f_dnor, val_c_dnor, val_t_dnor)
        l_dnor = val_nb_dnor.gt.0
        call char_read_val(keywordfact, iocc, 'DTAN', vale_type, val_nb_dtan,&
                           val_r_dtan, val_f_dtan, val_c_dtan, val_t_dtan)
        l_dtan = val_nb_dtan.gt.0
        l_ocmp = n_keyword.gt.0
!
! ----- Some verifications
!
        if (ndim .eq. 3 .and. l_dtan) then
            call utmess('F', 'CHARGES2_63')
        endif
        if (vale_type .eq. 'FONC') then
            if (l_dnor .or. l_dtan) then
                if (.not.(ndim.eq.2.or.ndim.eq.3)) then
                    call utmess('F', 'CHARGES2_6')
                endif
            endif
        endif
!
! ----- Normals and/or tangents
!
        if (l_dnor) then
            call canort(mesh, nb_elem, zi(jlima), ndim, nb_node,&
                        zi(jlino), 1)
            call jeveuo('&&CANORT.NORMALE', 'L', vr=normale)
        endif
        if (l_dtan) then
            call canort(mesh, nb_elem, zi(jlima), ndim, nb_node,&
                        zi(jlino), 2)
            call jeveuo('&&CANORT.TANGENT', 'L', vr=tangent)
        endif
!
! ----- If DNOR exists
!
        if (l_dnor) then
            do ino = 1, nb_node
                nume_node = zi(jlino+ino-1)
                call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
                do idim = 1, ndim
                    repe_defi(idim) = normale(ndim*(ino-1)+idim)
                end do
!
                if (lxfem) then
                    if (zl(jnoxfl-1+2*nume_node)) then
                        call xddlim(model, dof_name, name_node, nume_node, val_r_dnor,&
                                    val_c_dnor, val_f_dnor, vale_type, ibid, list_rela,&
                                    ndim, repe_defi, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                                    ch_xfem_ltno, connex_inv, mesh, ch_xfem_heav)
                        goto 105
                    endif
                endif
!
                repe_type = ndim
                call afrela([coef_real_unit], [coef_cplx_unit], dof_name, name_node, [repe_type],&
                            repe_defi, val_nb_dnor, val_r_dnor, val_c_dnor, val_f_dnor,&
                            coef_type, vale_type, lagr_type, 0.d0, list_rela)
!
105             continue
            enddo
        endif
!
! ----- If DTAN exists
!
        if (l_dtan) then
            do ino = 1, nb_node
                nume_node = zi(jlino+ino-1)
                call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
                do idim = 1, ndim
                    repe_defi(idim) = tangent(ndim* (ino-1)+idim)
                end do
!
                if (lxfem) then
                    if (zl(jnoxfl-1+2*nume_node)) then
                        call xddlim(model, dof_name, name_node, nume_node, val_r_dtan,&
                                    val_c_dtan, val_f_dtan, vale_type, ibid, list_rela,&
                                    ndim, repe_defi, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                                    ch_xfem_ltno, connex_inv, mesh, ch_xfem_heav)
                        goto 115
                    endif
                endif
!
                repe_type = ndim
                call afrela([coef_real_unit], [coef_cplx_unit], dof_name, name_node, [ndim],&
                            repe_defi, val_nb_dtan, val_r_dtan, val_c_dtan, val_f_dtan,&
                            coef_type, vale_type, lagr_type, 0.d0, list_rela)
!
115             continue
            enddo
        endif
!
! ----- If other components exist
!
        if (l_ocmp) then
!
! --------- Counting components
!
            AS_ALLOCATE(vi=icompt, size=n_keyword)
!
! --------- Linear relation
!
            do ino = 1, nb_node
                nume_node = zi(jlino-1+ino)
                call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
                call afddli(model, nbcmp, zk8(inom), nume_node, name_node,&
                            zi(jprnm-1+ (nume_node- 1)*nbec+1), 0, zr(jdirec+3*(nume_node-1)),&
                            coef_type, n_keyword, keywordlist, nbterm, vale_type,&
                            vale_real, vale_func, vale_cplx, icompt, list_rela,&
                            lxfem, jnoxfl, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv, mesh, ch_xfem_heav)
            enddo
!
! --------- Components doesn't exist on all nodes
!
            do i_keyword = 1, n_keyword
                keyword = keywordlist(i_keyword)
                if (icompt(i_keyword) .eq. 0) then
                    call utmess('F', 'CHARGES2_45', sk=keyword)
                endif
            enddo
!
            AS_DEALLOCATE(vi=icompt)
!
        endif
!
        call jedetr(list_node)
        call jedetr(list_elem)
!
    end do
!
! - Final linear relation affectation
!
    call agdual(load,1,'LIN')
    call aflrch(list_rela, load)
!
    call jedetr('&&CANORT.NORMALE')
    call jedetr('&&CANORT.TANGENT')
    call jedetr('&&CAFACI.DIRECT')
    call jedetr(keywordexcl)
    if (lxfem) then
        call jedetr(connex_inv)
        call detrsd('CHAM_NO_S', ch_xfem_node)
        call detrsd('CHAM_ELEM_S', ch_xfem_stat)
        call detrsd('CHAM_ELEM_S', ch_xfem_lnno)
        call detrsd('CHAM_ELEM_S', ch_xfem_ltno)
    endif
!
999 continue
    call jedema()
end subroutine
