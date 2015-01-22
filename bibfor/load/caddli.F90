subroutine caddli(keywordfact, load, mesh, ligrmo, vale_type)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getmjm.h"
#include "asterc/indik8.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/char_impo_liai.h"
#include "asterfort/char_read_keyw.h"
#include "asterfort/char_read_val.h"
#include "asterfort/char_xfem.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/getnode.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: keywordfact
    character(len=8), intent(in) :: load
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in) :: vale_type
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Keyword = 'DDL_IMPO/TEMP_IMPO/PRES_IMPO'
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact : factor keyword DDL_IMPO/TEMP_IMPO/PRES_IMPO
! In  mesh        : name of mesh
! In  load        : name of load
! In  ligrmo      : list of elements in model
! In  vale_type   : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n_max_cmp
    parameter (n_max_cmp=300)
    integer :: cmp_nb
    integer :: cmp_acti(n_max_cmp)
    real(kind=8) :: vale_real(n_max_cmp)
    complex(kind=8) :: vale_cplx(n_max_cmp)
    character(len=8) :: vale_func(n_max_cmp)
    character(len=16) :: cmp_name(n_max_cmp)
!
    integer :: iocc, ino, icmp, nume_node
    integer :: jdirec, jprnm, jnom, jcompt
    integer :: nbcmp, nbec, nbnoeu, nddli
    character(len=8) :: name_node, nomg, model
    character(len=19) :: list_rela
    character(len=4) :: coef_type
    character(len=19) :: connex_inv
    character(len=19) :: ch_xfem_stat, ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno
    integer :: jnoxfl, jnoxfv
    aster_logical :: lxfem
    character(len=24) :: list_node
    integer :: jlino
    integer :: nb_node
    aster_logical :: l_liai, l_ocmp
    integer :: val_nb_liai
    real(kind=8) :: val_r_liai
    character(len=8) :: val_f_liai
    character(len=16) :: val_t_liai
    complex(kind=8) :: val_c_liai
    character(len=8) :: liai_cmp_name(6)
    integer :: liai_cmp_index(6)
    integer :: liai_cmp_nb
    real(kind=8) :: liai_vale_real
    character(len=8) :: liai_vale_fonc
    complex(kind=8) :: liai_vale_cplx
    character(len=24) :: keywordexcl
    integer :: n_keyexcl
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getfac(keywordfact, nddli)
    if (nddli .eq. 0) goto 999
!
! - Initializations
!
    list_rela = '&&CADDLI.RLLISTE'
    model = ligrmo(1:8)
!
! - Create list of excluded keywords for using in char_read_keyw
!
    keywordexcl = '&&CADDLI.KEYWORDEXCL'
    call char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
!
! - Type of linear coefficient
!
    if (keywordfact .eq. 'DDL_IMPO') then
        coef_type = 'REEL'
    else if (keywordfact.eq. 'TEMP_IMPO') then
        coef_type = 'REEL'
    else if (keywordfact.eq. 'PRES_IMPO') then
        coef_type = 'COMP'
    else
        ASSERT(.false.)
    endif
!
! - Information about <GRANDEUR>
!
    if (keywordfact .eq. 'DDL_IMPO') then
        nomg = 'DEPL_R'
    else if (keywordfact.eq. 'TEMP_IMPO') then
        nomg = 'TEMP_R'
    else if (keywordfact.eq. 'PRES_IMPO') then
        nomg = 'PRES_C'
    else
        ASSERT(.false.)
    endif
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp)
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
! - Local coordinate system (dummy)
!
    call jelira(mesh//'.NOMNOE', 'NOMMAX', nbnoeu)
    call wkvect('&&CADDLI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
!
! - Xfem fields
!
    call char_xfem(mesh, model, lxfem, connex_inv, ch_xfem_stat,&
                   ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno)
    if (lxfem) then
        call jeveuo(ch_xfem_node//'.CNSL', 'L', jnoxfl)
        call jeveuo(ch_xfem_node//'.CNSV', 'L', jnoxfv)
    endif
!
! - Loop on factor keyword
!
    do iocc = 1, nddli
!
! ----- Read mesh affectation
!
        list_node = '&&CADDLI.LIST_NODE'
        call getnode(mesh, keywordfact, iocc, ' ', list_node,&
                     nb_node)
!
! ----- No nodes (empty groups)
!
        if (nb_node .eq. 0) goto 60
        call jeveuo(list_node, 'L', jlino)
!
! ----- Detection of LIAISON
!
        call char_read_val(keywordfact, iocc, 'LIAISON', 'TEXT', val_nb_liai,&
                           val_r_liai, val_f_liai, val_c_liai, val_t_liai)
        l_liai = val_nb_liai.gt.0
!
! ----- LIAISON case
!
        if (l_liai) then
!
! --------- Counting components
!
            cmp_nb = 6
            call wkvect('&&CADDLI.ICOMPT', 'V V I', cmp_nb, jcompt)
!
! --------- Data preparation for LIAISON
!
            ASSERT(val_nb_liai.eq.1)
            call char_impo_liai(nomg, val_t_liai, liai_cmp_nb, liai_cmp_name, liai_cmp_index,&
                                liai_vale_real, liai_vale_cplx, liai_vale_fonc)
!
! --------- Loop on nodes
!
            do ino = 1, nb_node
                nume_node = zi(jlino-1+ino)
                call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
                cmp_nb = 0
                do icmp = 1, liai_cmp_nb
                    if (exisdg(zi(jprnm-1+(nume_node-1)*nbec+1),liai_cmp_index(icmp))) then
                        cmp_nb = cmp_nb + 1
                        cmp_acti(cmp_nb) = 1
                        cmp_name(cmp_nb) = liai_cmp_name(icmp)
                        vale_real(cmp_nb) = liai_vale_real
                        vale_cplx(cmp_nb) = liai_vale_cplx
                        vale_func(cmp_nb) = liai_vale_fonc
                    endif
                enddo
                call afddli(model, nbcmp, zk8(jnom), nume_node, name_node,&
                            zi(jprnm-1+ (nume_node-1)*nbec+1), 0, zr(jdirec+3*(nume_node-1)),&
                            coef_type, cmp_nb, cmp_name, cmp_acti, vale_type,&
                            vale_real, vale_func, vale_cplx, zi(jcompt), list_rela,&
                            lxfem, jnoxfl, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv, mesh)
            enddo
!
            call jedetr('&&CADDLI.ICOMPT')
        endif
!
! ----- Read affected components and their values
!
        call char_read_keyw(keywordfact, iocc, vale_type, n_keyexcl, keywordexcl,&
                            n_max_cmp, cmp_nb, cmp_name, cmp_acti, vale_real,&
                            vale_func, vale_cplx)
        l_ocmp = cmp_nb.gt.0
!
! ----- Other cases
!
        if (l_ocmp) then
!
! --------- Counting components
!
            call wkvect('&&CADDLI.ICOMPT', 'V V I', cmp_nb, jcompt)
!
! --------- Loop on nodes
!
            do ino = 1, nb_node
                nume_node = zi(jlino-1+ino)
                call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
                call afddli(model, nbcmp, zk8(jnom), nume_node, name_node,&
                            zi(jprnm-1+ (nume_node-1)*nbec+1), 0, zr(jdirec+3*(nume_node-1)),&
                            coef_type, cmp_nb, cmp_name, cmp_acti, vale_type,&
                            vale_real, vale_func, vale_cplx, zi(jcompt), list_rela,&
                            lxfem, jnoxfl, jnoxfv, ch_xfem_stat, ch_xfem_lnno,&
                            ch_xfem_ltno, connex_inv, mesh)
            enddo
            do icmp = 1, cmp_nb
                if (zi(jcompt-1+icmp) .eq. 0) then
                    call utmess('F', 'CHARGES2_45', sk=cmp_name(icmp))
                endif
            enddo
        endif
!
 60     continue
!
        call jedetr('&&CADDLI.ICOMPT')
        call jedetr(list_node)
!
    end do
!
! - Final linear relation affectation
!
    call aflrch(list_rela, load)
!
    call jedetr('&&CADDLI.DIRECT')
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
