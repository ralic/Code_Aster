subroutine caddlp(load, mesh, ligrmo, vale_type)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/char_beam_lcs.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/char_read_keyw.h"
#include "asterfort/cncinv.h"
#include "asterfort/dismoi.h"
#include "asterfort/getnode.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/matloc.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
! Keyword = 'DDL_POUTRE'
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
    integer :: n_max_keyword
    parameter (n_max_keyword=300)
    integer :: ddlimp(n_max_keyword)
    real(kind=8) :: valimr(n_max_keyword)
    complex(kind=8) :: valimc(n_max_keyword)
    character(len=8) :: valimf(n_max_keyword)
    character(len=16) :: keywordlist(n_max_keyword)
!
!
    integer :: cmp_nb_glo
    parameter (cmp_nb_glo=6)
    integer :: cmp_acti_glo(cmp_nb_glo)
    real(kind=8) :: cmp_valr_glo(cmp_nb_glo)
    complex(kind=8) :: cmp_valc_glo(cmp_nb_glo)
    character(len=8) :: cmp_valf_glo(cmp_nb_glo)
    character(len=16) :: cmp_name_glo(cmp_nb_glo)
!
    integer :: nddli, iocc, ibid, ino
    integer :: ier, nbec, nbnoeu, n_keyword
    integer :: jdirec,  nume_node
    integer :: jnom, nbcmp,  jprnm
    real(kind=8) :: zero
    character(len=24) :: keywordexcl
    character(len=4) :: coef_type
    integer :: n_keyexcl
    integer :: i_keyword
    character(len=24) :: list_node
    integer :: jlino
    integer :: nb_node
    character(len=8) :: model, k8bid, nomg, name_node
    character(len=16) :: keywordfact, keyword
    character(len=19) :: lisrel, k19bid
    character(len=19) :: ncncin
    integer, pointer :: dimension(:) => null()
    integer, pointer :: icompt(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'DDL_POUTRE'
    call getfac(keywordfact, nddli)
    if (nddli .eq. 0) goto 999
!
! - Initializations
!
    lisrel = '&&CADDLP.RLLISTE'
    zero = 0.d0
!
! - Reverse connectivity
!
    ncncin = '&&CADDLP.CONINV'
    call jeexin(ncncin, ier)
    if (ier .eq. 0) call cncinv(mesh, [ibid], 0, 'V', ncncin)
!
    coef_type = 'REEL'
    ASSERT(vale_type .eq. 'REEL')
    model = ligrmo(1:8)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
! - Create list of excluded keywords for using in load_read_keyw
!
    keywordexcl = '&&CADDLP.KEYWORDEXCL'
    call char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k8bid)
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
!
! - Local coordinate system
!
    call jelira(mesh//'.NOMNOE', 'NOMMAX', nbnoeu, k8bid)
    call wkvect('&&CADDLP.DIRECT', 'V V R', 3*nbnoeu, jdirec)
    AS_ALLOCATE(vi=dimension, size=nbnoeu)
!
! - Loop on factor keyword
!
    do iocc = 1, nddli
!
! ----- Read mesh affectation
!
        list_node = '&&CADDLP.LIST_NODE'
        call getnode(mesh, keywordfact, iocc, 'F', list_node, &
                     nb_node)
        call jeveuo(list_node, 'L', jlino)
!
! ----- Counting components
!
        AS_ALLOCATE(vi=icompt, size=6)
!
! ----- Loop on nodes
!
        do ino = 1, nb_node
            nume_node = zi(jlino-1+ino)
            call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
!
! --------- Read affected components and their values
!
            call char_read_keyw(keywordfact, iocc, vale_type, n_keyexcl, keywordexcl,&
                                n_max_keyword, n_keyword, keywordlist, ddlimp, valimr,&
                                valimf, valimc)
            ASSERT(n_keyword.le.cmp_nb_glo)
!
! --------- Change components with local coordinate system
!
            call char_beam_lcs(mesh, model, ncncin, keywordfact, iocc,&
                               nume_node, name_node, keywordlist,  n_keyword, valimr, cmp_name_glo,&
                               cmp_acti_glo, cmp_valr_glo)
!
! --------- Final linear relation
!
            call afddli(model, nbcmp, zk8(jnom), nume_node, name_node,&
                        zi(jprnm-1+(nume_node-1)*nbec+1), dimension(nume_node),&
                        zr(jdirec+3*(nume_node-1)), coef_type, cmp_nb_glo, cmp_name_glo,&
                        cmp_acti_glo, vale_type, cmp_valr_glo, cmp_valf_glo, cmp_valc_glo,&
                        icompt, lisrel, .false._1, ibid, ibid,&
                        k19bid, k19bid, k19bid, k19bid, mesh)
        enddo
        do i_keyword = 1, 6
            keyword = cmp_name_glo(i_keyword)
            if (cmp_acti_glo(i_keyword) .eq. 1) then
                if (icompt(i_keyword) .eq. 0) then
                    call utmess('F', 'CHARGES2_45', sk=keyword)
                endif
            endif
        enddo
        AS_DEALLOCATE(vi=icompt)
        call jedetr(list_node)
    enddo
!
! - Final linear relation affectation
!
    call aflrch(lisrel, load)
!
    call jedetr('&&CADDLP.DIRECT')
    AS_DEALLOCATE(vi=dimension)
    call jedetr(ncncin)
!
999 continue
!
    call jedema()
!
end subroutine
