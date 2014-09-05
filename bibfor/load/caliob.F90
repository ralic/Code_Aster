subroutine caliob(load, mesh, ligrmo, vale_type)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8dgrd.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/char_excl_keyw.h"
#include "asterfort/char_read_keyw.h"
#include "asterfort/dismoi.h"
#include "asterfort/getnode.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcadr.h"
#include "asterfort/lxcaps.h"
#include "asterfort/matrot.h"
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
! Keyword = 'LIAISON_OBLIQUE'
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
    character(len=24) :: list_node
    integer :: jlino, nb_node
    integer :: ino
    integer :: ndim, nbec
    integer :: nliai, nume_node
    integer :: i_angle, i_keyword, iocc, i_direct
    real(kind=8) :: coefr, val_r, direct(3)
    character(len=8) :: ddl, coeff, val_f
    complex(kind=8) :: coefc, val_c
    character(len=2) :: typlag
    character(len=4) :: typcoe
    character(len=8) :: model, nomg
    character(len=8) :: name_node
    character(len=16) :: keywordfact, keyword
    integer :: n_keyword
    character(len=19) :: lisrel
    real(kind=8) :: matr_rota(3, 3), rdgd
    real(kind=8) :: zero
    real(kind=8) :: angl_naut(3)
    integer :: n_angle
    character(len=24) :: keywordexcl
    integer :: n_keyexcl
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'LIAISON_OBLIQUE'
    call getfac(keywordfact, nliai)
    if (nliai .eq. 0) goto 999
!
! - Initializations
!
    lisrel = '&&CALIOB.RLLISTE'
    zero = 0.d0
    typlag = '12'
    coefc = (1.0d0,0.0d0)
    coefr = 1.0d0
    coeff = ' '
    rdgd = r8dgrd()
!
    typcoe = 'REEL'
    if (vale_type .eq. 'COMP') then
        ASSERT(.false.)
    endif
    model = ligrmo(1:8)
!
! - Create list of excluded keywords for using in char_read_keyw
!
    keywordexcl = '&&CALIOB.KEYWORDEXCL'
    call char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
!
! - Information about <GRANDEUR>
!
    nomg = 'DEPL_R'
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
!
    call dismoi('DIM_GEOM', model, 'MODELE', repi=ndim)
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
        call utmess('F', 'CHARGES2_6')
    endif
!
! - Loop on factor keyword
!
    do iocc = 1, nliai
!
! ----- Read mesh affectation
!
        list_node = '&&CALIOB.LIST_NODE'
        call getnode(mesh, keywordfact, iocc, 'F', list_node,&
                     nb_node)
        call jeveuo(list_node, 'L', jlino)
!
! ----- Local orientation
!
        angl_naut(1) = zero
        angl_naut(2) = zero
        angl_naut(3) = zero
        call getvr8(keywordfact, 'ANGL_NAUT', iocc=iocc, nbval=3, vect=angl_naut,&
                    nbret=n_angle)
        do i_angle = 1, min(3, abs(n_angle))
            angl_naut(i_angle) = rdgd*angl_naut(i_angle)
        enddo
        call matrot(angl_naut, matr_rota)
!
! ----- Read affected components and their values
!
        call char_read_keyw(keywordfact, iocc, vale_type, n_keyexcl, keywordexcl,&
                            n_max_keyword, n_keyword, keywordlist, ddlimp, valimr,&
                            valimf, valimc)
!
        do i_keyword = 1, n_keyword
            keyword = keywordlist(i_keyword)
!
! --------- Values
!
            val_r = valimr(i_keyword)
            val_c = valimc(i_keyword)
            val_f = valimf(i_keyword)
            ASSERT(ddlimp(i_keyword).eq.1)
!
! --------- Which direction ?
!
            if ((keyword.eq.'DX') .or. (keyword.eq.'DRX')) then
                i_direct = 1
            else if ((keyword.eq.'DY').or.(keyword.eq.'DRY')) then
                i_direct = 2
            else if ((keyword.eq.'DZ').or.(keyword.eq.'DRZ')) then
                i_direct = 3
            else
                ASSERT(.false.)
            endif
            direct(1) = matr_rota(i_direct,1)
            direct(2) = matr_rota(i_direct,2)
            direct(3) = matr_rota(i_direct,3)
!
! --------- Which kind of dof ?
!
            if ((keyword.eq.'DX') .or. (keyword.eq.'DY') .or. (keyword.eq.'DZ')) then
                ddl = 'DEPL'
            else if ((keyword.eq.'DRX').or.(keyword.eq.'DRY').or.(keyword.eq.'DRZ')) then
                ddl = 'ROTA'
            else
                ASSERT(.false.)
            endif
!
! --------- Affect in direction
!
            do ino = 1, nb_node
                nume_node = zi(jlino+ino-1)
                call jenuno(jexnum(mesh//'.NOMNOE', nume_node), name_node)
                call afrela([coefr], [coefc], ddl, name_node, [ndim],&
                            direct, 1, val_r, val_c, val_f,&
                            typcoe, vale_type, typlag, 0.d0, lisrel)
            enddo
        enddo
!
        call jedetr(list_node)
    enddo
!
! - Final linear relation affectation
!
    call aflrch(lisrel, load)
!
    call jedetc('V', '&&CALIOB.RLLISTE', 1)
    call jedetr(keywordexcl)
!
999 continue
    call jedema()
end subroutine
