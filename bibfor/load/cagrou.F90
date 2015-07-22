subroutine cagrou(load, mesh, vale_type, phenom)
!
implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/aflrch.h"
#include "asterfort/agdual.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/getnode.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!  Person in charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: load
    character(len=8), intent(in) :: mesh
    character(len=4), intent(in) :: vale_type
    character(len=4), intent(in) :: phenom
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Keyword = 'LIAISON_UNIF'
!
! --------------------------------------------------------------------------------------------------
!
!
! In mesh      : name of mesh
! In load      : name of load
! In vale_type : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_term
    parameter (nb_term=2)
    real(kind=8) :: coef_real_unit(nb_term)
    complex(kind=8) :: coef_cplx_unit(nb_term)
    character(len=8) :: dof_name(nb_term)
    character(len=8) :: node_name(nb_term)
    integer :: node_nume(nb_term)
    integer :: repe_type(nb_term)
    real(kind=8) :: repe_defi(3, nb_term)
!
    character(len=2) :: lagr_type
    character(len=4) :: coef_type
    character(len=16) :: keywordfact
    character(len=19) :: list_rela
    integer :: ibid
    integer :: nliai
    real(kind=8) :: vale_real_zero
    character(len=8) :: vale_func_zero
    complex(kind=8) :: vale_cplx_zero
    integer :: iocc, i_no, i_dof
    character(len=24) :: list_node
    integer :: jlino
    integer :: nb_node
    character(len=24) :: list_dof
    integer :: jlidof
    integer :: nb_dof
!
    data repe_type /0, 0/
    data repe_defi /0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    keywordfact = 'LIAISON_UNIF'
    call getfac(keywordfact, nliai)
    if (nliai .eq. 0) goto 999
!
! - Initializations
!
    vale_func_zero = '&FOZERO'
    vale_cplx_zero = (0.d0,0.d0)
    vale_real_zero = 0.d0
    coef_cplx_unit(1) = (1.0d0,0.0d0)
    coef_cplx_unit(2) = (-1.0d0,0.0d0)
    coef_real_unit(1) = 1.d0
    coef_real_unit(2) = -1.d0
    list_rela = '&&CAGROU.RLLISTE'
    list_dof = '&&CAGROU.LIST_DOF'
!
! - Initializations of types
!
    lagr_type = '12'
    if (vale_type .eq. 'COMP') then
        ASSERT(.false.)
    else if (vale_type.eq.'REEL') then
        coef_type = 'REEL'
    else if (vale_type.eq.'FONC') then
        coef_type = 'REEL'
    else
        ASSERT(.false.)
    endif
!
    do iocc = 1, nliai
!
! ----- Read mesh affectation
!
        list_node = '&&CAGROU.LIST_NODE'
        call getnode(mesh, keywordfact, iocc, 'F', list_node, &
                     nb_node)
        if (nb_node .lt. 2) then
            call utmess('F', 'CHARGES2_82')
        endif
        call jeveuo(list_node, 'L', jlino)
!
! ----- Get dof
!
        call getvtx(keywordfact, 'DDL', iocc=iocc, nbval=0, nbret=nb_dof)
        ASSERT(nb_dof .ne. 0)
        nb_dof= - nb_dof
        call wkvect(list_dof, 'V V K8', nb_dof, jlidof)
        call getvtx(keywordfact, 'DDL', iocc=iocc, nbval=nb_dof, vect=zk8(jlidof),&
                    nbret=ibid)
!
! ----- First node
!
        node_nume(1) = zi(jlino-1+1)
        call jenuno(jexnum(mesh//'.NOMNOE', node_nume(1)), node_name(1))
!
! ----- Loop on dof
!
        do i_dof = 1, nb_dof
            dof_name(1) = zk8(jlidof-1+i_dof)
            dof_name(2) = zk8(jlidof-1+i_dof)
            do i_no = 2, nb_node
                node_nume(2) = zi(jlino-1+i_no)
                call jenuno(jexnum(mesh//'.NOMNOE', node_nume(2)), node_name(2))
                call afrela(coef_real_unit, coef_cplx_unit, dof_name, node_name, repe_type,&
                            repe_defi, nb_term, vale_real_zero, vale_cplx_zero, vale_func_zero,&
                            coef_type, vale_type, lagr_type, 0.d0, list_rela)
            enddo
        enddo
!
        call jedetr(list_node)
        call jedetr(list_dof)
    end do
!
! - Final linear relation affectation
!
    if (phenom.eq.'MECA') then
        call agdual(load,1,'LIN')
    endif
    call aflrch(list_rela, load)
!
999  continue
    call jedema()
end subroutine
