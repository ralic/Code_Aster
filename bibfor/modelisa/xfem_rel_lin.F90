subroutine xfem_rel_lin(char, noma, nomo)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/aflrch.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/xrelco.h"
    character(len=8), intent(in) :: char
    character(len=8), intent(in) :: noma
    character(len=8), intent(in) :: nomo
!
! --------------------------------------------------------------------------------------------------
!
! XFEM
!
! Linear relation between cotnact unknows for LBB condition
!
! --------------------------------------------------------------------------------------------------
!
!
! In  noma   : name of mesh
! In  nomo   : name of model
! In  char   : name of load
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nfismx
    parameter    (nfismx=100)
    integer :: ier, ifiss
    integer :: nfiss, nrel
    integer :: jnfis
    character(len=19) :: lisrel
    character(len=24) :: sd_cont_defi
    character(len=24) :: xnrell
    integer :: jxnrel
    character(len=19) :: nliseq
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    lisrel = '&&CAXFEM.RLISTE'
    nrel = 0
!
! - Cracks access
!
    call exixfe(nomo, ier)
    if (ier .eq. 0) then
        call utmess('F', 'XFEM2_8', sk=nomo)
    endif
    call jeveuo(nomo(1:8)//'.NFIS', 'L', jnfis)
    nfiss = zi(jnfis)
    if (nfiss .gt. nfismx) then
        call utmess('F', 'XFEM_2', si=nfismx)
    endif
!
! - Contact data structure access
!
    sd_cont_defi = char(1:8)//'.CONTACT'
    xnrell = sd_cont_defi(1:16)//'.XNRELL'
    call jeveuo(xnrell, 'L', jxnrel)
!
! - Loop on cracks
!
    do ifiss = 1, nfiss
        nliseq = zk24(jxnrel+ifiss-1)(1:19)
        call xrelco(noma, nliseq, lisrel, nrel)
    end do
!
! - Afffectation of linear relations
!
    if (nrel .ne. 0) then
        call aflrch(lisrel, char)
    endif
!
    call jedema()
end subroutine
