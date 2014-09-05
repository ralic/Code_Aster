subroutine xfem_calc_pc(action, nbnoxfem, neq_mloc, nnz_mloc, deca,&
                        tab_mloc, scal, iret, is_svd)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! BUT : CALCUL DU PRE-CONDITIONNEUR XFEM
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/xfem_calc_chol.h"
#include "asterfort/xfem_calc_inv.h"
#include "asterfort/xfem_calc_svd.h"
#include "asterfort/xfem_calc_mult.h"
!-----------------------------------------------------------------------
    character(len=*) :: action
    integer :: nbnoxfem, deca, iret
    integer :: neq_mloc(nbnoxfem)
    integer, optional :: nnz_mloc(nbnoxfem)
    real(kind=8) :: scal, tab_mloc(deca*nbnoxfem)
    aster_logical, optional :: is_svd(nbnoxfem)
!-----------------------------------------------------------------------
    integer :: j, nm, jadr
    character(len=8) :: methode
!-----------------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(scal .gt. 0.d0)
    ASSERT((action.eq.'CHOLESKY').or.(action.eq.'INV_TRI_SUP').or.(action.eq.'INV_AND_SCAL'))
    iret=0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (action .eq. 'CHOLESKY') then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ASSERT(present(is_svd))
        do j = 1, nbnoxfem
            is_svd(j)=.false.
            nm=neq_mloc(j)
            if (present(nnz_mloc)) then
                ASSERT((nm*(nm+1)/2) .eq. nnz_mloc(j))
            endif
            jadr=deca*(j-1)
!
            call xfem_calc_chol(tab_mloc, jadr, nm, scal, iret,&
                                methode)
            if (methode(1:3) .eq. 'SVD') is_svd(j)=.true.
!
            if (iret .ne. 0) then
                call utmess('A', 'XFEMPRECOND_5')
                goto 99
            endif
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    else if (action .eq. 'INV_TRI_SUP') then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do j = 1, nbnoxfem
            nm=neq_mloc(j)
            if (present(nnz_mloc)) then
                ASSERT((nm*(nm+1)/2) .eq. nnz_mloc(j))
            endif
            jadr=deca*(j-1)
!
            call xfem_calc_inv(tab_mloc, jadr, nm, scal, iret)
!
            if (iret .ne. 0) then
                call utmess('A', 'XFEMPRECOND_5')
                goto 99
            endif
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    else if (action .eq. 'INV_AND_SCAL') then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ASSERT(present(is_svd))
        do j = 1, nbnoxfem
            nm=neq_mloc(j)
            if (present(nnz_mloc)) then
                ASSERT((nm*(nm+1)/2) .eq. nnz_mloc(j))
            endif
            jadr=deca*(j-1)
            if (.not. is_svd(j)) then
                call xfem_calc_inv(tab_mloc, jadr, nm, scal, iret)
            else
                call xfem_calc_mult(tab_mloc, jadr, nm, scal, .false._1)
            endif
            if (iret .ne. 0) then
                call utmess('A', 'XFEMPRECOND_5')
                goto 99
            endif
        enddo
!
    endif 
!
 99 continue
!
    call jedema()
!
end subroutine
