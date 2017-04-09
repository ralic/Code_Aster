subroutine zgauss(v_matr, v_2mbr, dim, nb, v_solu)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/matfpe.h"
#include "asterfort/assert.h"
#include "blas/zgesvx.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1304,W1306
!
    integer, intent(in) :: dim
    integer, intent(in) :: nb
    complex(kind=8), pointer, intent(in) :: v_matr(:)
    complex(kind=8), pointer, intent(in) :: v_2mbr(:)
    complex(kind=8), pointer, intent(in) :: v_solu(:)
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8) :: af(dim, dim)
    complex(kind=8) :: work(2*dim)
    integer(kind=4) :: ipiv(dim*dim)
    real(kind=8) :: r(dim)
    real(kind=8) :: c(dim)
    real(kind=8) :: ferr(nb)
    real(kind=8) :: berr(nb)
    real(kind=8) :: rwork(2*dim)
    character(len=1) :: equed
    integer(kind=4) :: info
    real(kind=8) :: rcond
!
! --------------------------------------------------------------------------------------------------
!
    call matfpe(-1)
!
    equed = 'N'
    call zgesvx('N', 'T', dim, nb, v_matr,&
                dim, af, dim, ipiv, equed,&
                r, c, v_2mbr, dim, v_solu,&
                dim, rcond, ferr, berr, work,&
                rwork, info)
!
    call matfpe(1)
!
end subroutine
