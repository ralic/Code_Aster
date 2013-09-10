!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "aster_types.h"

interface
    subroutine dgesvx(fact, trans, n, nrhs, a,&
                      lda, af, ldaf, ipiv, equed,&
                      r, c, b, ldb, x,&
                      ldx, rcond, ferr, berr, work,&
                      iwork, info)
        integer, intent(in) :: ldx
        integer, intent(in) :: ldb
        integer, intent(in) :: ldaf
        integer, intent(in) :: lda
        character(len=1) ,intent(in) :: fact
        character(len=1) ,intent(in) :: trans
        integer, intent(in) :: n
        integer, intent(in) :: nrhs
        real(kind=8) ,intent(inout) :: a(lda, *)
        real(kind=8) ,intent(inout) :: af(ldaf, *)
        blas_int ,intent(inout) :: ipiv(*)
        character(len=1) ,intent(inout) :: equed
        real(kind=8) ,intent(inout) :: r(*)
        real(kind=8) ,intent(inout) :: c(*)
        real(kind=8) ,intent(inout) :: b(ldb, *)
        real(kind=8) ,intent(out) :: x(ldx, *)
        real(kind=8) ,intent(out) :: rcond
        real(kind=8) ,intent(out) :: ferr(*)
        real(kind=8) ,intent(out) :: berr(*)
        real(kind=8) ,intent(out) :: work(*)
        blas_int ,intent(out) :: iwork(*)
        blas_int, intent(out) :: info
    end subroutine dgesvx
end interface
