!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"

interface
    subroutine zggevx(balanc, jobvl, jobvr, sense, n,&
                      a, lda, b, ldb, alpha,&
                      beta, vl, ldvl, vr, ldvr,&
                      ilo, ihi, lscale, rscale, abnrm,&
                      bbnrm, rconde, rcondv, work, lwork,&
                      rwork, iwork, bwork, info)
        integer, intent(in) :: ldvr
        integer, intent(in) :: ldvl
        integer, intent(in) :: ldb
        integer, intent(in) :: lda
        character(len=1) ,intent(in) :: balanc
        character(len=1) ,intent(in) :: jobvl
        character(len=1) ,intent(in) :: jobvr
        character(len=1) ,intent(in) :: sense
        integer, intent(in) :: n
        complex(kind=8) ,intent(inout) :: a(lda, *)
        complex(kind=8) ,intent(inout) :: b(ldb, *)
        complex(kind=8) ,intent(out) :: alpha(*)
        complex(kind=8) ,intent(out) :: beta(*)
        complex(kind=8) ,intent(out) :: vl(ldvl, *)
        complex(kind=8) ,intent(out) :: vr(ldvr, *)
        blas_int, intent(out) :: ilo
        blas_int, intent(out) :: ihi
        real(kind=8) ,intent(out) :: lscale(*)
        real(kind=8) ,intent(out) :: rscale(*)
        real(kind=8) ,intent(out) :: abnrm
        real(kind=8) ,intent(out) :: bbnrm
        real(kind=8) ,intent(out) :: rconde(*)
        real(kind=8) ,intent(out) :: rcondv(*)
        complex(kind=8) ,intent(out) :: work(*)
        integer, intent(in) :: lwork
        real(kind=8) ,intent(out) :: rwork(*)
        blas_int ,intent(out) :: iwork(*)
        logical(kind=4) ,intent(out) :: bwork(*)
        blas_int, intent(out) :: info
    end subroutine zggevx
end interface
