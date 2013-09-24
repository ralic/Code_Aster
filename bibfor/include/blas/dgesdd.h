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
    subroutine dgesdd(jobz, m, n, a, lda,&
                      s, u, ldu, vt, ldvt,&
                      work, lwork, iwork, info)
        integer, intent(in) :: ldvt
        integer, intent(in) :: ldu
        integer, intent(in) :: lda
        character(len=1) ,intent(in) :: jobz
        integer, intent(in) :: m
        integer, intent(in) :: n
        real(kind=8) ,intent(inout) :: a(lda, *)
        real(kind=8) ,intent(out) :: s(*)
        real(kind=8) ,intent(out) :: u(ldu, *)
        real(kind=8) ,intent(out) :: vt(ldvt, *)
        real(kind=8) ,intent(out) :: work(*)
        integer, intent(in) :: lwork
        blas_int ,intent(out) :: iwork(*)
        blas_int, intent(out) :: info
    end subroutine dgesdd
end interface
