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
interface
    subroutine dgesvx(fact, trans, n, nrhs, a,&
                      lda, af, ldaf, ipiv, equed,&
                      r, c, b, ldb, x,&
                      ldx, rcond, ferr, berr, work,&
                      iwork, info)
        integer :: ldx
        integer :: ldb
        integer :: ldaf
        integer :: lda
        character(len=1) :: fact
        character(len=1) :: trans
        integer :: n
        integer :: nrhs
        real(kind=8) :: a(lda, *)
        real(kind=8) :: af(ldaf, *)
        integer :: ipiv(*)
        character(len=1) :: equed
        real(kind=8) :: r(*)
        real(kind=8) :: c(*)
        real(kind=8) :: b(ldb, *)
        real(kind=8) :: x(ldx, *)
        real(kind=8) :: rcond
        real(kind=8) :: ferr(*)
        real(kind=8) :: berr(*)
        real(kind=8) :: work(*)
        integer :: iwork(*)
        integer :: info
    end subroutine dgesvx
end interface
