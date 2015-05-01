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
interface
    subroutine intfac(noma, nmaabs, ifq, fa, nno,&
                      lst, lsn, ndim, grad, jglsn,&
                      jglst, igeom, m, indptf, gln,&
                      glt, codret)
        integer :: ndim
        integer :: nno
        character(len=8) :: noma
        integer :: nmaabs
        integer :: ifq
        integer :: fa(6, 8)
        real(kind=8) :: lst(nno)
        real(kind=8) :: lsn(nno)
        character(len=3) :: grad
        integer :: jglsn
        integer :: jglst
        integer :: igeom
        real(kind=8) :: m(ndim)
        integer :: indptf(3)
        real(kind=8) :: gln(ndim)
        real(kind=8) :: glt(ndim)
        integer :: codret
    end subroutine intfac
end interface
