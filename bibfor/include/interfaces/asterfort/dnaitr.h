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
    subroutine dnaitr(ido, bmat, n, k, np,&
                      resid, rnorm, v, ldv, h,&
                      ldh, ipntr, workd, info, alpha)
        integer :: ldh
        integer :: ldv
        integer :: np
        integer :: k
        integer :: n
        integer :: ido
        character(len=1) :: bmat
        real(kind=8) :: resid(n)
        real(kind=8) :: rnorm
        real(kind=8) :: v(ldv, k+np)
        real(kind=8) :: h(ldh, k+np)
        integer :: ipntr(3)
        real(kind=8) :: workd(3*n)
        integer :: info
        real(kind=8) :: alpha
    end subroutine dnaitr
end interface
