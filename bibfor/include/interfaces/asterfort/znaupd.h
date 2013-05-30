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
    subroutine znaupd(ido, bmat, n, which, nev,&
                      tol, resid, ncv, v, ldv,&
                      iparam, ipntr, workd, workl, lworkl,&
                      rwork, info, neqact, alpha)
        integer :: lworkl
        integer :: ldv
        integer :: ncv
        integer :: n
        integer :: ido
        character(len=1) :: bmat
        character(len=2) :: which
        integer :: nev
        real(kind=8) :: tol
        complex(kind=8) :: resid(n)
        complex(kind=8) :: v(ldv, ncv)
        integer :: iparam(11)
        integer :: ipntr(14)
        complex(kind=8) :: workd(3*n)
        complex(kind=8) :: workl(lworkl)
        real(kind=8) :: rwork(ncv)
        integer :: info
        integer :: neqact
        real(kind=8) :: alpha
    end subroutine znaupd
end interface
