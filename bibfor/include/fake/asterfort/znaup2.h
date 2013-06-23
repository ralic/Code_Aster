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
    subroutine znaup2(ido, bmat, n, which, nev,&
                      np, tol, resid, ishift, mxiter,&
                      v, ldv, h, ldh, ritz,&
                      bounds, q, ldq, workl, ipntr,&
                      workd, rwork, info, neqact, alpha)
        integer :: ldq
        integer :: ldh
        integer :: ldv
        integer :: np
        integer :: nev
        integer :: n
        integer :: ido
        character(len=1) :: bmat
        character(len=2) :: which
        real(kind=8) :: tol
        complex(kind=8) :: resid(n)
        integer :: ishift
        integer :: mxiter
        complex(kind=8) :: v(ldv, nev+np)
        complex(kind=8) :: h(ldh, nev+np)
        complex(kind=8) :: ritz(nev+np)
        complex(kind=8) :: bounds(nev+np)
        complex(kind=8) :: q(ldq, nev+np)
        complex(kind=8) :: workl((nev+np)*(nev+np+3))
        integer :: ipntr(13)
        complex(kind=8) :: workd(3*n)
        real(kind=8) :: rwork(nev+np)
        integer :: info
        integer :: neqact
        real(kind=8) :: alpha
    end subroutine znaup2
end interface
