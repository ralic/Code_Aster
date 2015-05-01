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
!
interface
    subroutine dneupd(rvec, howmny, select, dr, di,&
                      z, ldz, sigmar, sigmai, workev,&
                      bmat, n, which, nev, tol,&
                      resid, ncv, v, ldv, iparam,&
                      ipntr, workd, workl, lworkl, info)
        integer :: lworkl
        integer :: ldv
        integer :: ncv
        integer :: nev
        integer :: n
        integer :: ldz
        aster_logical :: rvec
        character(len=1) :: howmny
        aster_logical :: select(ncv)
        real(kind=8) :: dr(nev+1)
        real(kind=8) :: di(nev+1)
        real(kind=8) :: z(ldz, *)
        real(kind=8) :: sigmar
        real(kind=8) :: sigmai
        real(kind=8) :: workev(3*ncv)
        character(len=1) :: bmat
        character(len=2) :: which
        real(kind=8) :: tol
        real(kind=8) :: resid(n)
        real(kind=8) :: v(ldv, ncv)
        integer :: iparam(11)
        integer :: ipntr(14)
        real(kind=8) :: workd(3*n)
        real(kind=8) :: workl(lworkl)
        integer :: info
    end subroutine dneupd
end interface
