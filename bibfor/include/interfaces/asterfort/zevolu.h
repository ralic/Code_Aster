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
    subroutine zevolu(cine, z, zm, dinst, tp,&
                      k, n, tdeq, tfeq, coeffc,&
                      m, ar, br, g, dg)
        integer :: cine
        real(kind=8) :: z
        real(kind=8) :: zm
        real(kind=8) :: dinst
        real(kind=8) :: tp
        real(kind=8) :: k
        real(kind=8) :: n
        real(kind=8) :: tdeq
        real(kind=8) :: tfeq
        real(kind=8) :: coeffc
        real(kind=8) :: m
        real(kind=8) :: ar
        real(kind=8) :: br
        real(kind=8) :: g
        real(kind=8) :: dg
    end subroutine zevolu
end interface
