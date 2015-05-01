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
    subroutine impact(nmtab, nbpt, fn, vn, wk3,&
                      offset, t, elapse, nbchoc, fnmaxa,&
                      fnmmoy, fnmety, npari, lpari, valek)
        character(len=*) :: nmtab
        integer :: nbpt
        real(kind=8) :: fn(*)
        real(kind=8) :: vn(*)
        real(kind=8) :: wk3(*)
        real(kind=8) :: offset
        real(kind=8) :: t(*)
        real(kind=8) :: elapse
        integer :: nbchoc
        real(kind=8) :: fnmaxa
        real(kind=8) :: fnmmoy
        real(kind=8) :: fnmety
        integer :: npari
        character(len=16) :: lpari(*)
        character(len=16) :: valek(*)
    end subroutine impact
end interface
