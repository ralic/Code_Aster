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
    subroutine ftang(fn, xlocal, vitloc, cfrotd, cfrots,&
                     ktang, ctang, iadher, oldvt, oldft,&
                     oldxlo, cost, sint, ftange, flocal,&
                     vt)
        real(kind=8) :: fn
        real(kind=8) :: xlocal(3)
        real(kind=8) :: vitloc(3)
        real(kind=8) :: cfrotd
        real(kind=8) :: cfrots
        real(kind=8) :: ktang
        real(kind=8) :: ctang
        integer :: iadher
        real(kind=8) :: oldvt(2)
        real(kind=8) :: oldft(2)
        real(kind=8) :: oldxlo(3)
        real(kind=8) :: cost
        real(kind=8) :: sint
        real(kind=8) :: ftange(2)
        real(kind=8) :: flocal(3)
        real(kind=8) :: vt(2)
    end subroutine ftang
end interface
