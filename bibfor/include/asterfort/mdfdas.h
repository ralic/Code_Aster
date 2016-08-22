!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine mdfdas(dnorm, vnorm, vitloc, cost, sint,&
                      coefk1, coefk2, coefpy, coefc, coefad,&
                      xmax, fdispo, flocal)
        real(kind=8) :: dnorm
        real(kind=8) :: vnorm
        real(kind=8) :: vitloc(3)
        real(kind=8) :: cost
        real(kind=8) :: sint
        real(kind=8) :: coefk1
        real(kind=8) :: coefk2
        real(kind=8) :: coefpy
        real(kind=8) :: coefc
        real(kind=8) :: coefad
        real(kind=8) :: xmax
        real(kind=8) :: fdispo
        real(kind=8) :: flocal(3)
    end subroutine mdfdas
end interface
