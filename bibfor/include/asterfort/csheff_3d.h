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
    subroutine csheff_3d(dcash, dcsheff, dalpha, sic, csh,&
                         alsol, dalsol, csheff, xidtot, xidtot1,&
                         nasol, vnasol, dt, alpha, cash,&
                         alc, sc, id0, id1, id2)
        real(kind=8) :: dcash
        real(kind=8) :: dcsheff
        real(kind=8) :: dalpha
        real(kind=8) :: sic
        real(kind=8) :: csh
        real(kind=8) :: alsol
        real(kind=8) :: dalsol
        real(kind=8) :: csheff
        real(kind=8) :: xidtot
        real(kind=8) :: xidtot1
        real(kind=8) :: vnasol
        real(kind=8) :: nasol
        real(kind=8) :: dt
        real(kind=8) :: alpha
        real(kind=8) :: cash
        real(kind=8) :: alc
        real(kind=8) :: sc
        real(kind=8) :: id0
        real(kind=8) :: id1
        real(kind=8) :: id2
    end subroutine csheff_3d
end interface 
