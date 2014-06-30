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
    subroutine cafves(cont, tange, maxfa, nface, fks,&
                      dfks1, dfks2, mobfa, dmob1, dmob2,&
                      mob1f, mob2f, flux, dflx1, dflx2)
        integer :: maxfa
        logical(kind=1) :: cont
        logical(kind=1) :: tange
        integer :: nface
        real(kind=8) :: fks(maxfa)
        real(kind=8) :: dfks1(maxfa+1, maxfa)
        real(kind=8) :: dfks2(maxfa+1, maxfa)
        real(kind=8) :: mobfa(maxfa)
        real(kind=8) :: dmob1(maxfa)
        real(kind=8) :: dmob2(maxfa)
        real(kind=8) :: mob1f(maxfa)
        real(kind=8) :: mob2f(maxfa)
        real(kind=8) :: flux
        real(kind=8) :: dflx1(maxfa+1)
        real(kind=8) :: dflx2(maxfa+1)
    end subroutine cafves
end interface
