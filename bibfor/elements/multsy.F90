subroutine multsy(u22, a3, v22, msym)
!
    implicit  none
!
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    include 'asterfort/matmul.h'
    real(kind=8) :: msym(*)
!
    real(kind=8) :: u22(2, *), v22(2, *), a3(*), a22(2, 2), msym22(2, 2)
!
    real(kind=8) :: cp(2, 2)
! Construction of the symetric matrix A
    a22(1,1) = a3(1)
    a22(2,2) = a3(2)
    a22(1,2) = a3(3)
    a22(2,1) = a3(3)
!
! Multiplication: MSYM = U22 * A * V22
    call matmul(a22, v22, 2, 2, 2,&
                cp)
    call matmul(u22, cp, 2, 2, 2,&
                msym22)
! Construction of the rank-one result matrix
    msym(1) = msym22(1,1)
    msym(2) = msym22(2,2)
    msym(3) = msym22(1,2)
!
end subroutine
