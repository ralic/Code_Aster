subroutine shvrot(rr, x, nn)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
    implicit none
    include 'asterfort/assert.h'
    real(kind=8) :: rr(3, 3)
    integer :: nn
    real(kind=8) :: x(24)
! ---   VARIABLES LOCALES
    real(kind=8) :: aa(24), r(3, 3)
    integer :: i, j
!
!
    call assert((nn.eq.1).or.(nn.eq.2))
    if (nn .eq. 2) then
        do 10 i = 1, 3
            do 20 j = 1, 3
                r(i,j)= rr(j,i)
20          continue
10      continue
    else if (nn.eq.1) then
        do 30 i = 1, 3
            do 40 j = 1, 3
                r(i,j)= rr(i,j)
40          continue
30      continue
    endif
!
    aa(1) = r(1,1)*x(1) + r(1,2)*x(2) + r(1,3)*x(3)
    aa(2) = r(2,1)*x(1) + r(2,2)*x(2) + r(2,3)*x(3)
    aa(3) = r(3,1)*x(1) + r(3,2)*x(2) + r(3,3)*x(3)
!
    aa(4) = r(1,1)*x(4) + r(1,2)*x(5) + r(1,3)*x(6)
    aa(5) = r(2,1)*x(4) + r(2,2)*x(5) + r(2,3)*x(6)
    aa(6) = r(3,1)*x(4) + r(3,2)*x(5) + r(3,3)*x(6)
!
    aa(7) = r(1,1)*x(7) + r(1,2)*x(8) + r(1,3)*x(9)
    aa(8) = r(2,1)*x(7) + r(2,2)*x(8) + r(2,3)*x(9)
    aa(9) = r(3,1)*x(7) + r(3,2)*x(8) + r(3,3)*x(9)
!
    aa(10) = r(1,1)*x(10) + r(1,2)*x(11) + r(1,3)*x(12)
    aa(11) = r(2,1)*x(10) + r(2,2)*x(11) + r(2,3)*x(12)
    aa(12) = r(3,1)*x(10) + r(3,2)*x(11) + r(3,3)*x(12)
!
    aa(13) = r(1,1)*x(13) + r(1,2)*x(14) + r(1,3)*x(15)
    aa(14) = r(2,1)*x(13) + r(2,2)*x(14) + r(2,3)*x(15)
    aa(15) = r(3,1)*x(13) + r(3,2)*x(14) + r(3,3)*x(15)
!
    aa(16) = r(1,1)*x(16) + r(1,2)*x(17) + r(1,3)*x(18)
    aa(17) = r(2,1)*x(16) + r(2,2)*x(17) + r(2,3)*x(18)
    aa(18) = r(3,1)*x(16) + r(3,2)*x(17) + r(3,3)*x(18)
!
    aa(19) = r(1,1)*x(19) + r(1,2)*x(20) + r(1,3)*x(21)
    aa(20) = r(2,1)*x(19) + r(2,2)*x(20) + r(2,3)*x(21)
    aa(21) = r(3,1)*x(19) + r(3,2)*x(20) + r(3,3)*x(21)
!
    aa(22) = r(1,1)*x(22) + r(1,2)*x(23) + r(1,3)*x(24)
    aa(23) = r(2,1)*x(22) + r(2,2)*x(23) + r(2,3)*x(24)
    aa(24) = r(3,1)*x(22) + r(3,2)*x(23) + r(3,3)*x(24)
!
    do 50 i = 1, 24
        x(i)= aa(i)
50  continue
!
!
end subroutine
