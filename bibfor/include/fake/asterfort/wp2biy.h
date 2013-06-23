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
    subroutine wp2biy(lm, lc, lk, s2, dsr,&
                      isi, yh, yb, zh, zb,&
                      lbloq, u1, u2, u3, u4,&
                      n)
        integer :: lm
        integer :: lc
        integer :: lk
        real(kind=8) :: s2
        real(kind=8) :: dsr
        real(kind=8) :: isi
        real(kind=8) :: yh(*)
        real(kind=8) :: yb(*)
        real(kind=8) :: zh(*)
        real(kind=8) :: zb(*)
        integer :: lbloq(*)
        real(kind=8) :: u1(*)
        real(kind=8) :: u2(*)
        real(kind=8) :: u3(*)
        real(kind=8) :: u4(*)
        integer :: n
    end subroutine wp2biy
end interface
