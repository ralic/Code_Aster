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
    subroutine pj3dgb(ino2, geom2, geom1, tetr4, ndec,&
                      btdi, btvr, btnb, btlc, btco,&
                      p1, q1, r1, p2, q2,&
                      r2)
        integer :: ino2
        real(kind=8) :: geom2(*)
        real(kind=8) :: geom1(*)
        integer :: tetr4(*)
        integer :: ndec
        integer :: btdi(*)
        real(kind=8) :: btvr(*)
        integer :: btnb(*)
        integer :: btlc(*)
        integer :: btco(*)
        integer :: p1
        integer :: q1
        integer :: r1
        integer :: p2
        integer :: q2
        integer :: r2
    end subroutine pj3dgb
end interface
