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
    subroutine gddyng(kp, nno, en, x0sk, rmkm1,&
                      rmk, omkm1, ompkm1, omk, ompk,&
                      x0sec, rgmkm, rgmk, omgkm, ompgkm,&
                      omgk, ompgk)
        integer :: kp
        integer :: nno
        real(kind=8) :: en(3, 2)
        real(kind=8) :: x0sk(3, 3)
        real(kind=8) :: rmkm1(3, 3)
        real(kind=8) :: rmk(3, 3)
        real(kind=8) :: omkm1(3, 3)
        real(kind=8) :: ompkm1(3, 3)
        real(kind=8) :: omk(3, 3)
        real(kind=8) :: ompk(3, 3)
        real(kind=8) :: x0sec(3)
        real(kind=8) :: rgmkm(3)
        real(kind=8) :: rgmk(3)
        real(kind=8) :: omgkm(3)
        real(kind=8) :: ompgkm(3)
        real(kind=8) :: omgk(3)
        real(kind=8) :: ompgk(3)
    end subroutine gddyng
end interface
