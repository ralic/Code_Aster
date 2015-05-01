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
    subroutine i2chax(x1, y1, x2, y2, x3,&
                      y3, xm, ym, xn, yn,&
                      c, xnm, ynm, xnn, ynn)
        real(kind=8) :: x1
        real(kind=8) :: y1
        real(kind=8) :: x2
        real(kind=8) :: y2
        real(kind=8) :: x3
        real(kind=8) :: y3
        real(kind=8) :: xm
        real(kind=8) :: ym
        real(kind=8) :: xn
        real(kind=8) :: yn
        real(kind=8) :: c
        real(kind=8) :: xnm
        real(kind=8) :: ynm
        real(kind=8) :: xnn
        real(kind=8) :: ynn
    end subroutine i2chax
end interface
