subroutine rvrthe(x, y, t1, t2, n1,&
                  n2)
    implicit none
!
    real(kind=8) :: x, y, t1, t2, n1, n2
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    real(kind=8) :: r
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    r = sqrt(x*x + y*y)
!
    if (abs(r) .lt. 1.0d-6) then
!
        t1 = 1.0d0
        t2 = 0.0d0
        n2 = 1.0d0
        n1 = 0.0d0
!
    else
!
        t1 = 1.0d0/r
        t2 = t1*y
        t1 = t1*x
        n1 = -t2
        n2 = t1
!
    endif
!
end subroutine
