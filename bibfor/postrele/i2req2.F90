subroutine i2req2(epsi, a, b, c, nr,&
                  r1, r2, mult1, mult2)
    implicit none
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
    integer :: nr, mult1, mult2
    real(kind=8) :: a, b, c, r1, r2, epsi
!
    real(kind=8) :: delta, aux1, aux2, cond
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nr = 0
    mult1 = 0
    mult2 = 0
    r1 = 0.0d0
    r2 = 0.0d0
!
    cond = abs(a) + abs(b) + abs(c)
    a = a/cond
    b = b/cond
    c = c/cond
    delta = b*b - 4*a*c
    aux2 = 2*a
    aux1 = 0.0d0
!
    if (abs(a) .le. epsi) then
!
        if (abs(b) .gt. epsi) then
!
            nr = 1
            r1 = -c/b
            mult1 = 1
!
        endif
!
    else
!
        if (abs(delta) .lt. epsi) then
!
            nr = 1
            r1 = -b/aux2
            mult1 = 2
!
        else if (delta .gt. 0.0d0) then
!
            nr = 2
            aux1 = sqrt(delta)
            r1 = (-b - aux1)/aux2
            r2 = (-b + aux1)/aux2
            mult1 = 1
            mult2 = 1
!
            if (r1 .gt. r2) then
!
                aux1 = r1
                r1 = r2
                r2 = aux1
!
            endif
!
        else
!
        endif
!
    endif
!
end subroutine
