subroutine i3crtp(epsi, s, p, cr, iret)
    implicit none
!
    integer :: iret
    real(kind=8) :: epsi, s(3, *), p(*), cr(*)
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     CALCUL DES COORDONNEES DE REF DU POINT DE CORDO (X,Y) DANS LE
!     TRIANGLE DE SOMMETS S
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  P      : R : COORDONNEES DU POINT
! IN  S      : R : TABLE(1..3,1..3) DES COORDONNEES DES SOMMETS
! OUT CR     : R : TABLE(1..2)      DES COORDONNEES DE REF DE P
! OUT IRET   : I : CODE RETOUR : 0 --> PAS DE PB
!                                1 --> DEGENERESCENCE
!     ------------------------------------------------------------------
!
    real(kind=8) :: a(3, 2), b(3), c, d, zero, un
    integer :: i, j
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.0d0
    un = 1.0d0
    iret = 0
    do 10, i = 1, 3, 1
    d = s(i,1)
    do 11, j = 1, 2, 1
    a(i,j) = s(i,j+1) - d
11  continue
    b(i) = p(i) - d
    c = max(abs(a(i,1)),abs(a(i,2)))
    if (c .gt. epsi*d) then
        c = un/c
        a(i,1) = a(i,1)*c
        a(i,2) = a(i,2)*c
        b(i) = b(i) *c
    endif
    10 end do
    d = a(1,1)*a(2,2) - a(1,2)*a(2,1)
    if (abs(d) .gt. epsi) then
        d = un/d
        cr(1) = (b(1)*a(2,2) - b(2)*a(1,2))*d
        cr(2) = (b(2)*a(1,1) - b(1)*a(2,1))*d
    else
        d = a(1,1)*a(3,2) - a(1,2)*a(3,1)
        if (abs(d) .gt. epsi) then
            d = un/d
            cr(1) = (b(1)*a(3,2) - b(3)*a(1,2))*d
            cr(2) = (b(3)*a(1,1) - b(1)*a(3,1))*d
        else
            d = a(2,1)*a(3,2) - a(2,2)*a(3,1)
            if (abs(d) .gt. epsi) then
                d = un/d
                cr(1) = (b(2)*a(3,2) - b(3)*a(2,2))*d
                cr(2) = (b(3)*a(2,1) - b(2)*a(3,1))*d
            else
                iret = 1
            endif
        endif
    endif
    if (iret .ne. 1) then
        do 20, i = 1, 2, 1
        d = cr(i)
        if (abs(d) .lt. epsi) then
            cr(i) = zero
        else if (abs(d-un) .lt. epsi) then
            cr(i) = un
        else
        endif
20      continue
    endif
end subroutine
