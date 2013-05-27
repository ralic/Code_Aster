subroutine i3crqp(epsi, seuil, s, x, y,&
                  cr, iret)
    implicit none
!
    include 'asterfort/i3afk2.h'
    include 'asterfort/i3dch2.h'
    include 'asterfort/i3nwt2.h'
    integer :: iret
    real(kind=8) :: epsi, seuil, s(3, *), x, y, cr(*)
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
!     QUADRANGLE PLAN DE SOMMETS S
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  SEUIL  : R : SEUIL
! IN  X,Y    : R : COORDONNEES DU POINT
! IN  S      : R : TABLE(1..3,1..4) DES COORDONNEES DES SOMMETS
! OUT CR     : R : TABLE(1..2)      DES COORDONNEES DE REF DE (X,Y)
! OUT IRET   : I : CODE RETOUR : 0 --> CONVERGENCE
!                                1 --> ARRET A NB_MAX ITERATIONS
!                               -1 --> FACE DEGENEREE
!     ------------------------------------------------------------------
!
    real(kind=8) :: fk(4, 3), m(2), r1(2), r2(2), a, b, c, d, zero, un
    integer :: maxitr
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    iret = 0
    maxitr = 10
    zero = 0.0d0
    un = 1.0d0
    call i3afk2(s, fk, iret)
    a = fk(1,1)
    b = fk(2,1)
    c = fk(3,1)
    d = fk(4,1)
    a = max(abs(a),abs(b),abs(c),abs(d))
    if (a .eq. zero) then
        iret = -1
    else
        a = un/a
        fk(1,1) = fk(1,1)*a
        fk(2,1) = fk(2,1)*a
        fk(3,1) = fk(3,1)*a
        fk(4,1) = fk(4,1)*a
        m (1) = x*a
    endif
    a = fk(1,2)
    b = fk(2,2)
    c = fk(3,2)
    d = fk(4,2)
    a = max(abs(a),abs(b),abs(c),abs(d))
    if (a .eq. zero) then
        iret = -1
    else
        a = un/a
        fk(1,2) = fk(1,2)*a
        fk(2,2) = fk(2,2)*a
        fk(3,2) = fk(3,2)*a
        fk(4,2) = fk(4,2)*a
        m (2) = y*a
    endif
    fk(1,3) = zero
    fk(2,3) = zero
    fk(3,3) = zero
    fk(4,3) = zero
    if (iret .ne. -1) then
        call i3dch2(epsi, seuil, maxitr, fk, m,&
                    r1, r2, iret)
        cr(1) = 0.5d0*(r1(1) + r1(2))
        cr(2) = 0.5d0*(r2(1) + r2(2))
        if (iret .gt. 0) then
            fk(1,1) = fk(1,1) - m(1)
            fk(1,2) = fk(1,2) - m(2)
            call i3nwt2(epsi, epsi, maxitr, fk, cr,&
                        iret)
        endif
    endif
end subroutine
