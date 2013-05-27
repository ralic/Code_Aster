subroutine i3sl32(epsi, a, b, x, typ)
    implicit none
!
    character(len=4) :: typ
    real(kind=8) :: epsi, a(3, *), b(*), x(*)
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
!     RESOLUTION D' UN SL 3X2 POUR
!                  INTERSECTION DE 2 DROITES 3D
!                  CALCUL DES COORDONEES DE REF DANS UN TRIANGLE
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! VAR A      : R : IN : MATRICE ; OUT : SANS OBJET
! VAR B      : R : IN : SECOND MEMBRE ; OUT : SANS OBJET
! OUT X      : R : SOLUTION DANS LE CAS 'DETE' OU VIDE SINON
! OUT TYP    : K : 'DETE'/'INCO'/'INDE'
!     ------------------------------------------------------------------
!     RESTRICTION : SL DE RANG MINIMAL 1
!     ------------------------------------------------------------------
!
    real(kind=8) :: a11, a12, a21, a22, a31, a32, b1, b2, b3
    real(kind=8) :: da12, da13, da23, db12, db13, db23, db, un
!
!=======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    un = 1.0d0
    a11 = a(1,1)
    a12 = a(1,2)
    a21 = a(2,1)
    a22 = a(2,2)
    a31 = a(3,1)
    a32 = a(3,2)
    b1 = b(1)
    b2 = b(2)
    b3 = b(3)
    da12 = a11*a22 - a12*a21
    da13 = a11*a32 - a12*a31
    da23 = a21*a32 - a22*a31
    db12 = a12*b2 - a22*b1
    db13 = a12*b3 - a32*b1
    db23 = a22*b3 - a32*b2
    if (abs(da12*da12+da13*da13+da23*da23) .le. epsi) then
        if (abs(db12*db12+db13*db13+db23*db23) .le. epsi) then
            typ = 'INDE'
        else
            typ = 'INCO'
        endif
    else
        db = b1*da23 - b2*da13 + b3*da12
        if (abs(db) .gt. epsi) then
            typ = 'INCO'
        else
            typ = 'DETE'
        endif
    endif
    if (typ .eq. 'DETE') then
        if (abs(da12) .ge. max(abs(da13),abs(da23))) then
            da12 = un/da12
            x(1) = (b1*a22 - b2*a12)*da12
            x(2) = (b2*a11 - b1*a21)*da12
        else if (abs(da13) .ge. max(abs(da12),abs(da23))) then
            da13 = un/da13
            x(1) = (b1*a32 - b3*a12)*da13
            x(2) = (b3*a11 - b1*a31)*da13
        else
            da23 = un/da23
            x(1) = (b2*a32 - b3*a22)*da23
            x(2) = (b3*a21 - b2*a31)*da23
        endif
    endif
end subroutine
