subroutine i3sl33(epsi, a, b, x, typ)
    implicit none
!
    character(len=4) :: typ
    real(kind=8) :: epsi, a(3, *), b(*), x(*)
!
!     ------------------------------------------------------------------
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
!     RESOLUTION D' UN SL 3X3 POUR
!                  INTERSECTION SEGMENT-TRIANGLE
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! VAR A      : R : IN : MATRICE ; OUT : SANS OBJET
! VAR B      : R : IN : SECOND MEMBRE ; OUT : SANS OBJET
! OUT X      : R : INCONNUES
! OUT TYP    : K : 'DETE'/'INCO'/'INDE'
!     ------------------------------------------------------------------
!     RESTRICTION : SL DE RANG MINIMAL 1
!     ------------------------------------------------------------------
!
    real(kind=8) :: a11, a12, a21, a22, a31, a32, b1, b2, b3, a13, a23, a33
    real(kind=8) :: da12, da13, da23, db1, db3, db2, da, db, un
!
!=======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    un = 1.0d0
    a11 = a(1,1)
    a12 = a(1,2)
    a13 = a(1,3)
    b1 = b(1)
    a21 = a(2,1)
    a22 = a(2,2)
    a23 = a(2,3)
    b2 = b(2)
    a31 = a(3,1)
    a32 = a(3,2)
    a33 = a(3,3)
    b3 = b(3)
    da12 = a11*a22 - a12*a21
    da13 = a11*a32 - a12*a31
    da23 = a21*a32 - a22*a31
    da = a33*da12 - a23*da13 + a13*da23
    db3 = b3*da12 - b2*da13 + b1*da23
    da12 = a21*a13 - a11*a23
    da13 = a13*a31 - a11*a33
    da23 = a23*a31 - a21*a33
    db2 = b3*da12 - b2*da13 + b1*da23
    da12 = a12*a23 - a22*a13
    da13 = a12*a33 - a13*a32
    da23 = a22*a33 - a32*a23
    db1 = b3*da12 - b2*da13 + b1*da23
    if (abs(da) .gt. epsi) then
        typ = 'DETE'
        da = un/da
        x(1) = db1*da
        x(2) = db2*da
        x(3) = db3*da
    else
        db = db1*db1 + db2*db2 + db3*db3
        if (abs(db) .gt. epsi) then
            typ = 'INCO'
        else
            typ = 'INDE'
        endif
    endif
end subroutine
