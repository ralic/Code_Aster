subroutine draac2(a, b, c, x1, x2,&
                  kode)
!
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!======================================================================
!
!     EVALUE LES RACINES DU POLYNOME DU SECOND DEGRE Y=A X**2 + B X + C
!
! IN  A : COEFFICIENT DU POLYNOME
! IN  B : COEFFICIENT DU POLYNOME
! IN  C : COEFFICIENT DU POLYNOME
!
! OUT X1 : RACINE DU POLYNOME
! OUT X2 : RACINE DU POLYNOME
! OUT KODE : NOMBRE DE RACINES
!          = 0 : PAS DE RACINE REELLE
!          = 1 : UNE RACINE REELLE
!          = 2 : DEUX RACINES REELLES
!
    integer :: kode
!
    real(kind=8) :: a, b, c, x1, x2
    real(kind=8) :: delta, epsi, x0, deuza, asup
!
    x1 = 0.0d0
    x2 = 0.0d0
    kode = 0
    delta = 0.0d0
    epsi = 1.0d-8 * max(abs(a),abs(b),abs(c))
    x0 = 0.0d0
    deuza = 0.0d0
    asup = 0.0d0
!
    if (abs(a) .gt. epsi) then
        x1 = b * b
        x2 = 4.0d0 * a * c
        delta = x1 - x2
        asup = 1.0d-8 * max(x1,abs(x2))
        deuza = 2.0d0 * a
        x0 = -b / deuza
!
        if (delta .lt. -asup) then
!     CAS OU ON A DEUX RACINES IMAGINAIRES
            kode = 0
            x1 = 0.0d0
            x2 = 0.0d0
        else if (delta.lt.asup) then
!     CAS OU ON A UNE RACINE REELE DOUBLE
            kode = 1
            x1 = x0
            x2 = x0
        else
!     CAS OU ON A DEUX RACINES
            kode = 2
            x2 = sqrt(delta)/abs(deuza)
            x1 = x0 - x2
            x2 = x0 + x2
        endif
    else if (abs(b).le.epsi) then
!     CAS OU LE POLYNOME DU SECOND DEGRE
!     S APPARENTE A UNE CONSTANTE
        kode = 0
        x1 = 0.0d0
        x2 = 0.0d0
    else
!     CAS OU LE POLYNOME DU SECOND DEGRE
!     S APPARENTE A UN POLYNOME DU PREMIER DEGRE
        kode = 1
        x1 = - c / b
        x2 = x1
    endif
!
end subroutine
