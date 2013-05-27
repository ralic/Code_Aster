subroutine i3ptrv(epsi, lstpt, nbpt, t, trouve,&
                  ipos)
    implicit none
!
    include 'jeveux.h'
    integer :: nbpt, lstpt(*), ipos
    real(kind=8) :: epsi, t
    logical :: trouve
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
!     TEST DE PRESENCE POINT DANS OBJ LISTE_POINT
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  LSTPT  : I : OBJ LISTE_POINT
! IN  NBPT   : I : NOMBRE DE POINT TCONTENU DANS LSTPT
! IN  T      : R : VALEUR A CHERCHEE DANS LE CHAMP ABSC
! OUT TROUVE : L : REPONSE
! OUT IPOS   : L : POSITION
!     ------------------------------------------------------------------
!     STRUCT LISTE_POINT
!             ( REEL          ABSC      (NMAXPT)
!               ENTIER        FACE      (NMAXPT)
!               ENTIER        ARETE     (NMAXPT)
!              (PLANE,GAUCHE) TYPE_FACE (NMAXPT)
!               REEL          COORDO_REF(NMAXPT)
!               ENTIER        ORDRE     (NMAXPT)
!             );
!     STRUCT LISTE_POINT LSTPT;
!     ------------------------------------------------------------------
!
!
!
    integer :: i
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    i = 0
    trouve = .false.
40  continue
    if ((i .lt. nbpt) .and. (.not. trouve)) then
        i = i+1
        trouve = ( abs(t-zr(lstpt(1)-1+i)) .le. epsi )
        goto 40
    endif
    ipos = i
end subroutine
