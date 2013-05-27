subroutine gloloc(xgloba, xorig, sina, cosa, sinb,&
                  cosb, sing, cosg, xlocal)
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
!***********************************************************************
! 01/01/91    G.JACQUART AMV/P61 47 65 49 41
!***********************************************************************
!     FONCTION  : PASSAGE REPERE GLOBAL AU REPERE LOCAL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
!        NOM        MODE                    ROLE
!  ________________ ____ ______________________________________________
!                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
!  ________________ ____ ______________________________________________
!    XGLOBA         <--   COORDONEES DANS LE REPERE GLOBAL
!    XORIG          <--   ORIGINE DU REPERE LOCAL (COORD GLOBALES)
!    SINA,SINB,SING <--   SINUS DES ANGLES DE ROTATION REP. GLOBAL LOC.
!    COSA,COSB,COSG <--   COSINUS DES ANGLES DE ROTATION REP. GLOB LOC.
!    XLOCAL          -->  COORDONNES DANS LE REPERE LOCAL
!-----------------------------------------------------------------------
    implicit none
    include 'asterfort/rot3d.h'
    real(kind=8) :: xgloba(3), xorig(3), xlocal(3), xpiv(3)
!
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: cosa, cosb, cosg, sina, sinb, sing
!-----------------------------------------------------------------------
    do 10 i = 1, 3
        xpiv(i)=xgloba(i)-xorig(i)
10  end do
    call rot3d(xpiv, sina, cosa, sinb, cosb,&
               sing, cosg, xlocal)
end subroutine
