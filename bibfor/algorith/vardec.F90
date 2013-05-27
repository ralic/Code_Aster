subroutine vardec(xloc, xloc0, ivar, dt0, tole)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : TEST SUR LA VARIATION DES COORDONNEES LOCALES ENTRE
! -----------   LES INSTANTS N ET N+1
!
!               APPELANT : TESTCH
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    real(kind=8) :: xloc(*), xloc0(*)
    integer :: ivar
    real(kind=8) :: dt0, tole
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: temp
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    ivar = 0
!
    if (dt0 .eq. 0.0d0) goto 999
!
!  1. TEST PORTANT SUR LA PREMIERE COORDONNEE LOCALE
!     ----------------------------------------------
    if (xloc(1) .ne. 0.0d0) then
        temp = abs(xloc(1) - xloc0(1))
        if (temp .gt. tole) then
            ivar = 1
            goto 999
        endif
    endif
!
!  2. TEST PORTANT SUR LA DEUXIEME COORDONNEE LOCALE
!     ----------------------------------------------
    if (xloc(2) .ne. 0.0d0) then
        temp = abs(xloc(2) - xloc0(2))
        if (temp .gt. tole) then
            ivar = 1
            goto 999
        endif
    endif
!
!  3. TEST PORTANT SUR LA TROISIEME COORDONNEE LOCALE
!     -----------------------------------------------
    if (xloc(3) .ne. 0.0d0) then
        temp = abs(xloc(3) - xloc0(3))
        if (temp .gt. tole) then
            ivar = 1
        endif
    endif
!
999  continue
!
! --- FIN DE VARDEC.
end subroutine
