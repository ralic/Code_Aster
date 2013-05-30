subroutine tstpar(itest, nbm, amor, amor0, puls,&
                  puls0, dt, dt0)
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
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : TEST SUR L'EVOLUTION DES PARAMETRES AMORTISSEMENT,
! -----------   PULSATION ET PAS DE TEMPS ENTRE LES INSTANTS N ET N+1
!
!               APPELANT : CALCMI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/u2mess.h'
    integer :: itest, nbm
    real(kind=8) :: amor(*), amor0(*), puls(*), puls0(*), dt, dt0
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: temp, tol1, tol2, tol3
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    if (dt0 .eq. 0.0d0) then
        itest = 0
        goto 999
    endif
!
    itest = 1
    tol1 = 1.0d-05
    tol2 = 1.0d-05
    tol3 = 1.0d-05
!
!  1. TESTS PORTANT SUR LES AMORTISSEMENTS
!     ------------------------------------
!
    do 10 i = 1, nbm
        if (amor(i) .eq. 0.0d0) then
            call u2mess('I', 'ALGORITH11_3')
            temp = abs((amor(i) - amor0(i)) / amor0(i))
        else
            temp = abs((amor(i) - amor0(i)) / amor(i))
        endif
        if (temp .gt. tol1) then
            itest = 0
            goto 999
        endif
10  end do
!
!  2. TESTS PORTANT SUR LES PULSATIONS
!     --------------------------------
!
    do 20 i = 1, nbm
        temp = abs((puls(i) - puls0(i)) / puls(i))
        if (temp .gt. tol2) then
            itest = 0
            goto 999
        endif
20  end do
!
!  3. TEST PORTANT SUR LE PAS DE TEMPS
!     --------------------------------
!
    temp = abs((dt - dt0)/dt)
    if (temp .gt. tol3) itest = 0
!
999  continue
!
! --- FIN DE TSTPAR.
end subroutine
