subroutine calfmn(np1, nbm, testc, fmod0, fmod00,&
                  cmod, kmod, vitg0, depg0)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! DESCRIPTION : CALCUL DE LA FORCE MODALE A L'INSTANT N
! -----------
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/prmave.h"
#include "asterfort/utmess.h"
    integer :: np1, nbm, testc
    real(kind=8) :: fmod0(*), fmod00(*), cmod(np1, *), kmod(np1, *), vitg0(*)
    real(kind=8) :: depg0(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ier
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  PRMAVE
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    if (testc .eq. 0) then
!
        do 10 i = 1, nbm
            fmod0(i) = fmod00(i)
10      continue
!
    else
!
        ier = 0
        call prmave(0, kmod, np1, nbm, nbm,&
                    depg0, nbm, fmod0, nbm, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH_71')
        endif
!
        ier = 0
        call prmave(1, cmod, np1, nbm, nbm,&
                    vitg0, nbm, fmod0, nbm, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH_71')
        endif
!
        do 20 i = 1, nbm
            fmod0(i) = fmod00(i) - fmod0(i)
20      continue
!
    endif
!
! --- FIN DE CALFMN.
end subroutine
