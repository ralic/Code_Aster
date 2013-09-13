subroutine defttr(np1, np4, nbm, npf, nttr,&
                  ntrans, ttran0, ttrans, text, fext,&
                  fextt0, fexttr, dttr)
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
! DESCRIPTION : RECUPERATION DES EFFORTS EXTERIEURS GENERALISES
! -----------   POUR PASSAGE DU TRANSITOIRE
!
!               APPELANTS : MDITM2, TRANSI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
    integer :: np1, np4, nbm, npf, nttr, ntrans
    real(kind=8) :: ttran0, ttrans, text(*), fext(np4, *), fextt0(*), fexttr(*)
    real(kind=8) :: dttr
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: dt, past
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  DBLE, INT
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   LCINVN
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
! 1.  PREMIERE ESTIMATION DE LA DUREE DU TRANSITOIRE
!-----------------------------------------------------------------------
!
    if (ntrans .eq. 0) then
!
        call vecini(np1, 0.d0, fextt0)
        call vecini(np1, 0.d0, fexttr)
        nttr = int(2.0d0**dble(npf))
        do 10 i = 1, nbm
            fextt0(i) = fext(1,i)
10      continue
        do 20 i = 1, nbm
            fexttr(i) = fext(nttr,i)
20      continue
        ttran0 = text(1)
        ttrans = text(nttr)
!
!-----------------------------------------------------------------------
! 2.  ESTIMATIONS SUIVANTES
!-----------------------------------------------------------------------
!
    else
!
        do 30 i = 1, nbm
            fextt0(i) = fexttr(i)
30      continue
        ttran0 = ttrans
!
        ttrans = ttrans + dttr
40      continue
        if (ttrans .gt. text(nttr)) then
            nttr = nttr + 1
            if (nttr .gt. np4) then
                call utmess('F', 'ALGORITH2_66')
            endif
            goto 40
        endif
!
        past = text(nttr) - text(nttr-1)
        dt = ttrans - text(nttr-1)
        do 50 i = 1, nbm
            fexttr(i) = fext(nttr-1,i) + dt * ( fext(nttr,i) - fext( nttr-1,i) ) / past
50      continue
!
    endif
!
! --- FIN DE DEFTTR.
end subroutine
