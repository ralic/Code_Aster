function freqom(omega)
    implicit none
#include "asterc/r8depi.h"
    real(kind=8) :: freqom, omega
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
!     CALCULE LA FREQUENCE FREQOM ASSOCIEE A LA PULSATION OMEGA
!              FREQUENCE = SQRT(OMEGA) / (2*PI)
!     ------------------------------------------------------------------
!     REMARQUE : SI LA PULSATION D'ENTREE EST NEGATIVE
!                   ALORS ON RETOURNE  (-FREQOM)
!     ------------------------------------------------------------------
    real(kind=8) :: depide
    save                 depide
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data                 depide/-1.d0/
    if (depide .lt. 0) then
        depide = r8depi()
        depide = depide * depide
        depide = 1.d0 / depide
    endif
    if (omega .ge. 0.d0) then
        freqom = + sqrt( + omega * depide )
    else
        freqom = - sqrt( - omega * depide )
    endif
end function
