subroutine pha300(ifoi, ptf, phase)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     PROJECTION D'UN SPECTRE D'EXCITATION TURBULENTE REPARTIE SUR UNE
!     BASE MODALE PERTURBEE PAR COUPLAGE FLUIDE-STRUCTURE
!     VALEURS DES PHASES POUR LES INTERSPECTRES GRAPPE1, DEBIT 300M3/H
!-----------------------------------------------------------------------
! IN  : IFOI   : COMPTEUR DES INTERSPECTRES AU-DESSUS DE LA DIAGONALE
!                IFO = (IFO2-1)*(IFO2-2)/2 + IFO1
!                IFO1 INDICE DE LIGNE , IFO2 INDICE DE COLONNE (IFO2>1)
! IN  : PTF    : VALEUR DE LA FREQUENCE
! OUT : PHASE  : VALEUR DE LA PHASE
!
!
#include "jeveux.h"
#include "asterc/r8pi.h"
    integer :: ifoi
    real(kind=8) :: ptf, phase
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    real(kind=8) :: pi
!-----------------------------------------------------------------------
    pi = r8pi()
!
    goto (12,13,23,14,24,34,15,25,35,45,16,26,36,46,56) ifoi
!
12  continue
    phase = pi
    goto 100
!
13  continue
    if (ptf .le. 70.d0) then
        phase = pi
    else if (ptf.gt.70.d0 .and. ptf.le.130.d0) then
        phase = -2.d0*pi/3.d0
    else
        phase = pi
    endif
    goto 100
!
23  continue
    if (ptf .le. 55.d0) then
        phase = 0.d0
    else if (ptf.gt.55.d0 .and. ptf.le.75.d0) then
        phase = pi/4.d0
    else if (ptf.gt.75.d0 .and. ptf.le.90.d0) then
        phase = 2.d0*pi/3.d0
    else if (ptf.gt.90.d0 .and. ptf.le.115.d0) then
        phase = 5.d0*pi/6.d0
    else if (ptf.gt.115.d0 .and. ptf.le.130.d0) then
        phase = 2.d0*pi/3.d0
    else
        phase = pi/2.d0
    endif
    goto 100
!
14  continue
    if (ptf .le. 30.d0) then
        phase = pi
    else if (ptf.gt.30.d0 .and. ptf.le.50.d0) then
        phase = 0.d0
    else if (ptf.gt.50.d0 .and. ptf.le.65.d0) then
        phase = pi/3.d0
    else if (ptf.gt.65.d0 .and. ptf.le.80.d0) then
        phase = 0.d0
    else if (ptf.gt.80.d0 .and. ptf.le.110.d0) then
        phase = 2.d0*pi/3.d0
    else if (ptf.gt.110.d0 .and. ptf.le.130.d0) then
        phase = pi/3.d0
    else
        phase = 0.d0
    endif
    goto 100
!
24  continue
    if (ptf .le. 30.d0) then
        phase = 0.d0
    else if (ptf.gt.30.d0 .and. ptf.le.50.d0) then
        phase = pi
    else if (ptf.gt.50.d0 .and. ptf.le.65.d0) then
        phase = -pi/2.d0
    else if (ptf.gt.65.d0 .and. ptf.le.80.d0) then
        phase = pi
    else if (ptf.gt.80.d0 .and. ptf.le.110.d0) then
        phase = 0.d0
    else
        phase = -pi/2.d0
    endif
    goto 100
!
34  continue
    if (ptf .le. 30.d0) then
        phase = 0.d0
    else
        phase = pi
    endif
    goto 100
!
15  continue
    if (ptf .le. 5.d0) then
        phase = pi
    else if (ptf.gt.5.d0 .and. ptf.le.105.d0) then
        phase = 0.d0
    else
        phase = pi
    endif
    goto 100
!
25  continue
    if (ptf .le. 110.d0) then
        phase = pi
    else if (ptf.gt.110.d0 .and. ptf.le.125.d0) then
        phase = -pi/2.d0
    else
        phase = -pi/4.d0
    endif
    goto 100
!
35  continue
    if (ptf .le. 50.d0) then
        phase = pi
    else if (ptf.gt.50.d0 .and. ptf.le.125.d0) then
        phase = pi/3.d0
    else
        phase = -2.d0*pi/3.d0
    endif
    goto 100
!
45  continue
    if (ptf .le. 10.d0) then
        phase = -pi/2.d0
    else if (ptf.gt.10.d0 .and. ptf.le.20.d0) then
        phase = 0.d0
    else if (ptf.gt.20.d0 .and. ptf.le.25.d0) then
        phase = -pi/2.d0
    else if (ptf.gt.25.d0 .and. ptf.le.110.d0) then
        phase = pi
    else if (ptf.gt.110.d0 .and. ptf.le.125.d0) then
        phase = 2.d0*pi/3.d0
    else
        phase = pi/2.d0
    endif
    goto 100
!
16  continue
    phase = pi
    goto 100
!
26  continue
    if (ptf .le. 35.d0) then
        phase = 0.d0
    else if (ptf.gt.35.d0 .and. ptf.le.45.d0) then
        phase = -pi/2.d0
    else if (ptf.gt.45.d0 .and. ptf.le.55.d0) then
        phase = 0.d0
    else if (ptf.gt.55.d0 .and. ptf.le.65.d0) then
        phase = -pi/4.d0
    else if (ptf.gt.65.d0 .and. ptf.le.75.d0) then
        phase = pi/12.d0
    else if (ptf.gt.75.d0 .and. ptf.le.90.d0) then
        phase = 0.d0
    else if (ptf.gt.90.d0 .and. ptf.le.105.d0) then
        phase = -pi/6.d0
    else if (ptf.gt.105.d0 .and. ptf.le.115.d0) then
        phase = -2.d0*pi/3.d0
    else if (ptf.gt.115.d0 .and. ptf.le.125.d0) then
        phase = pi/6.d0
    else
        phase = pi/4.d0
    endif
    goto 100
!
36  continue
    if (ptf .le. 30.d0) then
        phase = 0.d0
    else if (ptf.gt.30.d0 .and. ptf.le.45.d0) then
        phase = pi
    else if (ptf.gt.45.d0 .and. ptf.le.50.d0) then
        phase = pi/4.d0
    else if (ptf.gt.50.d0 .and. ptf.le.60.d0) then
        phase = -pi/2.d0
    else if (ptf.gt.60.d0 .and. ptf.le.90.d0) then
        phase = pi/8.d0
    else if (ptf.gt.90.d0 .and. ptf.le.110.d0) then
        phase = pi/4.d0
    else if (ptf.gt.110.d0 .and. ptf.le.130.d0) then
        phase = 0.d0
    else
        phase = -pi/6.d0
    endif
    goto 100
!
46  continue
    if (ptf .le. 45.d0) then
        phase = 0.d0
    else if (ptf.gt.45.d0 .and. ptf.le.55.d0) then
        phase = pi/4.d0
    else if (ptf.gt.55.d0 .and. ptf.le.80.d0) then
        phase = 0.d0
    else if (ptf.gt.80.d0 .and. ptf.le.90.d0) then
        phase = pi/3.d0
    else
        phase = pi
    endif
    goto 100
!
56  continue
    if (ptf .le. 95.d0) then
        phase = pi
    else
        phase = 7.d0*pi/12.d0
    endif
!
100  continue
end subroutine
