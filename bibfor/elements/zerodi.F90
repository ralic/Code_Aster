subroutine zerodi(x, y)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/utmess.h"
    real(kind=8) :: x(4), y(4)
! ----------------------------------------------------------------------
!  RESOLUTION D'EQUATIONS SCALAIRES PAR UNE METHODE DE DICHOTOMIE
! ----------------------------------------------------------------------
! VAR X(1) BORNE DE L'INTERVALLE DE RECHERCHE  TQ Y(1) < 0
! VAR X(2) BORNE DE L'INTERVALLE DE RECHERCHE  TQ Y(2) > 0
! VAR X(3) SOLUTION X(N-1) PUIS SOLUTION EN X(N)  (NE SERT PAS)
! VAR X(4) SOLUTION X(N)   PUIS SOLUTION EN X(N+1)
! VAR Y(I) VALEUR DE LA FONCTION EN X(I)
! ----------------------------------------------------------------------
!
    real(kind=8) :: xp
!
!    REACTUALISATION DE L'INTERVALLE DE RECHERCHE
    if (y(4) .lt. 0.d0) then
        x(1) = x(4)
        y(1) = y(4)
    endif
!
    if (y(4) .gt. 0.d0) then
        x(2) = x(4)
        y(2) = y(4)
    endif
!
!    CONSTRUCTION D'UN NOUVEL ESTIME
    if (x(1) .eq. x(2)) then
        call utmess('F', 'ALGORITH9_84')
    endif
    xp = (x(1) + x(2)) / 2.d0
!
!    DECALAGE DES ITERES
    x(3) = x(4)
    x(4) = xp
    y(3) = y(4)
!
end subroutine
