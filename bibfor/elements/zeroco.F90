subroutine zeroco(x, y)
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
!
    implicit none
#include "asterc/r8maem.h"
#include "asterfort/utmess.h"
#include "asterfort/zerodi.h"
    real(kind=8) :: x(4), y(4)
! ----------------------------------------------------------------------
!  RESOLUTION D'EQUATIONS SCALAIRES PAR UNE METHODE DE CORDES
! ----------------------------------------------------------------------
! VAR X(1) BORNE DE L'INTERVALLE DE RECHERCHE  TQ Y(1) < 0
! VAR X(2) BORNE DE L'INTERVALLE DE RECHERCHE  TQ Y(2) > 0
! VAR X(3) SOLUTION X(N-1) PUIS SOLUTION EN X(N)
! VAR X(4) SOLUTION X(N)   PUIS SOLUTION EN X(N+1)
! VAR Y(I) VALEUR DE LA FONCTION EN X(I)
! ----------------------------------------------------------------------
!
    real(kind=8) :: xp, p, s
    real(kind=8) :: infini
!
!    TEST DES PRE-CONDITIONS
    if (y(1) .gt. 0 .or. y(2) .lt. 0) then
        call utmess('F', 'ELEMENTS4_61')
    endif
!
!
!    TRAITEMENT DES SITUATIONS AVEC BORNES INFINIES -> DICHOTOMIE
    infini = r8maem()
    if (abs(y(3)) .eq. infini .or. abs(y(4)) .eq. infini) then
        call zerodi(x, y)
        goto 9999
    endif
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
    if (x(3) .eq. x(4)) then
        call utmess('A', 'ALGORITH9_84')
        goto 9999
    endif
    p = (y(4)-y(3)) / (x(4)-x(3))
    if (p .ne. 0.d0) then
        xp = x(3) - y(3)/p
        s = (xp-x(1)) / (x(2)-x(1))
    else
        s = -2.d0
    endif
!
!    CORRECTION DU NOUVEL ESTIME SI NECESSAIRE (EN DEHORS DES BORNES)
    if (s .le. 0.d0 .or. s .ge. 1.d0) then
        if (abs(y(1)) .eq. infini .or. abs(y(2)) .eq. infini) then
            xp = (x(1)+x(2)) / 2
        else
            p = (y(2)-y(1)) / (x(2)-x(1))
            xp = x(1) - y(1)/p
        endif
    endif
!
!    DECALAGE DES ITERES
    x(3) = x(4)
    x(4) = xp
    y(3) = y(4)
!
9999  continue
end subroutine
