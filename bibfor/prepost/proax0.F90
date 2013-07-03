subroutine proax0(ui, vi, csta, cstb, a1,&
                  b1, u0, v0, rpax)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean.angles at edf.fr
    implicit      none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    real(kind=8) :: ui, vi, csta, cstb, a1, b1, u0, v0, rpax
! ----------------------------------------------------------------------
! BUT: PROJETER SUR L'AXE 2 LES POINTS REPRESANTANT LE
!      CISAILLEMENT TAU DANS LE PLAN u, v.
! ----------------------------------------------------------------------
! ARGUMENTS:
! UI        IN   R  : COMPOSANTES u DU VECTEUR TAU (CISAILLEMENT),
!                     POUR LE VECTEUR NORMAL COURANT.
! VI        IN   R  : COMPOSANTES v DU VECTEUR TAU (CISAILLEMENT),
!                     POUR LE VECTEUR NORMAL COURANT.
! CSTA      IN   R  : CONSTANTE A POUR LE VECTEUR NORMAL COURANT.
! CSTB      IN   R  : CONSTANTE B POUR LE VECTEUR NORMAL COURANT.
! A1        IN   R  : CONSTANTE A1 POUR LE VECTEUR NORMAL COURANT.
! B1        IN   R  : CONSTANTE B1 POUR LE VECTEUR NORMAL COURANT.
! U0        IN   R  : VALEUR CENTRALE DES u, POUR LE VECTEUR NORMAL
!                     COURANT.
! V0        IN   R  : VALEUR CENTRALE DES v, POUR LE VECTEUR NORMAL
!                     COURANT.
! RPAX      OUT  R  : VALEUR DE L'AMPLITUDE DU POINT PROJETE SUR L'AXE
!                     CHOISI, POUR LE VECTEUR NORMAL COURANT.
!
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    real(kind=8) :: uip, vip, a3, b3
    real(kind=8) :: up, vp, val
!-----------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
    rpax = 0.0d0
!
    uip = ui + 1.0d0
    vip = vi - (csta/cstb)*(uip-ui)
!
    a3 = (vip-vi)/(uip-ui)
    b3 = (uip*vi - ui*vip)/(uip-ui)
!
    up = (b3-b1)/(a1-a3)
    vp = (a1*b3 - a3*b1)/(a1-a3)
    val = sqrt((up-u0)**2 + (vp-v0)**2)
!
    if (up .lt. u0) then
        val = -val
    endif
    rpax = val
!
    call jedema()
end subroutine
