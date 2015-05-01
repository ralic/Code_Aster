subroutine edgini(itemax, prec, pm, eqsitr, mu,&
                  gamma, m, n, dp, iret)
    implicit none
!
!
#include "asterfort/edgiso.h"
    integer :: itemax
    real(kind=8) :: prec, pm, eqsitr
    real(kind=8) :: mu, gamma(3), m(3), n(3)
    real(kind=8) :: dp
    integer :: iret
!
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
!
! ----------------------------------------------------------------------
!     MODELE VISCOPLASTIQUE SANS SEUIL DE EDGAR
!    CALCUL DE LA SOLUTION CORRESPONDANT A LA MATRICE ANI ISOTROPE
!    ON SE RAMENE A UNE SEULE EQUATION EN DP
!    SEUIL(DP)=EQSITR-3*MU*DP-GAMMA(K)*((PM+DP)**M(K))*(DP**N(K))=0
!  IN  PM      : DEFORMATION PLASTIQUE CUMULEE A L INSTANT MOINS
!  IN  EQSITR : CONTRAINTE EQUIVALENTE ESSAI
!  IN  MU      : COEFFICIENT DE MATERIAU ELASTIQUE
!  IN  GAMMA   : COEFFICIENT VISQUEUX
!  IN  M       : COEFFICIENT VISQUEUX
!  IN  N       : COEFFICIENT VISQUEUX
!
!  OUT DP      : INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
!  OUT IRET    : CODE RETOUR CALCUL
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
! ----------------------------------------------------------------------
!
    integer :: iter
    real(kind=8) :: dpinf, dpsup, seuil, dseuil
!                              IRET=1 => ECHEC
! ----------------------------------------------------------------------
!
! 1 - MINORANT ET MAJORANT
!
    dpinf = 0.d0
    dpsup = eqsitr/(3.d0*mu)
    iret = 0
!
! 2 - INITIALISATION
!     CALCUL DE SEUIL ET DE SA DERIVEE DSEUIL
!
    dp = dpsup
    call edgiso(dp, pm, eqsitr, mu, gamma,&
                m, n, seuil, dseuil)
!
! 3 - RESOLUTION PAR UNE METHODE DE NEWTON ENTRE LES BORNES
!
    do 10 iter = 1, itemax
        if (abs(seuil) .le. prec*eqsitr) goto 100
!
        dp = dp - seuil/dseuil
        if (dp .le. dpinf .or. dp .ge. dpsup) dp=(dpinf+dpsup)/16.d0
!
        call edgiso(dp, pm, eqsitr, mu, gamma,&
                    m, n, seuil, dseuil)
!
        if (seuil .ge. 0.d0) dpinf = dp
        if (seuil .le. 0.d0) dpsup = dp
!
10  end do
!
    iret = 1
!
100  continue
!
end subroutine
