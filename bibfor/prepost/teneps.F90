subroutine teneps(jrwork, adr, sig, eps, epse,&
                  epsp)
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
!
    implicit     none
#include "jeveux.h"
!
    integer :: jrwork, adr
    real(kind=8) :: sig(6), eps(6), epse(6), epsp(6)
!
!
! ---------------------------------------------------------------------
! BUT: POUR UN NUMERO D'ORDE, RECUPERER LES TENSEURS DE CONTRAINTE ET
!                      DEFORMATION
! ---------------------------------------------------------------------
! ARGUMENTS:
!    JRWORK : IN : ADRESSE DE VECTEUR DE TRAVAIL ACTUEL
!    ARD    : IN : DECALGE DU NUMRO D'ORDE EN COURS
!    SIG    : OUT : CONTRAINTE (6 COMPOSANTES)
!    EPS    : OUT : DEFORMATION TOTALE (6 COMPOSANTES)
!    EPSE   : OUT : DEFORMATION ELASTIQUE (6 COMPOSANTES)
!    SIG    : OUT : DEFORMATION PLASTIQUE (6 COMPOSANTES)
!-----------------------------------------------------------------------
    integer :: k
!
    do 25 k = 1, 6
        sig(k) = 0.0d0
        eps(k) = 0.0d0
        epse(k)= 0.0d0
        epsp(k)= 0.0d0
25  end do
!
    do 35 k = 1, 6
        sig(k) = zr(jrwork + adr + k - 1)
        eps(k) = zr(jrwork + adr + k - 1 + 6)
        epsp(k) = zr(jrwork + adr + k - 1 + 12)
        epse(k) = eps(k) - epsp(k)
35  end do
!
! ON SUPPOSE QUE EPS_TOT = EPS_ELAS + EPSPLAS
!
!       EPSE(1) = C1*SIG(1) - C2*(SIG(1) + SIG(2) + SIG(3))
!       EPSE(2) = C1*SIG(2) - C2*(SIG(1) + SIG(2) + SIG(3))
!       EPSE(3) = C1*SIG(3) - C2*(SIG(1) + SIG(2) + SIG(3))
!       EPSE(4) = C1*SIG(4)
!       EPSE(5) = C1*SIG(5)
!       EPSE(6) = C1*SIG(6)
!
!       DO 45 K = 1, 6
!         EPSP(K) =  EPS(K) - EPSE(K)
! 45    CONTINUE
!
end subroutine
