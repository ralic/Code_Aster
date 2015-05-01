subroutine mahsms(ind1, nb1, xi, ksi3s2, intsr,&
                  xr, epais, vectn, vectg, vectt,&
                  hsfm, hss)
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
    implicit none
#include "asterfort/hfmss.h"
#include "asterfort/vectgt.h"
    integer :: nb1, intsr
    real(kind=8) :: xi(3, *), xr(*), vectn(9, 3)
    real(kind=8) :: epais, ksi3s2
    real(kind=8) :: vectg(2, 3), vectt(3, 3), hsfm(3, 9), hss(2, 9)
!
!
!     CONSTRUCTION DU VECTEUR N AUX PTS D'INTEGRATION REDUIT
!     (POUR CHAQUE INTSR, STOCKAGE DANS VECTT)
!
!     ET
!
!     CONSTRUCTION DES VECTEURS GA AUX PTS D'INTEGRATION REDUIT
!     (POUR CHAQUE INTSR, STOCKAGE DANS VECTG)
!
!     ET
!
!     CONSTRUCTION DES VECTEURS TA AUX PTS D'INTEGRATION REDUIT (T3=N)
!     (POUR CHAQUE INTSR, STOCKAGE DANS VECTT)
!
!     IND1= 0     0 : CALCULS AUX PTS D'INTEGRATION REDUIT
!
!-----------------------------------------------------------------------
    integer :: ind1, ind2
!-----------------------------------------------------------------------
    call vectgt(ind1, nb1, xi, ksi3s2, intsr,&
                xr, epais, vectn, vectg, vectt)
!
!     CONSTRUCTION DE HSM = HFM * S:(3,9) AUX PTS D'INTEGRATION REDUITS
!
!     ET
!
!     CONSTRUCTION DE HSS = HS * S :(2,9) AUX PTS D'INTEGRATION REDUITS
!
!     IND2= 1  --->  CALCUL DE HSS ( 0 SINON )
!
    ind2= 1
!
    call hfmss(ind2, vectt, hsfm, hss)
!
end subroutine
