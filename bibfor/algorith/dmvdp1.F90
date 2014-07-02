function dmvdp1(rho11, rho12, sat, dsatp1, phi,&
                cs, pvp, emmag, em)
    implicit none
#include "asterf_types.h"
    real(kind=8) :: rho11, rho12, sat, dsatp1, phi, cs, pvp, dmvdp1
    real(kind=8) :: em, dphip1
    aster_logical :: emmag
! ======================================================================
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- CALCUL DE L APPORT MASSIQUE DE VAPEUR PAR RAPPORT A LA PRESSION --
! --- CAPILLAIRE -------------------------------------------------------
! ======================================================================
    if (emmag) then
        dphip1 = - sat*em
        dmvdp1 = rho12 * (-dsatp1*phi + (1.d0-sat)*dphip1 - phi*(1.d0- sat)/pvp*rho12/rho11)
    else
        dmvdp1 = rho12 * (-dsatp1*phi - (1.d0-sat)*sat*cs - phi*(1.d0- sat)/pvp*rho12/rho11)
! ======================================================================
    endif
end function
