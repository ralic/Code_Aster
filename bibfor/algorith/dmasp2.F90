function dmasp2(rho11, rho12, rho21, sat, phi,&
                cs, pas, emmag, em)
    implicit none
#include "asterf_types.h"
    real(kind=8) :: rho11, rho12, rho21, sat, phi, cs, pas, dmasp2
    real(kind=8) :: em, dphip2
    aster_logical :: emmag
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- CALCUL DE LA DERIVEE DE L APPORT MASSIQUE D AIR SEC PAR RAPPORT --
! --- A LA PRESSION DE GAZ ---------------------------------------------
! ======================================================================
    if (emmag) then
        dphip2 = em
        dmasp2 = rho21*((1.d0-sat)*dphip2 + phi*(1.d0-sat)*(rho11- rho12)/rho11/pas)
    else
        dmasp2 = rho21*((1.d0-sat)*cs + phi*(1.d0-sat)*(rho11-rho12)/ rho11/pas)
    endif
! ======================================================================
end function
