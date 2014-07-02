function dmwdp2(rho11, sat, phi, cs, cliq,&
                dp11p2, emmag, em)
    implicit none
#include "asterf_types.h"
    real(kind=8) :: rho11, sat, phi, cs, cliq, dp11p2, dmwdp2
    real(kind=8) :: em
    real(kind=8) :: dphip2
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
! --- CALCUL DE LA DERIVEE DE L APPORT MASSIQUE DE L EAU PAR RAPPORT ---
! --- A LA PRESSION DE GAZ ---------------------------------------------
! ======================================================================
    if (emmag) then
        dphip2 = em
        dmwdp2 = rho11 *(phi*dphip2+phi*cliq*dp11p2)
    else
        dmwdp2 = rho11*sat*(phi*cliq*dp11p2+cs)
    endif
! ======================================================================
end function
