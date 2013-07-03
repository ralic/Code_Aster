function nmcri6(dp)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "asterfort/ecpuis.h"
    real(kind=8) :: nmcri6, dp
! ----------------------------------------------------------------------
!   BUT: EVALUER LA FONCTION DONT ON CHERCHE LE ZERO POUR LA PLASTICITE
!        DE VON_MISES ISOTROPE AVEC ECROUISSAGE EN PUISSANCE SIMO_MIEHE
!
!    IN: DP     : DEFORMATION PLASTIQUE CUMULEE
!   OUT: NMCRI6 : CRITERE NON LINEAIRE A RESOUDRE EN DP
!                   (DONT ON CHERCHE LE ZERO)
!                   ICI ON SUPPOSE LE CRITERE DE VON_MISES
!                   AVEC ECROUISSAGE ISOTROPE EN PUISSANCE
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: unsurn, rpp, rprim
!
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE PUISSANCE :
!      COMMONS COMMUNS A NMCRI2 ET NMISOT
!
    common /rconm6/mutrbe,tauteq
    real(kind=8) :: mutrbe, tauteq
!
    integer :: jprol, jvale, nbval
    real(kind=8) :: pm, young, nu, mu, unk, troisk, cother, sigy
    real(kind=8) :: sigm0, epsi0, dt, coefm, rpm, pente, apui, npui
    common /lcpim/&
     &          pm,young,nu,mu,unk,troisk,cother,&
     &          sigm0,epsi0,dt,coefm,rpm,pente,&
     &          apui,npui,sigy,jprol,jvale,nbval
!
! DEB-------------------------------------------------------------------
!
    unsurn=1.d0/npui
    call ecpuis(young, sigy, apui, unsurn, pm,&
                dp, rpp, rprim)
    nmcri6= rpp + mutrbe*dp - tauteq
!
end function
