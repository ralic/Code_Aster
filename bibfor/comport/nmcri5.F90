function nmcri5(dp)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "asterfort/rcfon2.h"
    real(kind=8) :: nmcri5, dp
! ----------------------------------------------------------------------
!    BUT:  EVALUER LA FONCTION DONT ON CHERCHE LE ZERO
!          POUR LA PLASTICITE DE VON_MISES ISOTROPE + CINEMATIQUE C_PLAN
!
!     IN:  DP     : DEFORMATION PLASTIQUE CUMULEE
!    OUT:  NMCRI5 : CRITERE NON LINEAIRE A RESOUDRE EN DP
!                   (DONT ON CHERCHE LE ZERO)
!                   ICI ON SUPPOSE LE CRITERE DE VON_MISES EN C_PLAN
!
! ----------------------------------------------------------------------
!
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE + CINEMATIQUE C_PLAN :
!      COMMONS COMMUNS A NMCRI1 ET NMECMI
    common /rconm5/deuxmu,troisk,sigy,rprim,pm,sigel,tp2,line,prag,xm
    common /kconm1/imate2, jprol2, jvale2,nbval2
!     VARIABLES LOCALES:
!     ------------------
    integer :: imate2, jprol2, jvale2, nbval2
    real(kind=8) :: drdp, prag, hp, fp, gp, hsg, dx, rpp, demuc, xm(6)
    real(kind=8) :: deuxmu, troisk, sigy, rprim, pm, sigel(6), tp2, line
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (line .ge. 0.5d0) then
        rpp = sigy +rprim*(pm+dp)
    else
        call rcfon2('V', jprol2, jvale2, nbval2, &
                    p = pm+dp, rp = rpp, rprim = drdp,&
                    c = prag)
    endif
    gp=1.d0+1.5d0*prag*dp/rpp
    hp=gp+1.5d0*deuxmu*dp/rpp
    hsg=hp/gp
    demuc=deuxmu+prag
!
    dx=  (hsg-1.d0)* sigel(3)
    dx=dx/( deuxmu/1.5d0 + troisk*hsg/3.d0 )
!
    fp= (sigel(1)-deuxmu/3.d0*dx)**2 + (sigel(2)-deuxmu/3.d0*dx)**2 +&
     &    (sigel(3)+deuxmu/3.d0*2.d0*dx)**2 + sigel(4)**2
!
    nmcri5= 1.5d0*demuc*dp - sqrt(1.5d0*fp) + rpp
!
end function
