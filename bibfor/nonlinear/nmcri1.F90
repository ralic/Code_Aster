function nmcri1(dp)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/rcfonc.h'
    real(kind=8) :: nmcri1, dp
! ----------------------------------------------------------------------
!    BUT:  EVALUER LA FONCTION DONT ON CHERCHE LE ZERO
!          POUR LA PLASTICITE DE VON_MISES ISOTROPE C_PLAN
!
!     IN:  DP     : DEFORMATION PLASTIQUE CUMULEE
!    OUT:  NMCRI1 : CRITERE NON LINEAIRE A RESOUDRE EN DP
!                   (DONT ON CHERCHE LE ZERO)
!                   ICI ON SUPPOSE LE CRITERE DE VON_MISES EN C_PLAN
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: g, dx, rpp
!
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE C_PLAN :
!      COMMONS COMMUNS A NMCRI1 ET NMISOT
    common /rconm1/deuxmu,nu,e,sigy,rprim,pm,sigel,line
    common /rconm2/alfafa,unsurn,sieleq
    common /kconm1/imate2, jprol2, jvale2,nbval2
    real(kind=8) :: deuxmu, nu, e, sigy, rprim, pm, sigel(6), line
    integer :: imate2, jprol2, jvale2, nbval2
    real(kind=8) :: drdp, airerp, dum, alfafa, unsurn, sieleq
!
! DEB-------------------------------------------------------------------
!
    if (line .ge. 0.5d0) then
        rpp = sigy +rprim*(pm+dp)
    else if (line.lt.-0.5d0) then
        rpp = sigy + sigy*(e*(pm+dp)/alfafa/sigy)**unsurn
    else
        call rcfonc('V', 1, jprol2, jvale2, nbval2,&
                    dum, dum, dum, pm+dp, rpp,&
                    drdp, airerp, dum, dum)
    endif
!
    dx = 3.d0*(1.d0-2.d0*nu)*sigel(3)*dp/(e*dp+2.d0*(1.d0-nu)*rpp)
!
    g = (&
        sigel(1)-deuxmu/3.d0*dx)**2 + (sigel(2)-deuxmu/3.d0*dx)**2 + (sigel(3)+deuxmu/3.d0*2.d0*d&
        &x)**2 + sigel(4&
        )**2
!
    nmcri1= 1.5d0*deuxmu*dp - sqrt(1.5d0*g) + rpp
!
end function
