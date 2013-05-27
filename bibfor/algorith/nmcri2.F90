function nmcri2(dp)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!     ARGUMENTS:
!     ----------
    include 'asterfort/ecpuis.h'
    real(kind=8) :: nmcri2, dp
! ----------------------------------------------------------------------
!   BUT: EVALUER LA FONCTION DONT ON CHERCHE LE ZERO POUR LA PLASTICITE
!         DE VON_MISES ISOTROPE AVEC ECROUISSAGE EN PUISSANCE
!
!    IN: DP     : DEFORMATION PLASTIQUE CUMULEE
!   OUT: NMCRI2 : CRITERE NON LINEAIRE A RESOUDRE EN DP
!                   (DONT ON CHERCHE LE ZERO)
!                   ICI ON SUPPOSE LE CRITERE DE VON_MISES
!                   AVEC ECROUISSAGE ISOTROPE EN PUISSANCE
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: rpp
!
!----- COMMONS NECESSAIRES A VON_MISES ISOTROPE PUISSANCE :
!      COMMONS COMMUNS A NMCRI2 ET NMISOT
    common /rconm1/deuxmu,nu,e,sigy,rprim,pm,sigel,line
    real(kind=8) :: deuxmu, nu, e, sigy, rprim, pm, sigel(6), line
    common /rconm2/alfafa,unsurn,sieleq
    real(kind=8) :: alfafa, unsurn, sieleq
!
! DEB-------------------------------------------------------------------
!
    call ecpuis(e, sigy, alfafa, unsurn, pm,&
                dp, rpp, rprim)
    nmcri2= rpp + 1.5d0*deuxmu*dp - sieleq
!
end function
