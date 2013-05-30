function nmcri9(dp)
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
! person_in_charge: sebastien.fayolle at edf.fr
    implicit none
!
!     ARGUMENTS:
!     ----------
    include 'asterfort/eccook.h'
    real(kind=8) :: nmcri9, dp
! ----------------------------------------------------------------------
!   BUT: EVALUER LA FONCTION DONT ON CHERCHE LE ZERO POUR LA PLASTICITE
!         DE VON_MISES ISOTROPE AVEC ECROUISSAGE DE JOHNSON_COOK
!
!    IN: DP     : DEFORMATION PLASTIQUE CUMULEE
!   OUT: NMCRI9 : CRITERE NON LINEAIRE A RESOUDRE EN DP
!                   (DONT ON CHERCHE LE ZERO)
!                   ICI ON SUPPOSE LE CRITERE DE VON_MISES
!                   AVEC ECROUISSAGE ISOTROPE EN PUISSANCE
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: rpp
!
    common /rconm9/acook,bcook,ccook,npuis,mpuis,&
     &               epsp0,troom,tmelt,tp,dinst,sieleq,deuxmu,rprim,pm
!
    real(kind=8) :: acook, bcook, ccook, npuis, mpuis, epsp0, troom, tmelt
    real(kind=8) :: dinst
    real(kind=8) :: tp, sieleq, deuxmu, rprim, pm
!
! DEB-------------------------------------------------------------------
!
    call eccook(acook, bcook, ccook, npuis, mpuis,&
                epsp0, troom, tmelt, tp, dinst,&
                pm, dp, rpp, rprim)
    nmcri9= rpp + 1.5d0*deuxmu*dp - sieleq
!
end function
