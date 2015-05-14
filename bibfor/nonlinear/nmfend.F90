function nmfend(dr)
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
! aslint: disable=W1304
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "asterc/r8miem.h"
    real(kind=8) :: nmfend, dr
! ----------------------------------------------------------------------
!    BUT:  EVALUER LA DERIVEE DE LA FONCTION DONT ON CHERCHE LE ZERO
!          POUR VENDOCHAB
!
!     IN:  DR     : DEFORMATION PLASTIQUE CUMULEE*(1-D)
!    OUT:  NMFEND : VALEUR DE LA FONCTION
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
!----- COMMONS NECESSAIRES A VENDOCHAB
    common   /fvendo/mu,syvp,kvp,rm,dm,seqe,ad,dt,rd,unsurn,unsurm
    real(kind=8) :: mu, syvp, kvp, rm, dm, seqe, ad, dt, unsurn, unsurm
    real(kind=8) :: dd, rd, gder, unmd, dtn, puis
!
    dtn=dt**unsurn
!
    if (dr .lt. r8miem()) then
        gder=0.d0
        dd=0.d0
    else
        gder=kvp*(dr**unsurn)*((rm+dr)**unsurm)+syvp*dtn
        puis=1.d0-rd*unsurn
        dd=(dt**puis)*((gder/ad)**rd)
    endif
    unmd=1.d0-dm-dd
    nmfend= gder*unmd+3.d0*mu*dr*dtn-seqe*unmd*dtn
!
end function
