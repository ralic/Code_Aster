subroutine ecpuis(e, sigy, alfafa, unsurn, pm,&
                  dp, rp, rprim)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: dp, rprim, e, sigy, pm, rp, alfafa, unsurn
! ----------------------------------------------------------------------
! BUT: EVALUER LA FONCTION D'ECROUISSAGE ISOTROPE AVEC LOI EN PUISSANCE
!    IN: DP     : DEFORMATION PLASTIQUE CUMULEE
!   OUT: RP     : R(PM+DP)
!   OUT: RPRIM  : R'(PM+DP)
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: p0, rp0, coef
!
    p0=1.d-10
    if ((pm+dp) .le. p0) then
        rp0= sigy*(e/alfafa/sigy*(p0))**unsurn + sigy
        rp= sigy+(pm+dp)*(rp0-sigy)/p0
        rprim=(rp0-sigy)/p0
    else
        coef = e/alfafa/sigy
        rp= sigy*(e/alfafa/sigy*(pm+dp))**unsurn + sigy
        rprim= unsurn * sigy * coef * (coef*(pm+dp))**(unsurn-1.d0)
    endif
!
end subroutine
