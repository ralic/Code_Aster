subroutine eccook(acook, bcook, ccook, npuis, mpuis,&
                  epsp0, troom, tmelt, tp, dinst,&
                  pm, dp, rp, rprim)
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
    real(kind=8) :: acook, bcook, ccook, npuis, mpuis, epsp0, troom, tmelt, tp
    real(kind=8) :: dinst, pm, dp, rp, rprim
! ----------------------------------------------------------------------
! BUT: EVALUER LA FONCTION D'ECROUISSAGE ISOTROPE AVEC
!      LA LOI DE JOHNSON-COOK
!    IN: DP     : DEFORMATION PLASTIQUE CUMULEE
!   OUT: RP     : R(PM+DP)
!   OUT: RPRIM  : R'(PM+DP)
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: p0
!
    p0=1.d-9
    if ((pm+dp) .le. p0) then
        rp= (acook+bcook*(p0)**npuis)
        rprim=npuis*bcook*(p0)**(npuis-1.d0)
    else
        rp=(acook+bcook*(pm+dp)**npuis)
        rprim=npuis*bcook*(pm+dp)**(npuis-1.d0)
        if (dp/dinst .gt. epsp0) then
            rprim=rprim+ccook*(rprim*log(dp/dinst/epsp0)+rp/dp)
            rp=rp*(1.d0+ccook*log(dp/dinst/epsp0))
        endif
        if ((tp.gt.troom) .and. (troom.ge.-0.5d0)) then
            rp=rp*(1.d0-((tp-troom)/(tmelt-troom))**mpuis)
            rprim=rprim*(1.d0-((tp-troom)/(tmelt-troom))**mpuis)
        endif
    endif
!
end subroutine
