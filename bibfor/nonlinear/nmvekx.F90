subroutine nmvekx(imate, tp, xhi, kxhi, dkxidx)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!
    implicit none
!
    include 'asterfort/rcvala.h'
    integer :: imate
    real(kind=8) :: xhi, kxhi, dkxidx
!
! ----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
!     CHABOCHE AVEC ENDOMAGEMENT
!     METHODE ITERATIVE D'EULER IMPLICITE
!
!     CALCUL DE LA CARACTERISTIQUE K DU MATERIAU FONCTION DE TEMPE+ ET
!     DE SIGMA
!     DERIVEE DK/DSIGMA
! ----------------------------------------------------------------------
! IN  IMATE  : ADRESSE DU MATERIAU CODE
!     XHI    : CRITERE D'ENDOMMAGEMENT VISCOPLASTIQUE
!
! OUT KXHI   : CARACTERISTIQUE VISQUEUSE EN T+
!     DKXIDX : DERIVEE DK/DXHI
!
! INFO      KXHI = MATE(9,2) = K_D          (ENDOMMAGEMENT)
! ----------------------------------------------------------------------
!
    real(kind=8) :: vpar(2), tp
    real(kind=8) :: zero
    parameter    (zero = 0.d0)
    integer :: ok
    character(len=8) :: nompar(2), nom
!
    data nom / 'K_D' /
!
! ----------------------------------------------------------------------
!-- 1. INITIALISATION
!--------------------
    kxhi = zero
    dkxidx = zero
!
!-- 2. CALCUL DE K_D(XHI,TEMP) A PARTIR DU MATERIAU CODE
!------------------------------------------------------
    nompar(1) = 'TEMP    '
    nompar(2) = 'X       '
!
    vpar(1) = tp
    vpar(2) = xhi
!
    call rcvala(imate, ' ', 'VENDOCHAB', 2, nompar,&
                vpar, 1, nom, kxhi, ok,&
                2)
!
!-- 3. CREATION D'UNE FONCTION KT(XHI) EXTRAITE DE LA NAPPE
!          A LA TEMPERATURE COURANTE
!-----------------------------------------------------------
!CCCC      CALL RCNAPP (IMATE,TP,IFON)
!
!-- 4.  CALCUL DE LA DERIVEE DK(XHI)/DXHI
!--------------------------------------------
!CCCC      CALL RCFODE (IFON,XHI,KXHI,DKHIDX)
!
end subroutine
