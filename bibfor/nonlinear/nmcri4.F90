function nmcri4(dp)
! ----------------------------------------------------------------------
    implicit none
    real(kind=8) :: nmcri4
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
!
!     EVALUATION DE LA FONCTION NON LINEAIRE DONT ON CHERCHE LE ZERO
!     POUR LA VISCOELASTICITE DES ELEMENTS DE POUTRE.
! ----------------------------------------------------------------------
!
!     IN  :  EFF    : EFFORT NORMAL
!     OUT :  NMCRI4 : VALEUR DE LA FONCTION DE EFF DONT ON CHERCHE LE 0.
!
! **************** COMMON COMMUN A NMCRI4 ET NMFGAS  *******************
!
    common /rconm4/ you,cfluag,sige,pmm,sdt
    integer :: ncoeff
!-----------------------------------------------------------------------
    real(kind=8) :: dp
!-----------------------------------------------------------------------
    parameter (ncoeff = 7)
    real(kind=8) :: cfluag(ncoeff), you, sige, pmm, sdt
    real(kind=8) :: t1, t2, p1
!
! ********************* DEBUT DE LA SUBROUTINE *************************
!
    if ((abs(sige)-you*dp) .le. 0.d0) then
        t1 = 0.d0
    else
        t1 = (abs(sige)-you*dp) ** cfluag(1)
    endif
    p1 = cfluag(1)*cfluag(3)
    if ((pmm+dp) .le. 0.d0) then
        t2 = 0.d0
    else
        t2 = (pmm+dp)**p1
    endif
    nmcri4 = dp * t2 * you - t1 * sdt * you
!
! ----------------------------------------------------------------------
!
end function
