subroutine psvari(compor, nbvari, dimens, ipop1, ipop2)
    implicit none
#include "asterfort/utmess.h"
    character(len=2) :: dimens
    character(len=16) :: compor
    integer :: ipop1, ipop2, nbvari
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     ------------------------------------------------------------------
!     FONCTION REALISEE :
!
!     PERMET DE CONNAITRE EN FONCTION DE LA RELATION DE COMPORTEMENT
!     PARMI LES VARIABLES INTERNES LA POSITION DE :
!
!         - LA DEFORMATION PLASTIQUE CUMULEE
!         - L'INDICATEUR DE PLASTICITE
!
! ENTREE  --->  COMPOR : NOM DE LA RELATION DE COMPORTEMENT
!         --->  NBVARI : NOMBRE DE VARIABLES INTERNES
!         --->  DIMENS : DIMENSION DU PROBLEME '2D', '3D'
!
! SORTIE
!         --->  IPOS1  : POSITION DE LA DEFORMATION PLASTIQUE CUMULEE
!         --->  IPOS2  : POSITION DE L'INDICATEUR DE PLASTICITE
!
!     ------------------------------------------------------------------
!
!
    if ((compor.eq.'LEMAITRE' ) .or. (compor.eq.'VMIS_ECMI_TRAC') .or.&
        (compor.eq.'VMIS_ECMI_LINE') .or. (compor.eq.'VMIS_CIN1_CHAB') .or.&
        (compor.eq.'VMIS_CIN2_CHAB') .or. (compor.eq.'VISC_CIN1_CHAB') .or.&
        (compor.eq.'VISC_CIN2_CHAB') .or. (compor.eq.'VMIS_ISOT_TRAC') .or.&
        (compor.eq.'VMIS_ISOT_LINE') .or. (compor.eq.'VISC_ISOT_TRAC') .or.&
        (compor.eq.'VISC_ISOT_LINE')) then
        ipop1=1
        ipop2=2
    else if ((compor.eq.'ROUSSELIER')) then
        ipop1 = 1
        ipop2 = 3
        else if ( (compor.eq.'ROUSS_PR') .or.(compor.eq.'ROUSS_VISC') )&
    then
        ipop1=1
        ipop2=nbvari
    else if (compor.eq.'MONOCRISTAL') then
        ipop1 = nbvari-1
        ipop2 = nbvari
    else if (compor.eq.'POLYCRISTAL') then
        ipop1 = 7
        ipop2 = nbvari
    else
!
        call utmess('F', 'ELEMENTS2_45')
!
    endif
!
end subroutine
