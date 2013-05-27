subroutine cgveca(ndim, option, cas)
    implicit none
!
    integer :: ndim
    character(len=16) :: option, cas
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : DETERMINATION DU CAS : 2D, 3D LOCAL OU 3D GLOBAL
!
!  IN :
!    NDIM   : DIMENSION DU PROBLEME
!    OPTION : OPTION DE CALC_G
!  OUT :
!    CAS    : '2D', '3D LOCAL' OU '3D GLOBAL'
! ======================================================================
!
!     DETERMINATION DU CAS : 2D, 3D LOCAL OU 3D GLOBAL
    if (ndim .eq. 3) then
!
        if (option .eq. 'CALC_G_GLOB' .or. option .eq. 'G_MAX_GLOB' .or. option .eq.&
            'G_BILI_GLOB') then
            cas = '3D_GLOBAL'
        else
            cas = '3D_LOCAL'
        endif
!
    else if (ndim.eq.2) then
!
        cas = '2D'
!
    endif
!
end subroutine
