function xcalc_saut(id_no, id_escl, id_mait, iflag)
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! BUT : CALCULER LA FONCTION SAUT : -2 | 0 | 2
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!   - ID_NO  : IDENTIFIANT <ENTIER> DU DOMAINE DE LA FONCTION HEAVISIDE
!   - ID_ESCL  : IDENTIFIANT DOMAINE ESCLAVE
!   - ID_MAIT  : IDENTIFIANT DOMAINE MAITRE
!   - IFLAG  : FLAG POUR LE MONO HEAVISIDE ==> A NETTOYER
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/xcalc_heav.h"
!-----------------------------------------------------------------------
    integer :: id_no, id_escl, id_mait
    integer, optional ::  iflag
    real(kind=8) :: xcalc_saut
!-----------------------------------------------------------------------
    integer :: iflagg
!-----------------------------------------------------------------------
!
    iflagg = -99
    if ( present(iflag) ) iflagg=iflag
!
    xcalc_saut=xcalc_heav(id_no,id_mait,iflagg)-xcalc_heav(id_no,id_escl,iflagg)
!
end function
