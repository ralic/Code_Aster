subroutine calc_coor_elga(ligrel, chgeom, chgaus)
!
implicit none
!
#include "asterfort/calcul.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=19), intent(in) :: ligrel
    character(len=19), intent(in) :: chgeom
    character(len=19), intent(in) :: chgaus
!
! --------------------------------------------------------------------------------------------------
!
! Compute <CARTE> with informations on Gauss points 
!
! --------------------------------------------------------------------------------------------------
!
! In  ligrel     : list of elements where computing
! In  chgeom     : name of <CARTE> for geometry
! In  chgaus     : name of <CARTE> with informations on Gauss points 
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: lpain(1), lpaout(1)
    character(len=16) :: option
    character(len=19) :: lchin(1), lchout(1)
    integer :: nbchin
!
! --------------------------------------------------------------------------------------------------
!
    lpain(1)  = 'PGEOMER'
    lchin(1)  = chgeom
    lpaout(1) = 'PCOORPG'
    lchout(1) = chgaus
    option    = 'COOR_ELGA'
    nbchin    = 1
!
    call calcul('S', option, ligrel, nbchin, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
end subroutine
