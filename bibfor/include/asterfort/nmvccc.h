!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine nmvccc(model    , nbin     , nbout    , lpain    , lchin,&
                      lpaout   , lchout   , exis_temp, exis_hydr, exis_ptot,&
                      exis_sech, exis_epsa, calc_meta, vect_elem)
        character(len=8) :: model
        integer, intent(in) :: nbout
        integer, intent(in) :: nbin
        character(len=8), intent(in) :: lpain(nbin)
        character(len=19), intent(in) :: lchin(nbin)
        character(len=8), intent(in) :: lpaout(nbout)
        character(len=19), intent(inout) :: lchout(nbout)
        aster_logical, intent(in) :: exis_temp
        aster_logical, intent(in) :: exis_hydr
        aster_logical, intent(in) :: exis_ptot
        aster_logical, intent(in) :: exis_sech
        aster_logical, intent(in) :: exis_epsa
        aster_logical, intent(in) :: calc_meta
        character(len=19), intent(in) :: vect_elem
    end subroutine nmvccc
end interface
