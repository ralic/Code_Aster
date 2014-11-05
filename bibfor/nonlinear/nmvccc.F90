subroutine nmvccc(model    , nbin     , nbout    , lpain    , lchin,&
                  lpaout   , lchout   , exis_temp, exis_hydr, exis_ptot,&
                  exis_sech, exis_epsa, calc_meta, vect_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/infdbg.h"
#include "asterfort/reajre.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
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
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Command variables - Compute elementary vectors
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  nbin           : number of input fields
! In  nbout          : number of output fields
! In  lpain          : list of input field parameters
! In  lchin          : list of input fields
! In  lpaout         : list of output field parameters
! IO  lchout         : list of output fields
! In  exis_temp      : .true. if temperature variable command exists
! In  exis_hydr      : .true. if hydratation variable command exists
! In  exis_ptot      : .true. if total pressure (THM) variable command exists
! In  exis_sech      : .true. if drying variable command exists
! In  exis_epsa      : .true. if non-elastic strain variable command exists
! In  calc_meta      : .true. to compute metallurgy variable command
! In  vect_elem      : name of elementary vectors
!
! --------------------------------------------------------------------------------------------------
!
    character(len=6) :: masque
    character(len=16) :: option
    character(len=24) :: ligrmo
    integer :: nbr
!
! --------------------------------------------------------------------------------------------------
!
    nbr    = 0
    masque = '.VEXXX'
    ligrmo = model(1:8)//'.MODELE'
!
! - Temperature
!
    if (exis_temp) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vect_elem(1:8)//masque
        option = 'CHAR_MECA_TEMP_R'
        call calcul('C'  , option, ligrmo, nbin  , lchin,&
                    lpain, nbout , lchout, lpaout, 'V'  ,&
                    'OUI')
        call reajre(vect_elem, lchout(1), 'V')
    endif
!
! - Hydratation
!
    if (exis_hydr) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vect_elem(1:8)// masque
        option = 'CHAR_MECA_HYDR_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vect_elem, lchout(1), 'V')
    endif
!
! - Total pressure (THM)
!
    if (exis_ptot) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vect_elem(1:8)// masque
        option = 'CHAR_MECA_PTOT_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vect_elem, lchout(1), 'V')
    endif
!
! - Drying
!
    if (exis_sech) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vect_elem(1:8)// masque
        option = 'CHAR_MECA_SECH_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vect_elem, lchout(1), 'V')
    endif
!
! - Non-elastic strain
!
    if (exis_epsa) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vect_elem(1:8)// masque
        option = 'CHAR_MECA_EPSA_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vect_elem, lchout(1), 'V')
    endif
!
! - Metallurgy
!
    if (calc_meta) then
        nbr = 6
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vect_elem(1:8)// masque
        option = 'CHAR_MECA_META_Z'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vect_elem, lchout(1), 'V')
    endif
!
end subroutine
