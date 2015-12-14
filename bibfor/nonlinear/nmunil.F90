subroutine nmunil(mesh  , disp_curr, disp_iter, solver    , matr_asse,&
                  cncine, iter_newt, time_curr, ds_contact, ctccvg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/algocu.h"
#include "asterfort/assert.h"
#include "asterfort/cuprep.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdsc3.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: disp_curr
    character(len=19), intent(in) :: disp_iter
    character(len=19), intent(in) :: solver
    character(len=19), intent(in) :: matr_asse
    character(len=19), intent(in) :: cncine
    integer, intent(in) :: iter_newt
    real(kind=8), intent(in) :: time_curr
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(out) :: ctccvg
!
! --------------------------------------------------------------------------------------------------
!
! Unilateral constraint - Solve
!
! Solve
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  disp_curr        : current displacements
! In  disp_iter        : displacement iteration
! In  solver           : datastructure for solver parameters
! In  matr_asse        : matrix
! In  cncine           : void load for kinematic loads
! In  iter_newt        : index of current Newton iteration
! In  time_curr        : current time
! IO  ds_contact       : datastructure for contact management
! Out ctccvg           : output code for contact algorithm
!                        -1 - No solving
!                         0 - OK
!                        +1 - Maximum contact iteration
!                        +2 - Singular contact matrix
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: sdcond_matr
    integer :: ldscon, lmat
    integer :: ifm, niv, nb_equa
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> DEBUT DU TRAITEMENT DES CONDITIONS UNILATERALes'
    endif
!
! - Get "contact" matrix
!
    sdcond_matr = ds_contact%sdunil_solv(1:14)//'.MATC'
    call mtdsc3(sdcond_matr)
    call jeveuo(sdcond_matr(1:19)//'.&INT', 'E', ldscon)
!
! - Initializations
!
    ctccvg = -1
!
! - Get matrix descripor
!
    call jeveuo(matr_asse//'.&INT', 'E', lmat)
!
! - Prepare unilateral constraints
!
    if (iter_newt .eq. 0) then
        nb_equa = zi(lmat+2)
        call cuprep(mesh, nb_equa, ds_contact, disp_curr, time_curr)
    endif
!
! - Solve
!
    call algocu(ds_contact, solver, lmat, ldscon, cncine,&
                disp_iter , ctccvg)
!
! - Yes for computation
!
    ASSERT(ctccvg.ge.0)
!
end subroutine
