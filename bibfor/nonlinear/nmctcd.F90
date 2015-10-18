subroutine nmctcd(list_func_acti, ds_contact, nume_dof, hval_veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmchex.h"
#include "asterfort/cffoco.h"
#include "asterfort/cffofr.h"
#include "asterfort/cufoco.h"
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
    integer, intent(in) :: list_func_acti(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: hval_veasse(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Compute
!
! Compute vectors for DISCRETE contact
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  nume_dof         : name of numbering (NUME_DDL)
! In  ds_contact       : datastructure for contact management
! In  hval_veasse      : hat-variable for vectors (node fields)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_unil, l_cont_disc, l_frot_disc, l_all_verif
    aster_logical :: l_cont_pena
    character(len=24) :: vect_asse
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! - Active functionnalites
!
    l_cont_disc = isfonc(list_func_acti        , 'CONT_DISCRET')
    l_frot_disc = isfonc(list_func_acti        , 'FROT_DISCRET')
    l_cont_pena = cfdisl(ds_contact%sdcont_defi, 'CONT_PENA')
    l_unil      = isfonc(list_func_acti        , 'LIAISON_UNILATER')
    l_all_verif = cfdisl(ds_contact%sdcont_defi, 'ALL_VERIF')
    if (.not.l_all_verif) then
!
! ----- Print
!
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL FORCES CONTACT'
        endif
!
! ----- Contact (DISCRETE) forces
!
        if (l_cont_disc) then
            call nmchex(hval_veasse, 'VEASSE', 'CNCTDC', vect_asse)
            call cffoco(nume_dof, ds_contact%sdcont_solv, vect_asse)
        endif
!
! ----- Friction (DISCRETE) forces
!
        if ((l_frot_disc) .or. (l_cont_pena)) then
            call nmchex(hval_veasse, 'VEASSE', 'CNCTDF', vect_asse)
            call cffofr(nume_dof, ds_contact%sdcont_solv, vect_asse)
        endif
!
! ----- Unilateral conditions (DISCRETE) forces
!
        if (l_unil) then
            call nmchex(hval_veasse, 'VEASSE', 'CNUNIL', vect_asse)
            call cufoco(nume_dof, ds_contact%sdunil_solv, vect_asse)
        endif
    endif
!
end subroutine
