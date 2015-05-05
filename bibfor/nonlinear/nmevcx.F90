subroutine nmevcx(sddisc      , nume_inst, sdcont_defi, sdcont_solv, i_echec,&
                  i_echec_acti)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/nmevcc.h"
#include "asterfort/nmevco.h"
    character(len=24) :: sdcont_defi, sdcont_solv
    integer :: i_echec, i_echec_acti, nume_inst
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! DETECTION DE L'EVENEMENT COLLISION
!
! ----------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! IN  NUMINS : NUMERO D'INSTANT
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  IECHEC : OCCURRENCE DE L'ECHEC
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
!                   0 SINON
!
! ----------------------------------------------------------------------
!
    aster_logical :: l_cont_cont, l_cont_disc
!
! ----------------------------------------------------------------------
!
    l_cont_cont  = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc  = cfdisl(sdcont_defi,'FORMUL_DISCRETE')
    i_echec_acti = 0
!
    if (l_cont_disc) then
        call nmevco(sddisc, nume_inst, sdcont_solv, i_echec    , i_echec_acti)
    else if (l_cont_cont) then
        call nmevcc(sddisc, nume_inst, sdcont_defi, sdcont_solv, i_echec,&
                    i_echec_acti)
    else
        ASSERT(.false.)
    endif
!
end subroutine
