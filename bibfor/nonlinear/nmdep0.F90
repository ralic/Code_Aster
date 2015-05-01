subroutine nmdep0(oper, solalg)
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
#include "asterfort/assert.h"
#include "asterfort/nmchso.h"
    character(len=3) :: oper
    character(len=19) :: solalg(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! MISE A ZERO DE DEPDEL
!
! ----------------------------------------------------------------------
!
!
! IN  OPER   : TYPE OPERATION
!               ON  - ON MET &&CNPART.ZERO
!               OFF - ON MET DEPDEL
! I/O SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    character(len=19) :: depde0, depdel
!
! ----------------------------------------------------------------------
!
    if (oper .eq. 'ON ') then
        depde0 = '&&CNPART.ZERO'
        call nmchso(solalg, 'SOLALG', 'DEPDEL', depde0, solalg)
    else if (oper.eq.'OFF') then
        depdel = '&&NMCH2P.DEPDEL'
        call nmchso(solalg, 'SOLALG', 'DEPDEL', depdel, solalg)
    else
        ASSERT(.false.)
    endif
!
end subroutine
