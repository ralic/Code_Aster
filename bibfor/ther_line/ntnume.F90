subroutine ntnume(model, list_load, result, nume_dof)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/gnomsd.h"
#include "asterfort/numero.h"
#include "asterfort/rsnume.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: model
    character(len=19), intent(in) :: list_load
    character(len=8), intent(in) :: result
    character(len=24), intent(out) :: nume_dof
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Initializations
!
! Create numbering
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  list_load        : name of datastructure for list of loads
! In  result           : name of result datastructure (EVOL_THER)
! Out nume_dof         : name of numbering object (NUME_DDL)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: nuposs
    character(len=24) :: noojb
!
! --------------------------------------------------------------------------------------------------
!
    nume_dof = '12345678.NUMED'
    noojb    = '12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    nume_dof = noojb(1:14)
    call rsnume(result, 'TEMP', nuposs)
    call numero(nume_dof, 'VG',&
                old_nume_ddlz = nuposs,&
                modelz = model , list_loadz = list_load)
!
end subroutine
