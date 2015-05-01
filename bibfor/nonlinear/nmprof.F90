subroutine nmprof(model        , result, list_load, solver, nume_ddl,&
                  sd_iden_relaz)
!
implicit none
!
#include "asterfort/gnomsd.h"
#include "asterfort/numero.h"
#include "asterfort/rsnume.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(out) :: nume_ddl
    character(len=8), intent(in) :: result
    character(len=19), intent(in) :: list_load
    character(len=19), intent(in) :: solver
    character(len=*), optional, intent(in) :: sd_iden_relaz
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initializations
!
! Create numbering
!
! --------------------------------------------------------------------------------------------------
!
! Out nume_ddl       : name of numbering object (NUME_DDL)
! In  solver         : name of solver datastructure
! In  result         : name of result datastructure (EVOL_NOLI)
! In  model          : name of model datastructure
! In  list_load      : list of loads
! In  sd_iden_rela   : name of object for identity relations between dof
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: nuposs
    character(len=24) :: noojb, sd_iden_rela
!
! --------------------------------------------------------------------------------------------------
!
    sd_iden_rela = ' '
    if (present(sd_iden_relaz)) then
        sd_iden_rela = sd_iden_relaz
    endif
!
! - Generate name of numbering object (nume_ddl)
!
    nume_ddl = '12345678.NUMED'
    noojb    = '12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    nume_ddl = noojb(1:14)
    call rsnume(result, 'DEPL', nuposs)
!
! - Create numbering
!
    call numero(nume_ddl, solver, 'VG',&
                old_nume_ddlz = nuposs,&
                modelz = model , list_loadz = list_load,&
                sd_iden_relaz = sd_iden_rela)
!
end subroutine
