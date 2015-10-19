subroutine numer3(modelz, list_loadz, nume_ddlz, sd_iden_relaz)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/idenob.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/numero.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: modelz
    character(len=*), intent(inout) :: nume_ddlz
    character(len=*), intent(in) :: list_loadz
    character(len=*), intent(in) :: sd_iden_relaz
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! (Re)-Numbering - Used for variable element topology (contact)
!
! --------------------------------------------------------------------------------------------------
!
! IO  nume_ddl       : name of numbering object (NUME_DDL)
! In  model          : name of model datastructure
! In  list_load      : list of loads
! In  sd_iden_rela   : name of object for identity relations between dof
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: nume_ddl_old, nume_ddl_save
!
! --------------------------------------------------------------------------------------------------
!
    call infmue()
    nume_ddl_old  = nume_ddlz
    nume_ddl_save = '&&NUMER3.NUAV'
    call copisd('NUME_DDL', 'V', nume_ddlz, nume_ddl_save)
    call detrsd('NUME_DDL', nume_ddlz)
!
! - Numbering
!
    call numero(nume_ddlz, 'VG',&
                modelz = modelz , list_loadz = list_loadz,&
                sd_iden_relaz = sd_iden_relaz)
!
! - Same equations ! The re-numbering works only with MUMPS/MULT_FRONT/PETSc, not with LDLT
!
    ASSERT(idenob(nume_ddl_old//'.NUME.DEEQ',nume_ddl_save//'.NUME.DEEQ'))
!
    call detrsd('NUME_DDL', nume_ddl_save)
    call infbav()
!
end subroutine
