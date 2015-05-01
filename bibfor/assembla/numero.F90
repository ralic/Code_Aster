subroutine numero(nume_ddlz    , solverz     , base,&
                  old_nume_ddlz, modelocz    ,&
                  modelz       , list_loadz  ,&
                  nb_matr_elem , list_matr_elem,&
                  sd_iden_relaz)
!
implicit none
!
#include "asterfort/as_deallocate.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/numer2.h"
#include "asterfort/numcch.h"
#include "asterfort/numoch.h"
#include "asterfort/uttcpu.h"
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
! ----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=*), intent(in) :: solverz
    character(len=*), intent(inout) :: nume_ddlz
    character(len=2), intent(in) :: base
    character(len=*), optional, intent(in) :: modelz
    character(len=*), optional, intent(in) :: list_loadz
    character(len=24), optional, intent(in) :: list_matr_elem(*)
    integer, optional, intent(in) :: nb_matr_elem
    character(len=*), optional, intent(in) :: old_nume_ddlz
    character(len=*), optional, intent(in) :: modelocz
    character(len=*), optional, intent(in) :: sd_iden_relaz
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Numbering
!
! --------------------------------------------------------------------------------------------------
!
! IO  nume_ddl       : name of numbering object (NUME_DDL)
! In  solver         : name of solver datastructure
! In  base           : JEVEUX base to create objects
!                      base(1:1) => PROF_CHNO objects
!                      base(2:2) => NUME_DDL objects
! In  old_nume_ddl   : name of previous nume_ddl object
! In  modelocz       : local mode for GRANDEUR numbering
! In  model          : name of model
! In  list_load      : list of loads
! In  list_matr_elem : list of elementary matrixes
! In  nb_matr_elem   : number of elementary matrixes
! In  sd_iden_rela   : name of object for identity relations between dof
!
! If old_nume_ddl is present
!   -> try to know if PROF_CHNO in old_nume_ddl can be reuse
!      In this case nume_ddl = old_nume_ddl
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_ligr
    character(len=24) :: modeloc, old_nume_ddl
    character(len=24), pointer :: list_ligr(:) => null()
    character(len=24) :: sd_iden_rela
!
! --------------------------------------------------------------------------------------------------
!
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.2', 'DEBUT', ' ')
!
! - Identity relations between dof
!
    sd_iden_rela = ' '
    if (present(sd_iden_relaz)) then
        sd_iden_rela = sd_iden_relaz
    endif
!
! - Local mode
!
    modeloc = ' '
    if (present(modelocz)) then
        modeloc = modelocz
    endif
    old_nume_ddl = ' '
    if (present(old_nume_ddlz)) then
        old_nume_ddl = old_nume_ddlz
    endif
!
! - Create list of LIGREL for numbering
!
    if (present(list_matr_elem)) then
        call numoch(list_matr_elem, nb_matr_elem, list_ligr, nb_ligr)
    else
        call numcch(modelz, list_loadz, list_ligr, nb_ligr)
    endif
!
! - Create numbering
!
    call numer2(nb_ligr     , list_ligr, solverz     , base, nume_ddlz,&
                old_nume_ddl, modeloc  , sd_iden_rela)
!
    AS_DEALLOCATE(vk24 = list_ligr)
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.2', 'FIN', ' ')
!
end subroutine
