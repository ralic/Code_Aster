subroutine numer2(nb_ligr      , list_ligr    , solverz, base, nume_ddlz,&
                  old_nume_ddlz, modelocz)
!
implicit none
!
#include "asterfort/detrsd.h"
#include "asterfort/idenob.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nueffe.h"
#include "asterfort/nugllo.h"
#include "asterfort/promor.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!
    integer, intent(in) :: nb_ligr
    character(len=24), pointer, intent(in) :: list_ligr(:)
    character(len=*), intent(in) :: solverz
    character(len=2), intent(in) :: base
    character(len=*), intent(inout) :: nume_ddlz
    character(len=*), intent(in) :: old_nume_ddlz
    character(len=*), intent(in) :: modelocz
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Numbering - Create objects
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_ligr        : number of LIGREL in list
! In  list_ligr      : pointer to list of LIGREL
! In  base           : JEVEUX base to create objects
!                      base(1:1) => PROF_CHNO objects
!                      base(2:2) => NUME_DDL objects
! In  solver         : name of solver datastructure
! IO  nume_ddl       : name of nume_ddl object
! In  modelocz       : local mode for GRANDEUR numbering
! In  old_nume_ddl   : name of previous nume_ddl object
!
! If old_nume_ddl is present
!   -> try to know if PROF_CHNO in old_nume_ddl can be reuse
!      In this case nume_ddl = old_nume_ddl
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l1, l2, l3, l4
    character(len=19) :: solver
    character(len=14) :: nume_ddl , old_nume_ddl, moloc
    character(len=24) :: renum
    character(len=24), pointer :: slvk(:) => null()
    character(len=24), pointer :: nslv(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    solver       = solverz
    nume_ddl     = nume_ddlz
    moloc        = modelocz
    old_nume_ddl = old_nume_ddlz
!
    call detrsd('NUME_DDL', nume_ddl)
!
! - Method for renumbering equation
!
    call jeveuo(solver//'.SLVK', 'L', vk24=slvk)
    renum = slvk(4)
!
! - Create NUME_EQUA objects
!
    call nueffe(nb_ligr, list_ligr, base, nume_ddl, renum,&
                solver , modelocz = moloc)
!
! - Create NUML_EQUA objects
!
    if (slvk(10)(1:3) .eq. 'OUI') then
        call nugllo(nume_ddlz, base, solver)
    endif
!
! - Trying to reuse old nume_ddl
!
    if (old_nume_ddl.ne.' ') then
!
! ----- Is same PROF_CHNO ?
!
        l1=idenob(nume_ddl//'.NUME.DEEQ',old_nume_ddl//'.NUME.DEEQ')
        l2=idenob(nume_ddl//'.NUME.LILI',old_nume_ddl//'.NUME.LILI')
        l3=idenob(nume_ddl//'.NUME.NUEQ',old_nume_ddl//'.NUME.NUEQ')
        l4=idenob(nume_ddl//'.NUME.PRNO',old_nume_ddl//'.NUME.PRNO')
!
! ----- Yes -> using old one
!
        if (l1 .and. l2 .and. l3 .and. l4) then
            call detrsd('NUME_DDL', nume_ddl)
            call jedupo(nume_ddlz//'     .ADNE', 'V', old_nume_ddl//'     .ADNE', .false._1)
            call jedupo(nume_ddlz//'     .ADLI', 'V', old_nume_ddl//'     .ADLI', .false._1)
            call jedetr(nume_ddlz//'     .ADLI')
            call jedetr(nume_ddlz//'     .ADNE')
            nume_ddl = old_nume_ddl
        endif
    endif
!
! - Create matrix topology
!
    call promor(nume_ddl, base(1:1))
!
! - Create NSLV object
!
    call jedetr(nume_ddl//'.NSLV')
    call wkvect(nume_ddl//'.NSLV', base(1:1)//' V K24', 1, vk24 = nslv)
    nslv(1)=solver
!
! - Cleaning
!
    call jedetr(nume_ddl//'     .ADLI')
    call jedetr(nume_ddl//'     .ADNE')
!
    nume_ddlz = nume_ddl
!
!     CALL CHEKSD('sd_nume_ddl',nume_ddlz,IRET)
!
    call jedema()
end subroutine
