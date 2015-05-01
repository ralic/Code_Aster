subroutine numer2(nb_ligr      , list_ligr, solverz      , base, nume_ddlz,&
                  nume_ddl_oldz, modelocz , sd_iden_relaz)
!
implicit none
!
#include "asterfort/detrsd.h"
#include "asterfort/idensd.h"
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
! person_in_charge: jacques.pellet at edf.fr
!
    integer, intent(in) :: nb_ligr
    character(len=24), pointer, intent(in) :: list_ligr(:)
    character(len=*), intent(in) :: solverz
    character(len=2), intent(in) :: base
    character(len=*), intent(inout) :: nume_ddlz
    character(len=*), intent(in) :: nume_ddl_oldz
    character(len=*), intent(in) :: modelocz
    character(len=*), optional, intent(in) :: sd_iden_relaz
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
! IO  nume_ddl       : name of numbering object (NUME_DDL)
! In  modeloc        : local mode for GRANDEUR numbering
! In  nume_ddl_old   : name of previous nume_ddl object
! In  sd_iden_rela   : name of object for identity relations between dof
!
! If nume_ddl_old is present
!   -> try to know if PROF_CHNO in nume_ddl_old can be reuse
!      In this case nume_ddl = nume_ddl_old
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: prof_chno, prof_chno_old
    character(len=19) :: solver
    character(len=14) :: nume_ddl , nume_ddl_old, moloc
    character(len=24) :: renum, sd_iden_rela
    logical :: l_matr_dist
    character(len=24), pointer :: slvk(:) => null()
    character(len=24), pointer :: nslv(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    solver        = solverz
    nume_ddl      = nume_ddlz
    moloc         = modelocz
    nume_ddl_old  = nume_ddl_oldz
    prof_chno     = nume_ddl//'.NUME'
    prof_chno_old = nume_ddl_old//'.NUME'
!
    call detrsd('NUME_DDL', nume_ddl)
!
! - Identity relations between dof
!
    sd_iden_rela = ' '
    if (present(sd_iden_relaz)) then
        sd_iden_rela = sd_iden_relaz
    endif
!
! - Method for renumbering equation
!
    call jeveuo(solver//'.SLVK', 'L', vk24=slvk)
    renum       = slvk(4)
    l_matr_dist = slvk(10) .eq. 'OUI'
!
! - Create NUME_EQUA objects
!
    call nueffe(nb_ligr, list_ligr, base, nume_ddl, renum,&
                solver , modelocz = moloc, sd_iden_relaz = sd_iden_rela)
!
! - Create NUML_EQUA objects
!
    if (l_matr_dist) then
        call nugllo(nume_ddlz, base, solver)
    endif
!
! - Trying to reuse old nume_ddl
!
    if (nume_ddl_old.ne.' ') then
        if (idensd('PROF_CHNO', prof_chno, prof_chno_old)) then
            call detrsd('NUME_DDL', nume_ddl)
            call jedupo(nume_ddl//'     .ADNE', 'V', nume_ddl_old//'     .ADNE', .false._1)
            call jedupo(nume_ddl//'     .ADLI', 'V', nume_ddl_old//'     .ADLI', .false._1)
            call jedetr(nume_ddl//'     .ADLI')
            call jedetr(nume_ddl//'     .ADNE')
            nume_ddl = nume_ddl_old
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
