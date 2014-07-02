subroutine char_rcbp_cabl(cabl_prec, list_cabl, list_anc1, list_anc2, nb_cabl,&
                          nb_anc1, nb_anc2)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexve.h"
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
!
    character(len=8), intent(in) :: cabl_prec
    character(len=24), intent(in) :: list_cabl
    character(len=24), intent(in) :: list_anc1
    character(len=24), intent(in) :: list_anc2
    integer, intent(out) :: nb_cabl
    integer, intent(out) :: nb_anc1
    integer, intent(out) :: nb_anc2
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! RELA_CINE_BP - Get information about cables
!
! --------------------------------------------------------------------------------------------------
!
! In  cabl_prec     : prestress information from CABLE_BP
! In  list_cabl     : list of cables
! In  list_anc1     : list of first ancrages
! In  list_anc2     : list of second ancrages
! Out nb_cabl       : number of cables
! Out nb_anc1       : number of first ancrages
! Out nb_anc2       : number of second ancrages
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: table
    aster_logical :: l_para_exis
    character(len=8) :: k8bid
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_cabl = 0
    nb_anc1 = 0
    nb_anc2 = 0
!
! - Get table
!
    call ltnotb(cabl_prec, 'CABLE_BP', table)
!
! - Check table
!
    call tbexip(table, 'NUME_CABLE', l_para_exis, k8bid)
    ASSERT(l_para_exis)
    call tbexip(table, 'NOM_ANCRAGE1', l_para_exis, k8bid)
    ASSERT(l_para_exis)
    call tbexip(table, 'NOM_ANCRAGE2', l_para_exis, k8bid)
    ASSERT(l_para_exis)
!
! - Get informations in table
!
    call tbexve(table, 'NUME_CABLE', list_cabl, 'V', nb_cabl,&
                k8bid)
    call tbexve(table, 'NOM_ANCRAGE1', list_anc1, 'V', nb_anc1,&
                k8bid)
    call tbexve(table, 'NOM_ANCRAGE2', list_anc2, 'V', nb_anc2,&
                k8bid)
!
    call jedema()
end subroutine
