subroutine nmcrdd(meshz, modelz, sd_inout, sd_suiv)
!
implicit none
!
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/nmcrdn.h"
#include "asterfort/nmextr.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: meshz
    character(len=*), intent(in) :: modelz
    character(len=24), intent(in) :: sd_inout
    character(len=24), intent(out) :: sd_suiv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Create dof monitor datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  result           : name of results datastructure
! In  sddisc           : datastructure for discretization
! in  sd_inout         : datastructure for input/output parameters
! Out sd_suiv          : datastructure for dof monitor parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_suiv, nbocc, nume_reuse
    character(len=16) :: keyw_fact
!
! --------------------------------------------------------------------------------------------------
!
    nb_suiv    = 0
    sd_suiv    = '&&NMCRDD.OBSV'
    nume_reuse = 0
    keyw_fact  = 'SUIVI_DDL'
    call getfac(keyw_fact, nbocc)
    ASSERT(nbocc.le.99)
!
! - Read datas for extraction
!
    call nmextr(meshz, modelz    , sd_suiv , sd_inout, keyw_fact,&
                nbocc, nume_reuse, nb_suiv)
!
! - Read name of columns
!
    if (nbocc .ne. 0) then
        call nmcrdn(sd_suiv, keyw_fact, nb_suiv, nbocc)
    endif
!
end subroutine
