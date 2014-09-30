subroutine nunuco(nume_ddl, sdnuco)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/select_dof.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    character(len=24), intent(in) :: nume_ddl
    character(len=24), intent(in) :: sdnuco
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! Get position of contact dof
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_ddl : name of numbering (NUME_DDL)
! In  sdnuco   : name of datastructure to save position of contact dof
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_equa, nb_cmp
    character(len=8), pointer :: list_cmp(:) => null()
    integer, pointer :: list_equa(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Create list of components
!
    nb_cmp = 3
    AS_ALLOCATE(vk8=list_cmp, size = nb_cmp)
    list_cmp(1) = 'LAGS_C'
    list_cmp(2) = 'LAGS_F1'
    list_cmp(3) = 'LAGS_F2'
!
! - Create list of equations
!
    call dismoi('NB_EQUA', nume_ddl, 'NUME_DDL', repi=nb_equa)
    call wkvect(sdnuco, 'V V I', nb_equa, vi = list_equa)
!
! - Find components in list of equations
!
    call select_dof(list_equa, &
                    nume_ddlz = nume_ddl,&
                    nb_cmpz   = nb_cmp  , list_cmpz  = list_cmp)
!
    AS_DEALLOCATE(vk8=list_cmp)
!
end subroutine
