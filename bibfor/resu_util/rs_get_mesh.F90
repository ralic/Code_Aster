subroutine rs_get_mesh(result_, mesh)
!
implicit none
!
#include "asterfort/rs_get_model.h"
#include "asterfort/dismoi.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: result_
    character(len=8), intent(out) :: mesh
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get mesh in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! Out mesh             : name of mesh
!                        
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result, model
    integer :: nume, codret
!
! --------------------------------------------------------------------------------------------------
!
    result    = result_
!
! - Get model
!
    nume = 1
    call rs_get_model(result, nume, model, codret)
!
! - Get mesh
!
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
end subroutine
