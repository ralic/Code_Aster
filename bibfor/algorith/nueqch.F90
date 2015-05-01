subroutine nueqch(error, chamno, nume_node, cmp_name, nueq)
!
implicit none
!
#include "asterc/ismaem.h"
#include "asterfort/select_dof.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: chamno
    character(len=1), intent(in) :: error
    integer, intent(in) :: nume_node
    character(len=8), intent(in) :: cmp_name
    integer, intent(inout) :: nueq
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! PERMET DE RECUPERER LES NUMEROS DES EQUATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAMNO  : CHAM_NO A MODIFIER
! IN  ERREUR  : 'F' SI UNE COMPOSANTE ABSENTE -> ERREUR
!               'A' SI UNE COMPOSANTE ABSENTE -> ALARME
!               ' ' SI UNE COMPOSANTE ABSENTE -> RIEN
!
! --------------------------------------------------------------------------------------------------
!
    integer, pointer :: list_idx_dof(:) => null()
    integer, pointer :: list_node_p(:) => null()
    character(len=8), pointer :: list_cmp_p(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    AS_ALLOCATE(vi = list_idx_dof, size = 1)
    AS_ALLOCATE(vi = list_node_p, size = 1)
    AS_ALLOCATE(vk8 = list_cmp_p, size = 1)
!
    list_node_p(1) = nume_node
    list_cmp_p(1)  = cmp_name
!
! - Find component in list of equations
!
    call select_dof(list_idx_dof = list_idx_dof, &
                    chamnoz      = chamno,&
                    nb_nodez     = 1  , list_nodez = list_node_p,&
                    nb_cmpz      = 1  , list_cmpz  = list_cmp_p)
!
! - Check
!
    if (list_idx_dof(1).eq.0) then
        call utmess(error, 'MECANONLINE5_50', sk = cmp_name)
    endif
!
! - Copy
!
    nueq = list_idx_dof(1)
!
    AS_DEALLOCATE(vi = list_idx_dof)
    AS_DEALLOCATE(vi = list_node_p)
    AS_DEALLOCATE(vk8 = list_cmp_p)
!
end subroutine
