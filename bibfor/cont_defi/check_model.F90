subroutine check_model(mesh, model, cont_form)
!
implicit none
!
#include "asterfort/exixfe.h"
#include "asterfort/exipat.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    integer, intent(in) :: cont_form
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Check if exist XFEM in model or PATCH in mesh
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  cont_form        : formulation of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
!
! - Check if exist XFEM in model
!
    if (cont_form .eq. 3) then
        call exixfe(model, iret)
        if (iret .eq. 0) then
            call utmess('F', 'XFEM2_8', sk=model)
        endif
    endif
!
! - Check if exist PATCH in mesh (LAC method)
!
    if (cont_form .eq. 5) then
        call exipat(mesh, iret)
        if (iret .eq. 0) then
            call utmess('F', 'CONTACT4_2', sk=mesh)
        endif
    endif  
!
end subroutine
 
