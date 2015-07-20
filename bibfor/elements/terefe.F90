subroutine terefe(refe_name, type_elem, refe_vale)
!
implicit none
!
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: refe_name
    character(len=*), intent(in) :: type_elem
    real(kind=8), intent(out) :: refe_vale
!
! --------------------------------------------------------------------------------------------------
!
! Get *_REFE value for REFE_FORC_NODA option
!
! --------------------------------------------------------------------------------------------------
!
! In  refe_name        : name of reference
! In  type_elem        : type of element
! Out refe_vale        : value of reference
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jvrefe, index
    real(kind=8) :: val
    character(len=16) :: kmess(2)
!
! --------------------------------------------------------------------------------------------------
!
    call jevech('PREFCO', 'L', jvrefe)
    refe_vale = r8nnem()
!
! - Get index of value in element catalog
!
    if (refe_name .eq. 'SIGM_REFE') then
        if (type_elem .eq. 'MECA_ISO') then
            index = 1
        else if (type_elem.eq.'THM_JOINT') then
            index = 1
        else if (type_elem.eq.'MECA_INTERFACE') then
            index = 1
        else if (type_elem.eq.'MECA_COQUE3D') then
            index = 1
        else if (type_elem.eq.'MECA_GRADVARI') then
            index = 1
        else if (type_elem.eq.'MECA_TUYAU') then
            index = 1
        else if (type_elem.eq.'THM') then
            index = 1
        else if (type_elem.eq.'MECA_INCO') then
            index = 1
        else if (type_elem.eq.'MECA_CG') then
            index = 1
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'EPSI_REFE') then
        if (type_elem .eq. 'MECA_INCO') then
            index = 2
        else if (type_elem.eq.'GRILLE') then
            index = 1
        else if (type_elem.eq.'MEMBRANE') then
            index = 1
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'FLUX_THER_REFE') then
        if (type_elem .eq. 'THM') then
            index = 4
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'FLUX_HYD1_REFE') then
        if (type_elem .eq. 'THM_JOINT') then
            index = 2
        else if (type_elem.eq.'THM') then
            index = 2
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'FLUX_HYD2_REFE') then
        if (type_elem .eq. 'THM') then
            index = 3
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'VARI_REFE') then
        if (type_elem .eq. 'MECA_GRADVARI') then
            index = 2
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'EFFORT_REFE') then
        if (type_elem .eq. 'MECA_DISCRET') then
            index = 1
        else if (type_elem.eq.'MECA_BARRE') then
            index = 1
        else if (type_elem.eq.'MECA_CABLE') then
            index = 1
        else if (type_elem.eq.'MECA_POULIE') then
            index = 1
        else if (type_elem.eq.'MECA_POUTRE') then
            index = 1
        else if (type_elem.eq.'MECA_COQUE') then
            index = 1
        else if (type_elem.eq.'MECA_CG') then
            index = 2
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'MOMENT_REFE') then
        if (type_elem .eq. 'MECA_DISCRET') then
            index = 2
        else if (type_elem.eq.'MECA_POUTRE') then
            index = 2
        else if (type_elem.eq.'MECA_COQUE') then
            index = 2
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'DEPL_REFE') then
        if (type_elem .eq. 'MECA_INTERFACE') then
            index = 2
        else if (type_elem.eq.'MECA_CG') then
            index = 4
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'LAGR_REFE') then
        if (type_elem .eq. 'MECA_GRADVARI') then
            index = 3
        else
            ASSERT(.false.)
        endif
    else if (refe_name.eq.'PI_REFE') then
        if (type_elem .eq. 'MECA_INCO') then
            index = 3
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Get and check value
!
    val = zr(jvrefe+index-1)
    if (isnan(val)) then
        kmess(1) = type_elem
        kmess(2) = refe_name
        call utmess('F', 'MECANONLINE5_55', nk=2, valk=kmess)
    else
        refe_vale = val
    endif
!
end subroutine
