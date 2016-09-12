subroutine caform(cont_form)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cazouu.h"
#include "asterfort/getvtx.h"
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
    integer, intent(out) :: cont_form
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get contact formulation
!
! --------------------------------------------------------------------------------------------------
!
! Out cont_form        : formulation of contact
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: s_formul, keywf, s_algo_cont
    integer :: noc, nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
    keywf     = 'ZONE'
    cont_form = 0
!
! - Contact formulation
!
    call getvtx(' ', 'FORMULATION', scal=s_formul, nbret=noc)
    ASSERT(noc.ne.0)
!
    if (s_formul .eq. 'DISCRETE') then
        cont_form = 1
    else if (s_formul.eq.'CONTINUE') then
        call getvtx(keywf, 'ALGO_CONT', iocc=1, scal=s_algo_cont)
        if (s_algo_cont.eq.'LAC') then
            call getfac(keywf, nb_cont_zone)
            call cazouu(keywf, nb_cont_zone, 'ALGO_CONT')
            cont_form = 5
        else
            cont_form = 2
        endif
    else if (s_formul.eq.'XFEM') then
        cont_form = 3
    else if (s_formul.eq.'LIAISON_UNIL') then
        cont_form = 4
    else
        ASSERT(.false.)
    endif
!
end subroutine
