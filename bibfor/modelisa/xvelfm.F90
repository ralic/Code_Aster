subroutine xvelfm(nb_cracks, cracks, model_xfem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/utmess.h"
#include "asterfort/xvfimo.h"
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
!
    character(len=8), intent(in) :: model_xfem
    integer, intent(in) :: nb_cracks
    character(len=8), intent(in) :: cracks(nb_cracks)
!
! --------------------------------------------------------------------------------------------------
!
! XFEM (Loads)
!
! Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  model_xfem     : name of XFEM model
! In  nb_cracks      : number of cracks
! In  cracks         : list of cracks
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_crack, iret
    aster_logical :: ltrouv
    character(len=8) :: valk(2)
    character(len=16) :: typdis
!
! --------------------------------------------------------------------------------------------------
!
!
! - XFEM model checking
!
    call exixfe(model_xfem, iret)
    if (iret .eq. 0) then
        call utmess('F', 'XFEM_72', sk=model_xfem)
    endif
!
! - Loop on cracks
!
    do i_crack = 1, nb_cracks
!
! ----- Crack in model ?
!
        ltrouv = xvfimo(model_xfem,cracks(i_crack))
        if (.not.ltrouv) then
            valk(1)=cracks(i_crack)
            valk(2)=model_xfem
            call utmess('F', 'XFEM_73', nk=2, valk=valk)
        endif
!
! ----- No PRES_REP on lips if CZM
!
        call dismoi('TYPE_DISCONTINUITE', cracks(i_crack), 'FISS_XFEM', repk=typdis)
        if (typdis .eq. 'COHESIF') then
            call utmess('F', 'XFEM2_7', sk=cracks(i_crack))
        endif
    end do
!
end subroutine
