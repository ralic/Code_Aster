subroutine xmapin(mesh  , model , sdcont_defi, sdcont_solv, sdtime,&
                  sdstat)
!
implicit none
!          
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/xmctcg.h"                 
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: model    
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM (not HPP) method - Initial pairing and initial options
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  sdtime           : datastructure for timers
! In  sdstat           : datastructure for statistics
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_allv
!
! --------------------------------------------------------------------------------------------------
!
    l_cont_allv  = cfdisl(sdcont_defi,'ALL_VERIF')
!
! - Pairing and initial options
!
    if (.not.l_cont_allv) then
        call xmctcg(model , mesh  , sdcont_defi, sdcont_solv, sdstat,&
                    sdtime)
    endif
!
end subroutine
