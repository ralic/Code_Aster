subroutine cbpres(load, mesh, ligrmo, ndim, vale_type)
!
    implicit   none
!
#include "asterc/getfac.h"
#include "asterfort/cafotu.h"
#include "asterfort/capres.h"
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
!
    integer, intent(in) :: ndim
    character(len=4), intent(in) :: vale_type
    character(len=8), intent(in) :: load
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: ligrmo
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Treatment of loads PRES_REP / FORCE_TUYAU
!
! --------------------------------------------------------------------------------------------------
!
!
! In  load      : load
! In  mesh      : mesh
! In  ligrmo    : model <LIGREL>
! In  ndim      : dimension of space
! In  vale_type : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbocc, ialloc
    character(len=16) :: keywordfact
!
! --------------------------------------------------------------------------------------------------
!
    ialloc = 0
!
! - PRES_REP loading
!
    keywordfact = 'PRES_REP'
    call getfac(keywordfact, nbocc)
    if (nbocc .ne. 0) then
        call capres(load, ligrmo, mesh, ndim, vale_type)
        ialloc = 1
    endif
!
! - FORCE_TUYAU loading
!
    keywordfact = 'FORCE_TUYAU'
    call getfac(keywordfact, nbocc)
    if (nbocc .ne. 0) then
        call cafotu(load, ligrmo, ialloc, mesh, vale_type)
    endif
!
end subroutine
