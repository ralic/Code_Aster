subroutine nxdomt(ds_algopara, ds_algorom)
!
use NonLin_Datastructure_type
use Rom_Datastructure_type
!
implicit none
!
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romAlgoNLRead.h"
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
    type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
!
! --------------------------------------------------------------------------------------------------
!
! THER_NON_LINE - Algorithm management
!
! Read parameters for algorithm management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_algopara      : datastructure for algorithm parameters
! IO  ds_algorom       : datastructure for ROM parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: reac_iter, iter_line_maxi
    real(kind=8) :: resi_line_rela
    character(len=16) :: keywf, algo_meth
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> . Read parameters for algorithm parameters'
    endif
!
! - Initializations
!
    algo_meth      = ' '
    reac_iter      = 0
    iter_line_maxi = 0
    resi_line_rela = 1.d-3
!
! - Get method
!
    call getvtx(' ', 'METHODE', scal=algo_meth)
    ds_algopara%method = algo_meth
!
! - Get parameters of method
!
    if (algo_meth .eq. 'NEWTON') then
        keywf = 'NEWTON'
        call getvis(keywf, 'REAC_ITER', iocc=1, scal=reac_iter)
        ASSERT(reac_iter .ge. 0)
        ds_algopara%reac_iter = reac_iter
        call getvr8(keywf, 'RESI_LINE_RELA', iocc=1, scal=resi_line_rela)
        call getvis(keywf, 'ITER_LINE_MAXI', iocc=1, scal=iter_line_maxi)
        ds_algopara%line_search%resi_rela = resi_line_rela
        ds_algopara%line_search%iter_maxi = iter_line_maxi
    else if (algo_meth .eq. 'MODELE_REDUIT') then
        keywf = 'MODELE_REDUIT'
        call getvis(keywf, 'REAC_ITER', iocc=1, scal=reac_iter)
        ASSERT(reac_iter .ge. 0)
        ds_algopara%reac_iter = reac_iter
        call romAlgoNLRead(ds_algorom)
    else
        ASSERT(.false.)
    endif
!
end subroutine
