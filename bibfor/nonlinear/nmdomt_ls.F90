subroutine nmdomt_ls(ds_algopara)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/getfac.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/deprecated_algom.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algorithm management
!
! Read parameters for algorithm management - Line search
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_algopara      : datastructure for algorithm parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iter_line_maxi
    real(kind=8) :: resi_line_rela
    real(kind=8) :: reli_rho_mini, reli_rho_maxi, reli_rho_excl
    integer :: nocc
    character(len=16) :: reli_meth, keywf
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Read parameters for algorithm parameters (Line search)'
    endif
!
    keywf          = 'RECH_LINEAIRE'
    reli_meth      = 'CORDE'
    iter_line_maxi = 0
    resi_line_rela = 1.d-3
    reli_rho_mini  = 0.d0
    reli_rho_maxi  = 1.d0
    reli_rho_excl  = 0.d0
!
! - Get parameters (line search)
!
    call getfac(keywf, nocc)
    if (nocc .ne. 0) then
        ds_algopara%l_line_search = .true._1
        call getvtx(keywf, 'METHODE'       , iocc=1, scal=reli_meth)
        call getvr8(keywf, 'RESI_LINE_RELA', iocc=1, scal=resi_line_rela)
        call getvis(keywf, 'ITER_LINE_MAXI', iocc=1, scal=iter_line_maxi)
        call getvr8(keywf, 'RHO_MIN'       , iocc=1, scal=reli_rho_mini)
        call getvr8(keywf, 'RHO_MAX'       , iocc=1, scal=reli_rho_maxi)
        call getvr8(keywf, 'RHO_EXCL'      , iocc=1, scal=reli_rho_excl)
    endif
!
    ds_algopara%line_search%method    = reli_meth
    ds_algopara%line_search%resi_rela = resi_line_rela
    ds_algopara%line_search%iter_maxi = iter_line_maxi
    ds_algopara%line_search%rho_mini  = reli_rho_mini
    ds_algopara%line_search%rho_maxi  = reli_rho_maxi
    ds_algopara%line_search%rho_excl  = reli_rho_excl
!
end subroutine
