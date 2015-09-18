subroutine nmdomt(ds_algopara)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/getexm.h"
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
! Read parameters for algorithm management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_algopara      : datastructure for algorithm parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: reac_incr, reac_iter, reac_iter_elas
    real(kind=8) :: pas_mini_elas
    integer :: iret
    character(len=16) :: keywf, algo_meth, matrix_pred, matrix_corr, answer
    character(len=8) :: result_prev_disp
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Read parameters for algorithm parameters'
    endif
!
! - Get method
!
    if (getexm(' ','METHODE') .eq. 1) then
        call getvtx(' ', 'METHODE', scal=algo_meth)
    else
        algo_meth = 'NEWTON'
    endif
    ds_algopara%method = algo_meth
!
! - Get parameters of method
!
    keywf = 'NEWTON'
    if ((algo_meth .eq. 'NEWTON') .or. (algo_meth .eq. 'NEWTON_KRYLOV')) then
        call getvtx(keywf, 'MATRICE'   , iocc=1, scal=matrix_corr)
        call getvtx(keywf, 'PREDICTION', iocc=1, scal=matrix_pred, nbret=iret)
        if (iret .eq. 0) then
            matrix_pred = matrix_corr
        endif
        ds_algopara%matrix_pred = matrix_pred
        ds_algopara%matrix_corr = matrix_corr
        if (matrix_pred .eq. 'DEPL_CALCULE') then
            call deprecated_algom(matrix_pred)
            call utmess('A', 'MECANONLINE5_57')
            call getvid(keywf, 'EVOL_NOLI', iocc=1, scal=result_prev_disp, nbret=iret)
            if (iret .le. 0) then
                call utmess('F', 'MECANONLINE5_45')
            endif
            ds_algopara%result_prev_disp = result_prev_disp
        endif
        call getvis(keywf, 'REAC_INCR', iocc=1, scal=reac_incr)
        ASSERT(reac_incr .ge. 0)
        ds_algopara%reac_incr = reac_incr
        call getvis(keywf, 'REAC_ITER', iocc=1, scal=reac_iter)
        ASSERT(reac_iter .ge. 0)
        ds_algopara%reac_iter = reac_iter
        call getvr8(keywf, 'PAS_MINI_ELAS', iocc=1, scal=pas_mini_elas, nbret=iret)
        if (iret .ne. 0) then
            ds_algopara%pas_mini_elas = pas_mini_elas
        endif
        call getvis(keywf, 'REAC_ITER_ELAS', iocc=1, scal=reac_iter_elas)
        ASSERT(reac_iter_elas .ge. 0)
        ds_algopara%reac_iter_elas   = reac_iter_elas
        call getvtx(keywf, 'MATR_RIGI_SYME', iocc=1, scal=answer)
        ds_algopara%l_matr_rigi_syme = answer.eq.'OUI'
    else if (algo_meth .eq. 'IMPLEX') then
        ds_algopara%matrix_pred = 'TANGENTE'
        ds_algopara%reac_incr   = 1
    else
        ASSERT(.false.)
    endif
!
end subroutine
