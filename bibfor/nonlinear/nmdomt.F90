subroutine nmdomt(algo_meth, algo_para)
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
    character(len=16), intent(inout) :: algo_meth(*)
    real(kind=8), intent(inout) :: algo_para(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Read
!
! Parameters of non-linear algorithm
!
! --------------------------------------------------------------------------------------------------
!
! IO  algo_meth        : parameters for algorithm methods
!                 1 : NOM DE LA METHODE NON LINEAIRE (NEWTON OU IMPLEX)
!                     (NEWTON OU NEWTON_KRYLOV OU IMPLEX)
!                 2 : TYPE DE MATRICE (TANGENTE OU ELASTIQUE)
!                 3 : -- INUTILISE --
!                 4 : -- INUTILISE --
!                 5 : METHODE D'INITIALISATION
!                 6 : NOM CONCEPT EVOL_NOLI SI PREDICTION 'DEPL_CALCULE'
!                 7 : METHODE DE RECHERCHE LINEAIRE
! IO  algo_para        : parameters for algorithm criteria
!                 1 : REAC_INCR
!                 2 : REAC_ITER
!                 3 : PAS_MINI_ELAS
!                 4 : REAC_ITER_ELAS
!                 5 : ITER_LINE_MAXI
!                 6 : RESI_LINE_RELA
!                 7 : RHO_MIN
!                 8 : RHO_MAX
!                 9 : RHO_EXCL
!
! --------------------------------------------------------------------------------------------------
!
    integer :: reac_incr, reac_iter, reac_iter_elas, iter_line_maxi
    real(kind=8) :: pas_mini_elas, resi_line_rela
    real(kind=8) :: reli_rho_mini, reli_rho_maxi, reli_rho_excl
    integer :: ifm, niv
    integer :: iret, nocc
    character(len=16) :: reli_meth, keywf
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE DONNEES RESOLUTION'
    endif
!
! - Get method
!
    keywf = 'NEWTON'
    call getvtx(' ', 'METHODE', scal=algo_meth(1))
    if ((algo_meth(1) .eq. 'NEWTON') .or. (algo_meth(1) .eq. 'NEWTON_KRYLOV')) then
        call getvtx(keywf, 'MATRICE', iocc=1, scal=algo_meth(2))
        call getvtx(keywf, 'PREDICTION', iocc=1, scal=algo_meth(5), nbret=iret)
        if (iret .le. 0) then
            algo_meth(5) = algo_meth(2)
        endif
        if (algo_meth(5) .eq. 'DEPL_CALCULE') then
            call deprecated_algom(algo_meth(5))
            call utmess('A', 'MECANONLINE5_57')
            call getvid(keywf, 'EVOL_NOLI', iocc=1, scal=algo_meth(6), nbret=iret)
            if (iret .le. 0) then
                call utmess('F', 'MECANONLINE5_45')
            endif
        endif
    else if (algo_meth(1) .eq. 'IMPLEX') then
        algo_meth(5) = 'TANGENTE'
    else
        ASSERT(.false.)
    endif
!
! - Get parameters (method)
!
    if ((algo_meth(1) .eq. 'NEWTON') .or. (algo_meth(1) .eq. 'NEWTON_KRYLOV')) then
        call getvis(keywf, 'REAC_INCR', iocc=1, scal=reac_incr)
        if (reac_incr .lt. 0) then
            ASSERT(.false.)
        else
            algo_para(1) = reac_incr
        endif
        call getvis(keywf, 'REAC_ITER', iocc=1, scal=reac_iter)
        if (reac_iter .lt. 0) then
            ASSERT(.false.)
        else
            algo_para(2) = reac_iter
        endif
        call getvr8(keywf, 'PAS_MINI_ELAS', iocc=1, scal=pas_mini_elas, nbret=iret)
        if (iret .le. 0) then
            algo_para(3) = -9999.0d0
        else
            algo_para(3) = pas_mini_elas
        endif
        call getvis(keywf, 'REAC_ITER_ELAS', iocc=1, scal=reac_iter_elas)
        if (reac_iter .lt. 0) then
            ASSERT(.false.)
        else
            algo_para(4) = reac_iter_elas
        endif
    else if (algo_meth(1) .eq. 'IMPLEX') then
        algo_para(1) = 1
    else
        ASSERT(.false.)
    endif
!
! - Get parameters (line search)
!
    keywf  = 'RECH_LINEAIRE'
    reli_meth = 'CORDE'
    iter_line_maxi = 0
    resi_line_rela = 1.d-3
    reli_rho_mini = 0.d0
    reli_rho_maxi = 1.d0
    reli_rho_excl = 0.d0
!
    call getfac(keywf, nocc)
    if (nocc .ne. 0) then
        call getvtx(keywf, 'METHODE'       , iocc=1, scal=reli_meth)
        call getvr8(keywf, 'RESI_LINE_RELA', iocc=1, scal=resi_line_rela)
        call getvis(keywf, 'ITER_LINE_MAXI', iocc=1, scal=iter_line_maxi)
        call getvr8(keywf, 'RHO_MIN'       , iocc=1, scal=reli_rho_mini)
        call getvr8(keywf, 'RHO_MAX'       , iocc=1, scal=reli_rho_maxi)
        call getvr8(keywf, 'RHO_EXCL'      , iocc=1, scal=reli_rho_excl)
        if (reli_rho_mini .ge. -reli_rho_excl .and. reli_rho_mini .le. reli_rho_excl) then
            call utmess('A', 'MECANONLINE5_46')
            reli_rho_mini = +reli_rho_excl
        endif
        if (reli_rho_maxi .ge. -reli_rho_excl .and. reli_rho_maxi .le. reli_rho_excl) then
            call utmess('A', 'MECANONLINE5_47')
            reli_rho_maxi = -reli_rho_excl
        endif
        if (reli_rho_maxi .lt. reli_rho_mini) then
            call utmess('A', 'MECANONLINE5_44')
            call getvr8(keywf, 'RHO_MIN', iocc=1, scal=reli_rho_maxi)
            call getvr8(keywf, 'RHO_MAX', iocc=1, scal=reli_rho_mini)
        endif
        if (abs(reli_rho_maxi-reli_rho_mini) .le. r8prem()) then
            call utmess('F', 'MECANONLINE5_43')
        endif
    endif
!
    algo_meth(7) = reli_meth
    algo_para(5) = iter_line_maxi
    algo_para(6) = resi_line_rela
    algo_para(7) = reli_rho_mini
    algo_para(8) = reli_rho_maxi
    algo_para(9) = reli_rho_excl
!
end subroutine
