subroutine ntreso(model , mate  , cara_elem, list_load, nume_dof,&
                  solver, lostat, time     , tpsthe   , reasvc  ,&
                  reasvt, reasmt, reasrg   , reasms   , creas   ,&
                  vec2nd, matass, maprec   , cndirp   , cnchci  ,&
                  mediri, compor)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/nxacmv.h"
#include "asterfort/nxreso.h"
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
! aslint: disable=W1504
!
    real(kind=8) :: tpsthe(6)
    character(len=1) :: creas
    character(len=19) :: list_load, solver, maprec
    character(len=24) :: model, mate, cara_elem, nume_dof
    character(len=24) :: time, vec2nd, matass, cndirp, cnchci, compor
    aster_logical :: reasvc, reasvt, reasmt, reasrg, reasms, lostat
!
! --------------------------------------------------------------------------------------------------
!
!     THERMIQUE LINEAIRE - RESOLUTION
!     *                    ****
!     COMMANDE:  THER_LINEAIRE
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: chsol, varc_curr
    character(len=24) :: mediri, vhydr, tmpchi, tmpchf, vec2ni
    character(len=24) :: vtemp
!
! --------------------------------------------------------------------------------------------------
!
!
    chsol     = '&&NTRESO_SOLUTION'
    varc_curr = '&&NTRESO.CHVARC'
    vtemp     ='&&NXLECTVAR_____'
!
! - Construct second member
!
    call nxacmv(model , mate  , cara_elem, list_load, nume_dof,&
                solver, lostat, time     , tpsthe   , reasvc  ,&
                reasvt, reasmt, reasrg   , reasms   , creas   ,&
                vtemp , vhydr , varc_curr, tmpchi   , tmpchf  ,&
                vec2nd, vec2ni, matass   , maprec   , cndirp  ,&
                cnchci, mediri, compor)
!
! - Solve linear system
!
    call nxreso(matass, maprec, solver, cnchci, vec2nd,&
                chsol)
!
! - Save solution
!
    call copisd('CHAMP_GD', 'V', chsol, vtemp)
    call detrsd('CHAMP_GD', chsol)
!
end subroutine
