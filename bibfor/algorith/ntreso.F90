subroutine ntreso(model , mate  , cara_elem, list_load, nume_dof,&
                  solver, lostat, time     , tpsthe   , reasvc  ,&
                  reasvt, reasmt, reasrg   , reasms   , creas   ,&
                  vec2nd, matass, maprec   , cndirp   , cnchci  ,&
                  mediri, compor)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infniv.h"
#include "asterfort/nxacmv.h"
#include "asterfort/resoud.h"
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
    integer :: ifm, niv
    character(len=16) :: k16b1, k16b2
    character(len=19) :: chsol, varc_curr
    character(len=24) :: mediri, vhydr, tmpchi, tmpchf, vec2ni, criter, result
    character(len=24) :: vtemp
    complex(kind=8) :: c16bid
    integer :: iret
    character(len=24) :: lload_name, lload_info, lload_func
    c16bid = dcmplx(0.d0, 0.d0)
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
    lload_func = list_load(1:19)//'.FCHA'
    chsol = '&&NTRESO_SOLUTION  '
    criter = '&&NTRESO_RESGRA_GCPC    '
    varc_curr = '&&NTRESO.CHVARC'
!
    call getres(result, k16b1, k16b2)
    vtemp='&&NXLECTVAR_____'
!
! 1 ==> ASSEMBLAGE DU SECOND MEMBRE
    call nxacmv(model , mate  , cara_elem, list_load, nume_dof,&
                solver, lostat, time     , tpsthe   , reasvc  ,&
                reasvt, reasmt, reasrg   , reasms   , creas   ,&
                vtemp , vhydr , varc_curr, tmpchi   , tmpchf  ,&
                vec2nd, vec2ni, matass   , maprec   , cndirp  ,&
                cnchci, mediri, compor)
!
! 2 ==> RESOLUTION AVEC VEC2ND COMME SECOND MEMBRE
    call resoud(matass, maprec, solver, cnchci, 0,&
                vec2nd, chsol, 'V', [0.d0], [c16bid],&
                criter, .true._1, 0, iret)
!
! 3. ==> SAUVEGARDE DE LA SOLUTION
!
! 4.1 ==> SAUVEGARDE DU CHAMP SOLUTION CHSOL DANS VTEMP
    call copisd('CHAMP_GD', 'V', chsol(1:19), vtemp(1:19))
!
! 4.2 ==> DESTRUCTION DU CHAMP SOLUTION CHSOL
    call detrsd('CHAMP_GD', chsol)
!
end subroutine
