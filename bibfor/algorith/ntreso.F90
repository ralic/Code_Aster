subroutine ntreso(modele, mate, carele, fomult, charge,&
                  lischa, infoch, numedd, solveu, lostat,&
                  time, tpsthe, reasvc, reasvt, reasmt,&
                  reasrg, reasms, creas, vec2nd, matass,&
                  maprec, cndirp, cnchci, mediri, compor)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1504
    implicit   none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infniv.h"
#include "asterfort/nxacmv.h"
#include "asterfort/resoud.h"
    real(kind=8) :: tpsthe(6)
    character(len=1) :: creas
    character(len=19) :: lischa, solveu, maprec
    character(len=24) :: modele, mate, carele, fomult, charge, infoch, numedd
    character(len=24) :: time, vec2nd, matass, cndirp, cnchci, compor
    logical :: reasvc, reasvt, reasmt, reasrg, reasms, lostat
!
! ----------------------------------------------------------------------
!     THERMIQUE LINEAIRE - RESOLUTION
!     *                    ****
!     COMMANDE:  THER_LINEAIRE
! ----------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE:INFMAJ,INFNIV.
!       JEVEUX/SD:COPISD,DETRSD,RSEXCH,RSAGSD,RSNOCH.
!       THERMIQUE: NXACMV,NTARCH.
!       SOLVEUR EF: RESOUD.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!
!
!
    integer :: ifm, niv
    real(kind=8) :: r8bid
    character(len=16) :: k16b1, k16b2
    character(len=19) :: chsol
    character(len=24) :: mediri, vhydr, tmpchi, tmpchf, vec2ni, criter, result
    character(len=24) :: vtemp
    complex(kind=8) :: c16bid
    integer :: iret
!
! ----------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    chsol = '&&NTRESO_SOLUTION  '
    criter = '&&NTRESO_RESGRA_GCPC    '
!
    call getres(result, k16b1, k16b2)
    vtemp='&&NXLECTVAR_____'
!
! 1 ==> ASSEMBLAGE DU SECOND MEMBRE
    call nxacmv(modele, mate, carele, fomult, charge,&
                lischa, infoch, numedd, solveu, lostat,&
                time, tpsthe, reasvc, reasvt, reasmt,&
                reasrg, reasms, creas, vtemp, vhydr,&
                tmpchi, tmpchf, vec2nd, vec2ni, matass,&
                maprec, cndirp, cnchci, mediri, compor)
!
! 2 ==> RESOLUTION AVEC VEC2ND COMME SECOND MEMBRE
    call resoud(matass, maprec, solveu, cnchci, 0,&
                vec2nd, chsol, 'V', r8bid, c16bid,&
                criter, .true., 0, iret)
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
