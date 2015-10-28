subroutine comcq1(fami  ,kpg   ,ksp   ,imate ,compor,&
                  carcri,instm ,instp ,eps   ,deps  ,&
                  sigm  ,vim   ,option,angmas,sigp  ,&
                  vip   ,dsde  ,codret)
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
    implicit none
#include "asterfort/nm1dci.h"
#include "asterfort/nm1dis.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verifg.h"
#include "asterfort/verift.h"
!
!  VARIABLE ENTREE/SORTIE
    character(len=*) :: fami
    character(len=16) :: option, compor(*)
    integer :: codret, kpg, ksp, imate
    real(kind=8) :: angmas(3), sigm(4), eps(4), deps(4)
    real(kind=8) :: vim(*), vip(*), sigp(4), dsde(6, 6), carcri(*), lc(1)
    real(kind=8) :: instm, instp,  wkout(1)
    character(len=8) :: typmod(2)
!
!
    call r8inir(36, 0.d0, dsde, 1)
    call r8inir(4, 0.d0, sigp, 1)
!
    codret=0
!     INTEGRATION DE LA LOI DE COMPORTEMENT POUR LES COQUE_1D :
!     COQUE_AXIS : COMPORTEMENT C_PLAN
!     COQUE_AXIS, DIRECTION Y : CPLAN (EVT DEBORST)
!     DIRECTION Z : EPSZZ CONNU
!
        typmod(1) = 'C_PLAN  '
        call nmcomp(fami, kpg, ksp, 2, typmod,&
                    imate, compor, carcri, instm, instp,&
                    4, eps, deps, 4, sigm,&
                    vim, option, angmas, 1, lc,&
                    sigp, vip, 36, dsde, 1,&
                    wkout, codret)
!
end subroutine
