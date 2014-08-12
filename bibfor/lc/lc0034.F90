subroutine lc0034(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
    implicit none
#include "asterfort/nmhuj.h"
#include "asterfort/plasti.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utlcal.h"
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, iret
    real(kind=8) :: crit(*), angmas(*), instam, instap, tampon(*)
    real(kind=8) :: epsm(*), deps(*), sigm(6), sigp(6), vim(*), vip(*)
    real(kind=8) :: dsidep(6, 6), tm, tp, tref
    character(len=16) :: compor(*), option, algo
    character(len=8) :: typmod(*)
    character(len=*) :: fami
!
! APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
! RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret)
!
! --- LE CHOIX DE L'ALGORITHME DE RESOLUTION LOCALE
!     CONDITIONNE LA SUITE DU TRAITEMENT POUR L'INTEGRATION
!
    call utlcal('VALE_NOM', algo, crit(6))
!
    if ((algo(1:10).eq.'SPECIFIQUE') .or. (option(1:9).eq.'RIGI_MECA')) then
        call nmhuj(fami, kpg, ksp, typmod, imate,&
                   compor, crit, instam, instap,&
                   tm, tp, tref, angmas, epsm,&
                   deps, sigm, vim, option, sigp,&
                   vip, dsidep, codret)
!
    else
        call plasti(fami, kpg, ksp, typmod, imate,&
                    compor, crit, instam, instap, tm,&
                    tp, tref, epsm, deps, sigm,&
                    vim, option, angmas, sigp, vip,&
                    dsidep, icomp, nvi, tampon, codret)
!
    endif
!
end subroutine
