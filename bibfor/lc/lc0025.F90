subroutine lc0025(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, cp,&
                  epsm, deps, sigm, vim, option,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, numlc, dsidep, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/u2mess.h"
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, numlc
    real(kind=8) :: crit(*)
    real(kind=8) :: instam, instap, tampon(*)
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), sigp(6)
    real(kind=8) :: vim(*), vip(*)
    real(kind=8) :: dsidep(6, 6)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    logical :: cp
!
!     KIT_DDI
!       CALL NMCOUP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CP,
!C     &              CRIT,INSTAM, INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,
!C     &              TAMPON,NUMLC,SIGP,VIP,DSIDEP,CODRET)
    call u2mess('F', 'FERMETUR_11')
end subroutine
