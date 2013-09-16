subroutine nmco1d(fami, kpg, ksp, imate, compor,&
                  option, epsm, deps, angmas, sigm,&
                  vim, sigp, vip, dsidep, codret)
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
    implicit none
#include "jeveux.h"
#include "asterfort/comp1d.h"
#include "asterfort/nm1dci.h"
#include "asterfort/nm1dis.h"
#include "asterfort/nm1dpm.h"
#include "asterfort/nmmaba.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/vmci1d.h"
    integer :: imate, codret, kpg, ksp
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    real(kind=8) :: epsm, deps, sigm, vim(*)
    real(kind=8) :: angmas(3)
!
    real(kind=8) :: sigp, vip(*), dsidep
! --------------------------------------------------------------------------------------------------
!
!          REALISE LES LOIS 1D (DEBORST OU EXPLICITEMENT 1D)
!
!
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
!                               (2) = NB VARIABLES INTERNES / PG
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
! IN  DEPS    : INCREMENT DE DEFORMATION (SCALAIRE DANS CE CAS)
! IN  SIGM    : CONTRAINTE A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN   TM     : TEMPERATURE L'INSTANT DU CALCUL PRECEDENT
! IN   TP     : TEMPERATURE A L'INSTANT DU
! IN  TREF    : TEMPERATURE DE REFERENCE
! OUT SIGP    : CONTRAINTE A L'INSTANT ACTUEL
!     VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
!     DSIDEP  : RIGIDITE (SCALAIRE DANS CE CAS)
!     CODRET  : CODE RETOUR NON NUL SI SIGYY OU SIGZZ NON NULS
!
! --------------------------------------------------------------------------------------------------
!
    logical :: cine, isot, pinto, com1d, elas, cinegc
    real(kind=8) :: e, et, sigy
    integer :: nvarpi
    parameter    ( nvarpi=8)
    integer :: ncstpm, iret
    parameter     (ncstpm=13)
    real(kind=8) :: cstpm(ncstpm)
    real(kind=8) :: em, ep, depsth, depsm, val(1)
    integer :: codres(1)
!
    character(len=8) ::  materi
    character(len=16) :: valkm(2)
! --------------------------------------------------------------------------------------------------
!
    elas = .false.
    isot = .false.
    cine = .false.
    cinegc = .false.
    pinto = .false.
    com1d = .false.
    codret = 0
    materi = ' '
!
    if (compor(1)(1:16) .eq. 'GRILLE_ISOT_LINE') then
        isot = .true.
    else if (compor(1)(1:16) .eq. 'GRILLE_CINE_LINE') then
        cine = .true.
    else if (compor(1)(1:12) .eq. 'VMIS_CINE_GC') then
        cinegc = .true.
    else if (compor(1)(1:16) .eq. 'GRILLE_PINTO_MEN') then
        pinto = .true.
    else if (compor(1)(1:4) .eq. 'ELAS') then
        elas = .true.
    else
        com1d=.true.
        if ((compor(5)(1:7).ne.'DEBORST') .and. (compor(1)(1:4) .ne.'SANS')) then
            valkm(1) = compor(1)
            valkm(2) = 'COMP_INCR'
            call utmess('F', 'ALGORITH6_81', nk=2, valk=valkm)
        endif
    endif
!
    if (.not.com1d) then
!       caractéristiques élastiques à t-
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'E', val, codres, 1)
        em=val(1)            
!       caractéristiques élastiques à t+
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'E', val, codres, 1)
        ep=val(1)            
    endif
!
    if (isot) then
        call verift(fami, kpg, ksp, 'T', imate,&
                    materi, 'ELAS', 1, depsth, iret)
        depsm = deps-depsth
        call nm1dis(fami, kpg, ksp, imate, em,&
                    ep, sigm, depsm, vim, option,&
                    compor, ' ', sigp, vip, dsidep)
!
    else if (cine) then
        call verift(fami, kpg, ksp, 'T', imate,&
                    materi, 'ELAS', 1, depsth, iret)
        depsm = deps-depsth
        call nm1dci(fami, kpg, ksp, imate, em,&
                    ep, sigm, depsm, vim, option,&
                    ' ', sigp, vip, dsidep)
!
    else if (cinegc) then
        call verift(fami, kpg, ksp, 'T', imate,&
                    materi, 'ELAS', 1, depsth, iret)
        depsm = deps-depsth
        call vmci1d('RIGI', kpg, ksp, imate, em,&
                    ep, sigm, depsm, vim, option,&
                    ' ', sigp, vip, dsidep)
    else if (elas) then
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
            dsidep = ep
        endif
        if (option .eq. 'RAPH_MECA' .or. option .eq. 'FULL_MECA') then
            vip(1) = 0.d0
            call verift(fami, kpg, ksp, 'T', imate,&
                        materi, 'ELAS', 1, depsth, iret)
            sigp = ep* (sigm/em+deps-depsth)
        endif
!
    else if (com1d) then
        call comp1d(fami, kpg, ksp, option, sigm,&
                    epsm, deps, angmas, vim, vip,&
                    sigp, dsidep, codret)
!
    else if (pinto) then
        call nmmaba(imate, compor, e, et, sigy,&
                    ncstpm, cstpm)
        call verift(fami, kpg, ksp, 'T', imate,&
                    materi, 'ELAS', 1, depsth, iret)
        depsm = deps-depsth
        call nm1dpm(fami, kpg, ksp, imate, option,&
                    nvarpi, ncstpm, cstpm, sigm, vim,&
                    depsm, vip, sigp, dsidep)
    endif
end subroutine
