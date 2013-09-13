subroutine vrcpto(compor, deps, neps, fami, kpg,&
                  ksp, imate)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
    implicit none
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
    integer :: imate, kpg, ksp
    character(len=*) :: fami
    character(len=16) :: compor(*)
    integer :: neps
    real(kind=8) :: deps(neps)
! ----------------------------------------------------------------------
!     ON CALCULE LA DEFORMATION MECANIQUE ASSOCIEE A LA VARIABLE DE
!     COMMANDE PTOT.
!     ON LA RETRANCHE ENSUITE AUX DEFORMATIONS MECANIQUES TOTALES DEPS
!
!
! IN COMPOR   : COMPORTEMENT DE L ELEMENT
!                COMPOR(1) = RELATION DE COMPORTEMENT (VMIS_...)
!                COMPOR(2) = NB DE VARIABLES INTERNES
!                COMPOR(3) = TYPE DE DEFORMATION (PETIT,GREEN...)
! IN/OUT DEPS : INCREMENT DE DEFORMATION
! IN  NEPS    : NOMBRE DE CMP DE DEPS (SUIVANT MODELISATION)
! IN  FAMI    : FAMILLE DE POINTS DE GAUSS
! IN  KPG,KSP : NUMERO DU (SOUS)POINT DE GAUSS
! IN  IMATE   : ADRESSE DU MATERIAU CODE
!
    integer :: icodre(2)
    character(len=8) :: nomres(2)
    real(kind=8) :: valres(2)
    real(kind=8) :: ptotm, ptotp, biotp, biotm, em, num, ep, nup, troikp, troikm
    integer :: iret1, iret2, k
!
    logical :: lpomec
    integer :: dmmeca, ii
    parameter     ( dmmeca = 19 )
    character(len=16) :: pomeca(dmmeca)
!
    data pomeca / 'ELAS'            ,&
     &              'CJS'             ,&
     &              'HUJEUX'          ,&
     &              'CAM_CLAY'        ,&
     &              'BARCELONE'       ,&
     &              'LAIGLE'          ,&
     &              'LETK'            ,&
     &              'VISC_DRUC_PRAG'  ,&
     &              'HOEK_BROWN_EFF'  ,&
     &              'HOEK_BROWN_TOT'  ,&
     &              'MAZARS'          ,&
     &              'ENDO_ISOT_BETON' ,&
     &              'ELAS_GONF'       ,&
     &              'DRUCK_PRAGER'    ,&
     &              'DRUCK_PRAG_N_A'  ,&
     &              'JOINT_BANDIS'    ,&
     &              'CZM_LIN_REG'     ,&
     &              'CZM_EXP_REG'     ,&
     &              'ENDO_HETEROGENE' /
!
!
! DEB ------------------------------------------------------------------
!
    call rcvarc(' ', 'PTOT', '-', fami, kpg,&
                ksp, ptotm, iret1)
    call rcvarc(' ', 'PTOT', '+', fami, kpg,&
                ksp, ptotp, iret2)
!
    if ((iret1.eq.1) .and. (iret2.eq.1)) goto 9999
!
    if (iret1 .ne. iret2) then
        call utmess('F', 'CHAINAGE_11')
    endif
!
    if ((iret1.eq.0) .and. (iret2.eq.0)) then
!
        lpomec = .false.
        do 1 ii = 1, dmmeca
            if (compor(1) .eq. pomeca(ii)) lpomec = .true.
 1      continue
!
        if (.not.lpomec) then
            call utmess('F', 'CHAINAGE_9', sk=compor(1))
        endif
!
        if (compor(3) .ne. 'PETIT') then
            call utmess('F', 'CHAINAGE_8')
        endif
!
!
! --- COEFFICIENT DE BIOT
!
        nomres(1)='BIOT_COE'
!
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'THM_DIFFU', 0, ' ', 0.d0,&
                    1, nomres(1), valres(1), icodre, 1)
        if (icodre(1) .ne. 0) valres(1) = 0.d0
        biotm = valres(1)
!
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'THM_DIFFU', 0, ' ', 0.d0,&
                    1, nomres(1), valres(1), icodre, 1)
        if (icodre(1) .ne. 0) valres(1) = 0.d0
        biotp = valres(1)
!
! --- MODULE DE YOUNG ET COEFFICIENT DE POISSON
!
        nomres(1)='E'
        nomres(2)='NU'
!
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    2, nomres, valres, icodre, 2)
        if (icodre(1) .ne. 0) valres(1) = 0.d0
        if (icodre(2) .ne. 0) valres(2) = 0.d0
        em = valres(1)
        num = valres(2)
!
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    2, nomres, valres, icodre, 2)
        if (icodre(1) .ne. 0) valres(1) = 0.d0
        if (icodre(2) .ne. 0) valres(2) = 0.d0
        ep = valres(1)
        nup = valres(2)
!
        troikp = ep/(1.d0-2.d0*nup)
        troikm = em/(1.d0-2.d0*num)
!
! --- CALCUL DE LA DEFORMATION TOTALE ACTUALISEE
!
        do 10 k = 1, 3
            deps(k) = deps(k)-(biotp/troikp*ptotp-biotm/troikm*ptotm)
10      continue
!
    endif
!
9999  continue
!
end subroutine
