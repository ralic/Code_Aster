subroutine bursif(materd, materf, nmat, an, bn,&
                  cn, deps, nr, yd, dsig)
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
! person_in_charge: alexandre.foucault at edf.fr
!
!=====================================================================
! ROUTINE QUI CALCUL L INCREMENT DE CONTRAINTES (ELASTIQUE)
!  CORRIGE PAR LE FLUAGE TOTAL (PROPRE + DESSICCATION)
!
! IN  MATERD   : TABLEAU DES PROPRIETES MATERIAUX A T
!     MATERF   : TABLEAU DES PROPRIETES MATERIAUX A T+DT
!     NMAT     : DIMENSION DU TABLEAU (NMAT,2)
!     AN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE
!     BN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE
!     CN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE
!     DEPS     : INCREMENT DE DEFORMATION TOTALE -(RETRAITS)
!     NR       : DIMENSION VECTEUR INCONNUES
!     Y  D     : VECTEUR INCONNUES A T
! OUT DSIG     : INCREMENT DE CONTRAINTES ESTIME
!_______________________________________________________________________
!
    implicit none
#include "asterfort/lcdive.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcsove.h"
#include "asterfort/mgauss.h"
    integer :: i, j, ndt, ndi, nmat, iret, nr
    real(kind=8) :: an(6), bn(6, 6), cn(6, 6)
    real(kind=8) :: yd(nr), sigf(6), deps(6)
    real(kind=8) :: materf(nmat, 2), materd(nmat, 2), hook(6, 6), hookm(6, 6)
    real(kind=8) :: cnxe(6, 6), ident(6, 6), inver(6, 6), sige(6)
    real(kind=8) :: det, depsc(6), bnxsid(6)
    real(kind=8) :: dsig(6), sigd(6), sigt(6)
!     ----------------------------------------------------------------
    common /tdim/   ndt ,ndi
!     ----------------------------------------------------------------
! === =================================================================
! --- INITIALISATION DES VARIABLES
! === =================================================================
    do 1 i = 1, ndt
        sigf(i) = 0.d0
        sigd(i) = yd(i)
        do 2 j = 1, ndt
            cnxe(i,j) = 0.d0
            ident(i,j) = 0.d0
 2      continue
 1  end do
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', materf, hook)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 4 (I+E.CN)
! === =================================================================
    call lcprmm(hook, cn, cnxe)
    do 4 i = 1, ndt
        cnxe(i,i) = 1.d0 + cnxe(i,i)
 4  end do
!
    do 5 i = 1, ndt
        ident(i,i) = 1.d0
 5  end do
! === =================================================================
! --- INVERSION DU TENSEUR ORDRE 4 --> (I+E.CN)^(-1)
! === =================================================================
    call mgauss('NFVP', cnxe, ident, 6, ndt,&
                ndt, det, iret)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 2 DEPSC=(DEPS-AN-BN*SIGD)
! === =================================================================
    call lcprmv(bn, sigd, bnxsid)
    do 6 i = 1, ndt
        depsc(i)=deps(i)-an(i)-bnxsid(i)
 6  end do
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 2 DSIG = HOOK * DEPSC
! === =================================================================
    call lcprmv(hook, depsc, dsig)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 2 HOOK(T+)*(HOOK(T-)^)-1*SIGD = SIGT
! === =================================================================
    call lcopli('ISOTROPE', '3D      ', materd, hookm)
    call lcinma(0.d0, inver)
    do 7 i = 1, ndt
        inver(i,i) = 1.d0
 7  end do
    call mgauss('NFVP', hookm, inver, 6, ndt,&
                ndt, det, iret)
    call lcprmm(hook, inver, hookm)
    call lcprmv(hookm, sigd, sigt)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 2 (SIGT+DSIG) = SIGF
! === =================================================================
    call lcsove(sigt, dsig, sige)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 2  (I+E.CN)^(-1)*SIGF=SIGF
! === =================================================================
    call lcprmv(ident, sige, sigf)
! === =================================================================
! --- CONSTRUCTION TENSEUR ORDRE 2  DSIG = SIGF - SIGD
! === =================================================================
    call lcdive(sigf, sigd, dsig)
!
end subroutine
