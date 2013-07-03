subroutine lcdehy(fami, kpg, ksp, nmat, materd,&
                  materf, depsm, epsdm)
    implicit none
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       RETRAIT DE DEFORMATION DUE AU RETRAIT ENDOGENE ET AU RETRAIT
!       DE DESSICCATION
!       POUR TENIR COMPTE DES CONTRAINTES DE RETRAIT :
!
!       ON RETIRE       - A DEPSM, L INCREMENT DE DEFORMATION DUE
!                         AU RETRAIT
!                       - ET A EPSDM , LE RETRAIT A T
!
!       POUR OBTENIR    - L'INCREMENT DE DEFORMATION MECANIQUE DEPSM
!                       - ET LA DEFORMATION MECANIQUE A T      EPSDM
!
!       ON A SIG = HOOK EPSE  = HOOK ( EPST - EPSP - EPSRET )
!                             = HOOK ( EPST - EPSP ) - HOOK EPSRET
!       DONC            SIG   = SIGM                 + SIGRET
!       AVEC            SIGRET= + HOOK (KAPPA*(SREF-SECH) + BETA*HYDR) I
!                       DE SIGNE OPPOSE A LA DEFORMATION THERMIQUE
!       OU   EN PRENANT EPS   = EPST - EPSRET
!                       SIG   = HOOK ( EPS - EPSP )
!
!       ON PEUT DONC - SOIT TRAVAILLER AVEC EPST ET AJOUTER SIGRET APRES
!                    - SOIT TRAVAILLER AVEC EPS = EPST - EPSRET
!                      CE QUI EST FAIT ICI
!       ----------------------------------------------------------------
!       IN      NMAT    DIMENSION  DE MATER
!               MATERD  COEFFICIENTS MATERIAU A T
!               MATERF  COEFFICIENTS MATERIAU A T+DT
!       VAR     DEPSM   INCREMENT DE DEFORMATION MECANIQUE
!               EPSDM   DEFORMATION MECANIQUE A T
!       ----------------------------------------------------------------
#include "asterfort/rcvarc.h"
    integer :: ndt, ndi, nmat, k, iret, kpg, ksp
    character(len=*) :: fami
    real(kind=8) :: hd, hf, sd, sf, sref
    real(kind=8) :: epsdm(6), depsm(6)
    real(kind=8) :: bendod, bendof, kdessd, kdessf
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
!
    bendod = materd(4,1)
    bendof = materf(4,1)
    kdessd = materd(5,1)
    kdessf = materf(5,1)
    call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                ksp, hd, iret)
    if (iret .ne. 0) hd=0.d0
    call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                ksp, hf, iret)
    if (iret .ne. 0) hf=0.d0
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, sd, iret)
    if (iret .ne. 0) sd=0.d0
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, sf, iret)
    if (iret .ne. 0) sf=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret)
    if (iret .ne. 0) sref=0.d0
!
    do 110 k = 1, ndi
        depsm(k) = depsm(k) + ( bendof*hf - bendod*hd) + ( kdessf*( sref-sf) - kdessd*(sref-sd))
        epsdm(k) = epsdm(k) + bendod*hd + kdessd*(sref-sd)
110  continue
!
end subroutine
