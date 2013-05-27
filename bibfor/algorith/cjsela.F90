subroutine cjsela(mod, crit, materf, deps, sigd,&
                  sigf, nvi, vind, vinf, iret)
    implicit none
!       ===============================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ---------------------------------------------------------------
!       INTEGRATION ELASTIQUE NON LINEAIRE DE LA LOI CJS
!       IN  MOD    :  MODELISATION
!           CRIT   : CRITERES DE CONVERGENCE
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           SIGD   :  CONTRAINTE  A T
!           DEPS   :  INCREMENT DE DEFORMATION
!       OUT SIGF   :  CONTRAINTE A T+DT
!           IRET   : CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!       ---------------------------------------------------------------
    include 'asterfort/cjsci1.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nvi, iret
    real(kind=8) :: coef, e, nu, al, la, mu, hook(6, 6), i1
    real(kind=8) :: deps(6), dsig(6), sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: materf(14, 2), crit(*)
    character(len=8) :: mod
    real(kind=8) :: zero, un, d12, deux, trois, pa, qinit
    logical :: tract
    integer :: i, j
!
    common /tdim/   ndt  , ndi
!
    data          zero  / 0.d0 /
    data          d12   / .5d0 /
    data          un    / 1.d0 /
    data          deux  / 2.d0 /
    data          trois / 3.d0 /
!
!       ---------------------------------------------------------------
    pa = materf(12,2)
    qinit = materf(13,2)
!
!--->   CALCUL DE I1=TR(SIG) A T+DT PAR METHODE DE LA SECANTE
!       OU EXPLICITEMENT SI NIVEAU CJS1
!
    call cjsci1(crit, materf, deps, sigd, i1,&
                tract, iret)
    if (iret .eq. 1) goto 9999
!
!--->   EN CAS D'ENTREE EN TRACTION, LES CONTRAINTES SONT
!       RAMENEES SUR L'AXE HYDROSTATIQUE A DES VALEURS FAIBLES
!       ( EGALES A PA/100.0 SOIT -1 KPA )
!
    if (tract) then
        do 10 i = 1, ndi
            sigf(i) = -qinit/3.d0+pa/100.0d0
10      continue
        do 20 i = ndi+1, ndt
            sigf(i) = zero
20      continue
        goto 9999
    endif
!
!
!                         I1+QINIT
!--->   CALCUL DU COEF  (-----------)**N ET MODULE_YOUNG A T+DT
!                        3 PA
!
!
    coef = ((i1+qinit)/trois/pa)**materf(3,2)
    e = materf(1,1)* coef
    nu = materf(2,1)
    al = e * (un-nu) / (un+nu) / (un-deux*nu)
    la = nu * e / (un+nu) / (un-deux*nu)
    mu = e * d12 / (un+nu)
!
!--->   OPERATEUR DE RIGIDITE
!
    call lcinma(zero, hook)
!
! - 3D/DP/AX
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
        do 40 i = 1, ndi
            do 40 j = 1, ndi
                if (i .eq. j) hook(i,j) = al
                if (i .ne. j) hook(i,j) = la
40          continue
        do 45 i = ndi+1, ndt
            do 45 j = ndi+1, ndt
                if (i .eq. j) hook(i,j) = deux* mu
45          continue
!
! - CP/1D
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call u2mess('F', 'ALGORITH2_15')
    endif
!
!
!--->   INCREMENTATION DES CONTRAINTES  SIGF = SIGD + HOOK DEPS
!
    call lcprmv(hook, deps, dsig)
    call lcsove(sigd, dsig, sigf)
!
!
9999  continue
!
!--->   VINF = VIND, ETAT A T+DT = ELASTIQUE = 0
!
    call lceqvn(nvi-1, vind, vinf)
    vinf(nvi) = 0.d0
!
end subroutine
