subroutine lcgldm(epsm, deps, vim, option, sig,&
                  vip, dsidep, lambda, deuxmu, lamf,&
                  deumuf, gmt, gmc, gf, seuil,&
                  alf, alfmc, crit, codret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
!
#include "asterfort/ceps33.h"
#include "asterfort/cntmat.h"
#include "asterfort/cstgld.h"
#include "asterfort/diago2.h"
#include "asterfort/gldloc.h"
#include "asterfort/r8inir.h"
    integer :: codret
    real(kind=8) :: epsm(6), deps(6), vim(*), crit(*), seuil, alfmc
    real(kind=8) :: lambda, deuxmu, lamf, deumuf, alf, gmt, gmc, gf
    real(kind=8) :: sig(6), dsidep(6, 6), vip(*)
    character(len=16) :: option
! ----------------------------------------------------------------------
!
!      LOI GLOBALE POUR LES PLAQUES/COQUES DKT - GLRC_DM
!
! IN:
!       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
!       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
!       LAMF    : PARAMETRE D ELASTICITE - FLEXION
!       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION
!       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!       GF      : PARAMETRE GAMMA POUR LA FLEXION
!       SEUIL   : INITIAL MEMBRANE
!       ALF     : PARAMETRE DE SEUIL FLEXION
!       VIM     : VARIABLES INTERNES EN T-
!       OPTION  : TOUTES
!       CRIT    : CRITERES DE CONVERGENCE LOCAUX
!              (1) = NB ITERATIONS MAXI A CONVERGENCE
!                    (ITER_INTE_MAXI == ITECREL)
!              (2) = TYPE DE JACOBIEN A T+DT
!                    (TYPE_MATR_COMP == MACOMP)
!                     0 = EN VITESSE     >SYMETRIQUE
!                     1 = EN INCREMENTAL >NON-SYMETRIQUE
!              (3) = VALEUR TOLERANCE DE CONVERGENCE
!                    (RESI_INTE_RELA == RESCREL)
!              (5) = NOMBRE D'INCREMENTS POUR LE
!                    REDECOUPAGE LOCAL DU PAS DE TEMPS
!                    (ITER_INTE_PAS  == ITEDEC)
!                    -1,0,1 = PAS DE REDECOUPAGE
!                     N = NOMBRE DE PALIERS
!              (6) = TYPE D INTEGRATION LOCAL POUR LA LOI DE
!                    COMPORTEMENT (ALGO_INTE)
! OUT:
!       SIG     : CONTRAINTE
!       VIP     : VARIABLES INTERNES EN T+
!       DSIDEP  : MATRICE TANGENTE
!       CODRET  : CODE RETOUR DE L'INTEGRATION DE LA LDC
!                 0 => PAS DE PROBLEME
!                 1 => ABSENCE DE CONVERGENCE
! ----------------------------------------------------------------------
!
!       QM1 ET QM2 = Tm DANS R7.01.32
!       QFF        = Tf DANS R7.01.32
!
    logical :: rigi, resi, coup
    logical :: lelas, elas, elas1, elas2
    integer :: k, kdmax
    real(kind=8) :: eps(6), emp(2), efp(2), qff(2)
    real(kind=8) :: vmp(2, 2), vfp(2, 2)
    real(kind=8) :: muf, trot, treps, eps33, de33d1, de33d2
    real(kind=8) :: da1, da2, ksi2d, dksi1, dksi2
    real(kind=8) :: tr2d, told, cof1(2), q2d(2)
    real(kind=8) :: cof2(2), dq2d(2)
!
! --  OPTION ET MODELISATION
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    coup = (option(6:9).eq.'COUP')
    if (coup) rigi=.true.
    lelas = option .eq.'RIGI_MECA       '
!
! -- INITIALISATION
    if (lelas) then
        call r8inir(6, 0.d0, epsm, 1)
        call r8inir(6, 0.d0, deps, 1)
    endif
!
    muf = deumuf*0.5d0
!
! --  CALCUL DES EPSILON INITIAUX
    if (resi) then
        do k = 1, 6
            eps(k) = epsm(k) + deps(k)
        end do
    else
        do k = 1, 6
            eps(k) = epsm(k)
        end do
    endif
!
! --  ON UTILISE EPSILON SOUS FORME VECTORIELLE
! --  DONC ON DIVISE LES TERMES NON DIAGONNAUX PAR 2
    eps(3) = eps(3)/2.0d0
    eps(6) = eps(6)/2.0d0
!
! -- DIAGONALISATION DES DEFORMATIONS
    call diago2(eps(1), vmp, emp)
    call diago2(eps(4), vfp, efp)
!
! --  CALCUL DES TRACES
    tr2d = eps(1)+eps(2)
    trot = efp(1)+efp(2)
!
! --  CALCUL DES CONSTANTES INDEPENDANTES DE DA1, DA2 ET EPS33
    call cstgld(lamf, muf, alf, gf, emp, efp, qff)
!
! --  INITIALISATION DE DA1, DA2 ET EPS33
    if (lelas) then
        da1 = 0.0d0
        da2 = 0.0d0
    else
        da1 = vim(1)
        da2 = vim(2)
    endif
!
! --  EVOLUTION DE DA1, DA2 ET EPS33
!     INTEGRATION DE LA LOI DE COMPORTEMENT
    if (resi) then
        told = crit(3)
        kdmax = nint(crit(1))
!
        call gldloc(lambda, deuxmu, deumuf, seuil, alf,&
                    alfmc, gmt, gmc, gf, cof1,&
                    vim, q2d, qff, tr2d, eps33,&
                    de33d1, de33d2, ksi2d, dksi1, dksi2,&
                    da1, da2, kdmax, told, codret,&
                    emp)
!
        if (da1 .lt. vim(1)) da1 = vim(1)
        if (da2 .lt. vim(2)) da2 = vim(2)
!
        elas1 = da1.le.vim(1)
        elas2 = da2.le.vim(2)
!
        elas1 = elas1 .or. lelas
        elas2 = elas2 .or. lelas
        elas = elas1 .and. elas2
!
        vip(1) = da1
        vip(2) = da2
        if (elas1) then
            vip(3) = 0.0d0
        else
            vip(3) = 1.0d0
        endif
        if (elas2) then
            vip(4) = 0.0d0
        else
            vip(4) = 1.0d0
        endif
        vip(5)=1.d0-0.5d0*((1.d0+gmt*da1)/(1.d0+da1) +(1.d0+gmt*da2)/(1.d0+da2))
        vip(6)=1.d0-0.5d0*((1.d0+gmc*da1)/(1.d0+da1) +(1.d0+gmc*da2)/(1.d0+da2))
        vip(7)=1.d0-max((1.d0+gf*da1)/(1.d0+da1), (1.d0+gf*da2)/(1.d0+da2))
    else
        if (lelas) then
            da1 = 0.0d0
            da2 = 0.0d0
            elas1 = .true.
            elas2 = .true.
            elas = .true.
        else
            da1 = vim(1)
            da2 = vim(2)
            elas1 = nint(vim(3)).eq.0
            elas2 = nint(vim(4)).eq.0
            elas = (elas1.and.elas2)
        endif
    endif
    call ceps33(lambda, deuxmu, alfmc, gmt, gmc,&
                tr2d, da1, da2, eps33, de33d1,&
                de33d2, ksi2d, dksi1, dksi2, cof1,&
                q2d, emp, cof2, dq2d)
!
! --  CALCUL DE LA TRACE 3D
    treps = tr2d + eps33
!
! --  CALCUL DES CONTRAINTES GENERALISEES ET DE LA MATRICE TANGENTE
!
    call cntmat(lambda, deuxmu, lamf, deumuf, alf,&
                alfmc, emp, efp, eps, vmp,&
                vfp, tr2d, trot, treps, gmt,&
                gmc, gf, da1, da2, ksi2d,&
                qff, cof1, q2d, de33d1, de33d2,&
                elas, elas1, elas2, coup, rigi,&
                resi, option, dsidep, sig, cof2,&
                dq2d)
!
end subroutine
