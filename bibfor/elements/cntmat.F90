subroutine cntmat(lambda, deuxmu, lamf, deumuf, alf,&
                  alfmc, emp, efp, eps, vmp,&
                  vfp, tr2d, trot, treps, gmt,&
                  gmc, gf, da1, da2, ksi2d,&
                  qff, cof1, q2d, de33d1, de33d2,&
                  elas, elas1, elas2, coup, rigi,&
                  resi, option, dsidep, sig, cof2,&
                  dq2d)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1504
    implicit none
! --  IN
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/matinv.h"
#include "asterfort/r8inir.h"
#include "asterfort/tanmgl.h"
    aster_logical :: resi, rigi, coup, elas, elas1, elas2
!
    real(kind=8) :: lambda, deuxmu, lamf, deumuf, alf, treps, gmt, gmc, gf
    real(kind=8) :: da1, da2, cof1(2), q2d(2), de33d1, de33d2, ksi2d, tr2d
    real(kind=8) :: trot, de33i
    real(kind=8) :: eps(2), emp(2), efp(2), vmp(2, 2), vfp(2, 2), qff(2)
    real(kind=8) :: cof2(2), dq2d(2)
    character(len=16) :: option
!
! --  OUT
    real(kind=8) :: sig(6), dsidep(6, 6)
!
!----------------------------------------------------------------------
!        CALCUL DES CONTRAINTES GENERALISEES ET DE LA MATRICE TANGENTE
!
!     IN :
!         LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
!         MU      : PARAMETRE D ELASTICITE - MEMBRANE
!         LAMF    : PARAMETRE D ELASTICITE - FLEXION
!         DEUMUF  : PARAMETRE D ELASTICITE - FLEXION
!         ALF     : PARAMETRE DE SEUIL FLEXION
!         EMP(2)  : VALEURS PROPRES DE EPS 2D
!         EFP(2)  : VALEURS PROPRES DE KAPPA
!         VMP(2,2): VECTEURS PROPRES DE EPS 2D
!         VFP(2,2): VECTEURS PROPRES DE KAPPA
!         TR2D    : TRACE EPSILON 2D
!         TROT    : TRACE KAPPA 2D
!         TREPS   : TRACE 3D
!         GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!         GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!         GF      : PARAMETRE GAMMA POUR LA FLEXION
!         DA1,DA2 : VALEURS D'ENDOMMAGEMENT
!         KSI2D   : FONCTION CARACTERISTIQUE D ENDOMMAGEMENT
!         QFF(2)  : TF
!         COF1    : INTERMEDIAIRE DE CALCUL
!         Q2D     : INTERMEDIAIRE DE CALCUL
!         DE33D1  : DERIVEE DE E33 PAR RAPPORT A DA1
!         DE33D2  : DERIVEE DE E33 PAR RAPPORT A DA2
!         ELAS    : .TRUE. SI ELASTIQUE
!         ELAS1   : .TRUE. SI DA1 == VIM(1)
!         ELAS2   : .TRUE. SI DA2 == VIM(2)
!         COUP    : OPTION
!         RIGI    : OPTION
!         RESI    : OPTION
!         OPTION  : TOUTES
!
!     OUT :
!          SIG(6)      : CONTRAINTES GENERALISEES DANS LE REPERE GLOBAL
!          DSIDEP(6,6) : MATRICE TANGENTE
!----------------------------------------------------------------------
!
    integer :: k, l, i
!
    real(kind=8) :: lambdd, lamfd, dlmd1, dlmd2, dlmfd1, dlmfd2
    real(kind=8) :: treps2, gf1, gf2, mu, fd1, fd2
    real(kind=8) :: qm1, qm2, alfmc
    real(kind=8) :: a(2, 2), ainv(2, 2)
    real(kind=8) :: dndd(2, 2), dmdd(2, 2)
    real(kind=8) :: deumud(2), demudf(2), d1mud(2), d2mud(2)
    real(kind=8) :: d1mudf(2), d2mudf(2)
    real(kind=8) :: fdi1(2), fdi2(2), sigp(3), sigf(3)
    real(kind=8) :: deta
    real(kind=8) :: dspdep(6, 6)
!
    mu = deuxmu * 0.5d0
!
! --  CALCUL DES TRACES
    tr2d = eps(1)+eps(2)
    trot = efp(1)+efp(2)
!
! --  CALCUL DE GF1 ET GF2
!     ICI ON SUPPOSE QUE GF1=GF2, CE QUI N EST PAS NECESSAIRE
    gf1 = gf
    gf2 = gf
!
! --  CALCUL DES CONTRAINTES GENERALISEES
! --  INITIALISATION
    call r8inir(3, 0.d0, sigp, 1)
    call r8inir(3, 0.d0, sigf, 1)
!
! --  CALCUL DE LA CONTRAINTE DE MEMBRANE SIGP
    if (tr2d .gt. 0.d0) then
        fd1 = (1.d0 + gmt*da1) / (1.d0 + da1)
        fd2 = (1.d0 + gmt*da2) / (1.d0 + da2)
        dlmd1 = -0.5d0*lambda*(1.d0-gmt)/(1.d0+da1)**2
        dlmd2 = -0.5d0*lambda*(1.d0-gmt)/(1.d0+da2)**2
    else
        fd1 = (alfmc + gmc*da1) / (alfmc + da1)
        fd2 = (alfmc + gmc*da2) / (alfmc + da2)
        dlmd1 = -0.5d0*lambda*alfmc*(1.d0-gmc)/(alfmc+da1)**2
        dlmd2 = -0.5d0*lambda*alfmc*(1.d0-gmc)/(alfmc+da2)**2
    endif
!
    lambdd = lambda *0.5d0*(fd1 + fd2)
!
    do 80 k = 1, 2
        if (emp(k) .gt. 0.d0) then
            fdi1(k) = (1.d0 + gmt*da1) / (1.d0 + da1)
            fdi2(k) = (1.d0 + gmt*da2) / (1.d0 + da2)
            d2mud(k) = -mu*(1.d0-gmt)/(1.d0+da2)**2
            d1mud(k) = -mu*(1.d0-gmt)/(1.d0+da1)**2
        else
            fdi1(k) = (alfmc + gmc*da1) / (alfmc + da1)
            fdi2(k) = (alfmc + gmc*da2) / (alfmc + da2)
            d2mud(k) = -mu*alfmc*(1.d0-gmc)/(alfmc+da2)**2
            d1mud(k) = -mu*alfmc*(1.d0-gmc)/(alfmc+da1)**2
        endif
        deumud(k) = mu*(fdi1(k) + fdi2(k))
        sigp(1) = sigp(1) + deumud(k)*emp(k)*vmp(1,k)**2
        sigp(2) = sigp(2) + deumud(k)*emp(k)*vmp(2,k)**2
        sigp(3) = sigp(3) + deumud(k)*emp(k)*vmp(1,k)*vmp(2,k)
 80 end do
!
! --  CALCUL DE LA CONTRAINTE DE FLEXION SIGF
    if (trot .gt. 0.0d0) then
        lamfd = lamf*(alf + gf2*da2)/(alf + da2)
        dlmfd2 = -lamf*alf*(1.0d0 - gf2)/(alf + da2)**2
        dlmfd1 = 0.0d0
    else
        lamfd = lamf*(alf + gf1*da1)/(alf + da1)
        dlmfd1 = -lamf*alf*(1.0d0 - gf1)/(alf + da1)**2
        dlmfd2 = 0.0d0
    endif
!
    do 90 k = 1, 2
        if (efp(k) .gt. 0.0d0) then
            demudf(k) = deumuf*(alf + gf2*da2)/(alf + da2)
            d2mudf(k) = -deumuf*alf*(1.0d0 - gf2)/(alf + da2)**2
            d1mudf(k) = 0.0d0
        else
            demudf(k) = deumuf*(alf + gf1*da1)/(alf + da1)
            d1mudf(k) = -deumuf*alf*(1.0d0 - gf1)/(alf + da1)**2
            d2mudf(k) = 0.0d0
        endif
!
        sigf(1) = sigf(1) + demudf(k)*efp(k)*vfp(1,k)**2
        sigf(2) = sigf(2) + demudf(k)*efp(k)*vfp(2,k)**2
        sigf(3) = sigf(3) + demudf(k)*efp(k)*vfp(1,k)*vfp(2,k)
 90 end do
!
! --  CALCUL DE SIG
    if (resi .and. (.not.coup)) then
        call r8inir(6, 0.d0, sig, 1)
!
        sig(1) = sigp(1)+lambdd*treps
        sig(2) = sigp(2)+lambdd*treps
        sig(3) = sigp(3)
        sig(4) = sigf(1)+lamfd*trot
        sig(5) = sigf(2)+lamfd*trot
        sig(6) = sigf(3)
    endif
!
! ---------------------------------------------------------------
!
! ----  CALCUL DE LA MATRICE TANGENTE
! ----  CALCUL DE LA PARTIE ELASTIQUE
! ----  CALCUL DE LA PARTIE ELASTIQUE DANS LE REPERE PROPRE
    if (rigi) then
        if (option(11:14) .eq. 'ELAS') then
            elas =.true.
            elas1 =.true.
            elas2 =.true.
        endif
!
        call r8inir(36, 0.d0, dspdep, 1)
!
        if (coup) then
            call r8inir(72, 0.d0, dsidep, 1)
        else
            call r8inir(36, 0.d0, dsidep, 1)
        endif
!
        de33i = -lambda*ksi2d/(deuxmu + lambda*ksi2d)
!
        do 100 k = 1, 2
            do 110 l = 1, 2
                dspdep(k,l) = lambdd + lambda*ksi2d*de33i
                dspdep(k+3,l+3) = lamfd
110         continue
100     continue
!
        do 120 k = 1, 2
            dspdep(k,k) = dspdep(k,k) + deumud(k)
            dspdep(k+3,k+3) = dspdep(k+3,k+3) + demudf(k)
120     continue
!
        if (abs(emp(1) - emp(2)) .le. r8prem()) then
            dspdep(3,3)=deumud(1)
        else
            dspdep(3,3)=(deumud(1)*emp(1)-deumud(2)*emp(2)) /(emp(1)-&
            emp(2))
        endif
!
        if (abs(efp(1) - efp(2)) .le. r8prem()) then
            dspdep(6,6)=demudf(1)
        else
            dspdep(6,6)=(demudf(1)*efp(1)-demudf(2)*efp(2)) /(efp(1)-&
            efp(2))
        endif
!
! -- CONTRIBUTION DISSIPATIVE
        if (.not. elas) then
!
            treps2 = treps*treps
            qm1 = 0.5d0*cof1(1)*treps2+q2d(1)
            qm2 = 0.5d0*cof1(2)*treps2+q2d(2)
!
! --  CALCUL DE LA DERIVEE DES CONTRAINTES MEMBRANES PAR RAPPORT A DA
!
            dndd(1,1) = dlmd1*treps+d1mud(1)*emp(1)+lambda*ksi2d* de33d1
            dndd(2,1) = dlmd1*treps+d1mud(2)*emp(2)+lambda*ksi2d* de33d1
            dndd(1,2) = dlmd2*treps+d2mud(1)*emp(1)+lambda*ksi2d* de33d2
            dndd(2,2) = dlmd2*treps+d2mud(2)*emp(2)+lambda*ksi2d* de33d2
!
! --  CALCUL DE LA DERIVEE DES CONTRAINTES FLEXION PAR RAPPORT A DA
!
            dmdd(1,1) = dlmfd1*trot + d1mudf(1) * efp(1)
            dmdd(2,1) = dlmfd1*trot + d1mudf(2) * efp(2)
            dmdd(1,2) = dlmfd2*trot + d2mudf(1) * efp(1)
            dmdd(2,2) = dlmfd2*trot + d2mudf(2) * efp(2)
!
            if ((.not. elas1) .and. (.not. elas2)) then
!
                a(1,1) = 2.0d0*(&
                         qm1/(1.0d0 + da1)**3 + qff(1)/(alf + da1)**3) - (cof1(1)*treps*de33d1+0.&
                         &5d0*cof2(1)*treps2+ dq2d(1)) /(1.d0 + da1&
                         )**2
                a(1,2) = -cof1(1)*treps*de33d2/(1.0d0 + da1)**2
!
                a(2,2) = 2.0d0*(&
                         qm2/(1.0d0 + da2)**3 + qff(2)/(alf + da2)**3) - (cof1(2)*treps*de33d2+0.&
                         &5d0*cof2(2)*treps2+ dq2d(2)) /(1.d0 + da2&
                         )**2
                a(2,1) = -cof1(2)*treps*de33d1/(1.0d0 + da2)**2
!
                call matinv('S', 2, a, ainv, deta)
!
                do 930 i = 1, 2
                    do 931 k = 1, 2
                        do 932 l = 1, 2
                            dspdep(l,k) = dspdep(l,k) -dndd(l,i)*( dndd(k,1)*ainv(i,1)+dndd(k,2)*&
                                          &ainv(i,2))
!
                            dspdep(l+3,k+3) = dspdep(l+3,k+3) -dmdd(l, i)*(dmdd(k,1)*ainv(i,1)+dm&
                                              &dd(k,2)*ainv(i, 2))
!
                            dspdep(l+3,k) = dspdep(l+3,k) - dmdd(l,i)* (dndd(k,1)*ainv(i,1)+dndd(&
                                            &k,2)*ainv(i,2))
!
                            dspdep(l,k+3) = dspdep(l,k+3) - dndd(l,i)* (dmdd(k,1)*ainv(i,1)+dmdd(&
                                            &k,2)*ainv(i,2))
932                     continue
931                 continue
930             continue
!
            else if (.not. elas1) then
!
                a(1,1) = 2.0d0*(&
                         qm1/(1.0d0 + da1)**3 + qff(1)/(alf + da1)**3) - (cof1(1)*treps*de33d1+0.&
                         &5d0*cof2(1)*treps2+ dq2d(1)) /(1.d0 + da1&
                         )**2
!
                ainv(1,1)=1.d0/a(1,1)
!
                do 950 k = 1, 2
                    do 951 l = 1, 2
                        dspdep(l,k)=dspdep(l,k)-dndd(l,1)*dndd(k,1)*&
                        ainv(1,1)
!
                        dspdep(l+3,k+3)=dspdep(l+3,k+3)-dmdd(l,1)*&
                        dmdd(k,1)* ainv(1,1)
!
                        dspdep(l+3,k) = dspdep(l+3,k) - dmdd(l,1)* dndd(k,1)* ainv(1,1)
!
                        dspdep(l,k+3) = dspdep(l,k+3) - dndd(l,1)* dmdd(k,1)* ainv(1,1)
951                 continue
950             continue
!
            else if (.not. elas2) then
!
                a(2,2) = 2.0d0*(&
                         qm2/(1.0d0 + da2)**3 + qff(2)/(alf + da2)**3) - (cof1(2)*treps*de33d2+0.&
                         &5d0*cof2(2)*treps2+ dq2d(2)) /(1.d0 + da2&
                         )**2
!
                ainv(2,2)=1.d0/a(2,2)
!
                do 960 k = 1, 2
                    do 961 l = 1, 2
                        dspdep(l,k)=dspdep(l,k)-dndd(l,2)*dndd(k,2)*&
                        ainv(2,2)
!
                        dspdep(l+3,k+3) = dspdep(l+3,k+3)-dmdd(l,2)* dmdd(k,2)* ainv(2,2)
!
                        dspdep(l+3,k) = dspdep(l+3,k) - dmdd(l,2)* dndd(k,2)* ainv(2,2)
!
                        dspdep(l,k+3) = dspdep(l,k+3) - dndd(l,2)* dmdd(k,2)* ainv(2,2)
961                 continue
960             continue
            endif
        endif
        call tanmgl(vmp, vfp, dspdep, dsidep)
    endif
end subroutine
