subroutine nmchdp(crit, seuil, dp, iret, iter)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
!
!      NMCHDP   -- CETTE ROUTINE CONCERNE L'INTEGRATION DE LA LOI
!                  DE COMPORTEMENT 'VISC_CIN1_CHAB' OU VISC_CIN2_CHAB
!                  RESOLUTION DE L'EQUATION SCALAIRE NON LINEAIRE EN DP
!                  (INCREMENT DE DEFORMATION PLASTIQUE CUMULEE) :
!
!  ||(RP/DENOMI*SIGEDV - MP*GAMMAP*DP*
!                       (-2/3+DP/DENOMI*(2*MU+2/3*MP))*ALPHAM)|| = RP
!
!                  CETTE EQUATION EST RELATIVE AU MODELE DE CHABOCHE
!                  A UNE OU DEUX TENSEURS CINEMATIQUES
!                  ET ELLE EST RESOLUE PAR UNE METHODE DE SECANTES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MAT(8+2*NBVAR) IN    R       TABLEAU DES COEFFICIENTS
!                                 D'ECROUISSAGE DU MATERIAU
!    PM             IN    R       DEFORMATION PLASTIQUE CUMULEE A
!                                 L'INSTANT DU CALCUL PRECEDENT
!    NDIMSI         IN    I       DIMENSION DU VECTEUR DES CONTRAINTES
!                                 I.E. 4 EN 2D ET 6 EN 3D
!    SIGEDV(6)       IN    R       VECTEUR DES CONTRAINTES D'ESSAI, I.E.
!                                 SIGEDV = MU/(MU-)*SIGM +2MU*DELTA_EPS
!    NBVAR          IN    R       NOMBRE DE TENSEURS DE RAPPEL
!    EPSPM(6)       IN    R       DEFORMATION PLASTIQUE A L'INSTANT -
!    ALFAM(6)       IN    R       LE TENSEUR DE RAPPEL XM A L'INSTANT -
!    ALFA2M(6)                     DU CALCUL PRECEDENT EST RELIE
!                                 AU TENSEUR ALFAM PAR XM = 2/3*C*ALFAM
!    DEUXMU         IN    R       COEFFICIENT DE LAME :2*MU
!    CRIT(6)        IN    R       TABLEAU DE CRITERES LOCAUX
!                                 DE CONVERGENCE :
!                                 CRIT(1) : NOMBRE D'ITERATIONS
!                                 MAXIMUM A LA CONVERGENCE ...
!    SEUIL          IN    R       CRITERE DE PLASTICITE
!                                 SEUIL = F - RP
!    VISC           IN    I       INDICATEUR DE VISCOSITE
!    MEMO           IN    R       INDICATEUR EFFET DE MEMOIRE
!    DT             IN    R       VALEUR DE L'INCREMENT DE TEMPS DELTAT
!    RM             IN    R       R(PM)
!    QM             IN    R       Q(PM)
!    KSIM           IN    R       KSI(PM)
!    RP             OUT   R       R(PM+DP)
!    QP             OUT   R       Q(PM+DP)
!    KSIP           OUT   R       KSI(PM+DP)
!    DP             OUT   R       INCREMENT DE DEFORMATION PLASTIQUE
!                                 CUMULEE
!    IRET           OUT   I    CODE RETOUR DE  L'INTEGRATION DE LA LDC
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ABSENCE DE CONVERGENCE DANS
!                                        LORS DE L'INTEGRATION DE LA
!                                        LOI
!    ITER           OUT    I   NOMBRE D'ITERATIONS POUR CONVERGER
!
#include "asterfort/infniv.h"
#include "asterfort/nmchcr.h"
#include "asterfort/u2mess.h"
#include "asterfort/utlcal.h"
#include "asterfort/zerofr.h"
    integer :: ndimsi, nbvar, visc, memo, niter, i, iter, ifm, niv, nbp, iret
    integer :: idelta
    real(kind=8) :: mat(18), pm, sigedv(6), alfam(6), deuxmu, dp, dt, qp
    real(kind=8) :: ksip(6)
    real(kind=8) :: crit(*), seuil, alfa2m(6), z, zz, ksim(6), qm, dpe, n1, n2
    real(kind=8) :: beta1
    real(kind=8) :: zero, dix, epspm(6), dpmax1, prec, dpmax, ddp, rpvm, rpvp
    real(kind=8) :: beta2
    real(kind=8) :: cinf, k, w, c2inf, cm, c2m, kvi, valden, f0, fmax, depsp(6)
    character(len=8) :: nomvar(16)
    character(len=16) :: meth
    common/fchab/mat,pm,sigedv,epspm,alfam,alfa2m,deuxmu,rpvm,rpvp,&
     &    qm,qp,ksim,ksip,dt,n1,n2,depsp,&
     &    beta1,beta2,ndimsi,nbvar,visc,memo,idelta
    data nomvar/'R0','RINF','B','CINF','K','W','GAMMA0',&
     &'AINF','C2INF','GAMM20','KVI','N','ETA','QM','Q0','MU'/
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     ===============
    zero = 0.0d0
    dix = 10.0d0
    iret=0
! --- POUR CHERCHER LA SOLUTION, PREMIERE APPROXIMATION
    cinf = mat(4)
    k = mat(5)
    w = mat(6)
    c2inf = mat(9)
    cm = cinf * (1.d0 + (k-1.d0)*exp(-w*pm))
    c2m = c2inf *(1.d0 + (k-1.d0)*exp(-w*pm))
    dpmax = seuil/(1.5d0*deuxmu+cm+c2m)
    if (visc .eq. 1) then
        valden=mat(11)
        kvi =mat(12)
        dpmax1=dt*(seuil/kvi)**valden
        if (dpmax1 .lt. 1.d0) then
            dpmax=max(dpmax,dpmax1)
        endif
    endif
!
! --- EXAMEN DE LA SOLUTION DPE = 0 :
!     =============================
    dpe = zero
!
! --- CALCUL DE LA VALEUR F0 DE LA FONCTION DONT ON CHERCHE LA RACINE
! --- POUR DP = 0 :
!     -----------
    f0=nmchcr(dpe)
!
! --- NOMBRE D'ITERATIONS DONT ON DISPOSE POUR CONVERGER ET TOLERANCE
! --- SUR LA VALEUR CONVERGEE :
!     -----------------------
    niter = int(crit(1))
    prec = crit(3)
!
!     RECHERCHE DES BORNES 0-DPMAX
!
    if (abs(f0) .le. prec) then
        dp = dpe
        goto 50
    else if (f0.gt.zero) then
        call u2mess('A', 'ELEMENTS4_61')
        goto 41
    else
!
! ---   F0 < 0 , ON CHERCHE DPMAX TEL QUE FMAX < 0 :
!
        fmax=nmchcr(dpmax)
        if (abs(fmax) .le. prec) then
            dp = dpmax
            iter=1
            goto 50
        else if (fmax.gt.zero) then
!          FMAX > 0.
!          VERIFICATION QUE DPMAX N'EST PAS TROP GRAND. BRACKETTING
            do 31 i = 1, niter
                dpmax = dpmax/dix
                fmax=nmchcr(dpmax)
                if (abs(fmax) .le. prec) then
                    dp = dpmax
                    iter=i
                    goto 50
                else if (fmax.lt.zero) then
!                ON RECALCULE LA VALEUR PRECEDENTE DE DPMAX
                    dpmax = dpmax*dix
                    fmax=nmchcr(dpmax)
                    goto 20
                endif
31          continue
            goto 20
!
        else
!          FMAX <0. On augmente DPMAX jusqu'à ce que F(DPMAX) > 0
            do 30 i = 1, niter
                fmax=nmchcr(dpmax)
                if (abs(fmax) .le. prec) then
                    dp = dpmax
                    iter=i
                    goto 50
                else if (fmax.gt.zero) then
                    goto 20
                else
                    dpmax = dpmax*dix
                endif
30          continue
            call u2mess('A', 'ALGORITH6_79')
            goto 20
        endif
!
    endif
!
20  continue
!
! --- CALCUL DE LA SOLUTION DE L'EQUATION F = 0 :
!     ===========================================
!
!     RECUPERATION DE L'ALGORITHME DE RESOLUTION 1D
!
    call utlcal('VALE_NOM', meth, crit(6))
!
!     PREC RELATIVE CAR EQUATION NORMEE
!     RESOLUTION 1D
    call zerofr(0, meth, nmchcr, 0.d0, dpmax,&
                prec, niter, dp, iret, iter)
    if (iret .eq. 0) goto 50
!
41  continue
!
!     CAS DE NON CONVERGENCE : IMPRESSIONS SI INFO=2
    call infniv(ifm, niv)
    if (niv .eq. 2) then
        write (ifm,*) 'MODELE CINX_CHAB : ATTENTION'
        write (ifm,*) 'PAS DE CONVERGENCE A LA PRECISION DEMANDEE',&
        prec
        write (ifm,*) 'AU BOUT DU NOMBRE D ITERATION DEMANDE',niter
        write (ifm,*) 'VALEURS DE DP ',dp
        write (ifm,*) 'AUGMENTER ITER_INTE_MAXI'
        write (ifm,*) 'PARAMETRES :'
        do 61 i = 1, 16
            write (ifm,*) nomvar(i),mat(i)
61      continue
        nbp = 20
        ddp = dpmax/nbp
        write (ifm,*) 'DP     -     F(DP)'
        z=zero
        do 60 i = 1, nbp
            zz=nmchcr(z)
            write (ifm,*) z,zz
            z = z + ddp
60      continue
    endif
    iret = 1
!
50  continue
!
end subroutine
