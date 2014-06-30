subroutine nmveei(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  epsm, deps, sigm, vim, option,&
                  sigp, vip, dsidep, iret)
! ----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
#include "asterfort/assert.h"
#include "asterfort/lcdvmi.h"
#include "asterfort/lceqmn.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcmate.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprsm.h"
#include "asterfort/mgauss.h"
#include "asterfort/nmvecd.h"
#include "asterfort/nmvend.h"
#include "asterfort/nmveot.h"
#include "asterfort/nmveso.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
    integer :: ndim, imate, iret, kpg, ksp
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    real(kind=8) :: crit(*), instam, instap, tm, tp, tref
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), vim(*), sigp(6), vip(*), dsidep(6, 6)
! ----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
!     CHABOCHE AVEC ENDOMAGEMENT
!     METHODE ITERATIVE D'EULER IMPLICITE
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
! ----------------------------------------------------------------------
!-- ARGUMENTS
!------------
!
! IN  FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
! IN  KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
!               CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                         (ITER_INTE_MAXI == ITMAX)
!               CRIT(2) = TYPE DE JACOBIEN A T+DT
!                         (TYPE_MATR_COMP == MACOMP)
!                         0 = EN VITESSE     > SYMETRIQUE
!                         1 = EN INCREMENTAL > NON-SYMETRIQUE
!               CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                         (RESI_INTE_RELA == TOLER)
!               CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                         REDECOUPAGE LOCAL DU PAS DE TEMPS
!                         (RESI_INTE_PAS == 0)
!                         0 = PAS DE REDECOUPAGE
!                         N = NOMBRE DE PALIERS
!               CRIT(6) = TYPE D INTEGRATION LOCAL POUR LA LOI DE
!                         COMPORTEMENT (ALGO_INTE)
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE TANGENTE
! OUT IRET    : CODE RETOUR DE  L'INTEGRATION DE LA LDC
!                  IRET=0 => PAS DE PROBLEME
!                  IRET=1 => ABSENCE DE CONVERGENCE DANS L'INTEGRATION
!                            DE LA LOI VISCO PLASTIQUE DE CHABOCHE
!                            AVEC ENDOMAGEMENT
!
! ----------------------------------------------------------------------
! INFO    MATM          (*,1) = CARACTERISTIQUES ELASTIQUES A T-
!                       (*,2) = CARACTERISTIQUES PLASTIQUES A T-
!         MATE          (*,1) = CARACTERISTIQUES ELASTIQUES A T
!                       (*,2) = CARACTERISTIQUES PLASTIQUES A T
!         MATCST        'OUI' SI MATERIAU CST ENTRE T- ET T
!                       'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
!                       'NON' SINON
!         NDT            NB DE COMPOSANTES TOTALES DES TENSEURS
!                                = 6  3D
!                                = 4  AXIS  C_PLAN  D_PLAN
!         NDI            NB DE COMPOSANTES DIRECTES DES TENSEURS
!         NVI            NB DE VARIABLES INTERNES
!         NR             NB EQUATIONS SYSTEME INTEGRE A RESOUDRE
!     ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!     L'ORDRE :  XX YY ZZ XY XZ YZ         RR ZZ TT RZ
! ----------------------------------------------------------------------
    integer :: nb, np, ni, nr, nmat, un, nt, iret1
    real(kind=8) :: zero, dammax, det, epsthp, epsthm
    parameter  (nb = 6, np = 2, ni = 9, nr = 8, nt=3*nb)
!     NOMBRE DE COEF MATERIAU MAXIMUM
    parameter  (nmat = 90)
    parameter  ( un   = 1   )
    parameter  ( zero = 0.d0   )
    parameter  (dammax = 0.99d0)
!
    logical(kind=1) :: cplan
!
    integer :: itmax, i, ier, iter, iret2, iret3, iret4
    integer :: ndt, nvi, nrv, ndi, k, l, isimp
!
    real(kind=8) :: pgl(3, 3), angmas(3), toler, deltx, sumx, dt, se2
    real(kind=8) :: vind(ni), matm(nmat, 2), a(6, 6), b(6)
    real(kind=8) :: mate(nmat, 2), hook(6, 6), hookm(6, 6)
    real(kind=8) :: p(np), beta(nb), ep(nt), rm, dm, unmd
    real(kind=8) :: dsgde(nb, nb), dsgdb(nb, nb), dsgdp(nb, np)
    real(kind=8) :: rb(nb), rp(np), drbdb(nb, nb), drbdp(nb, np)
    real(kind=8) :: drpdb(np, nb), drpdp(np, np), drbde(nb, nb)
    real(kind=8) :: drpde(np, nb), epthm(nb), deltb, sumb
    real(kind=8) :: dbeta(nb), dp(np), dsedb(nb), dsedb2(nb, nb), se
!
    character(len=3) :: matcst
    character(len=16) :: loi
    character(len=11) :: meting
    character(len=8) :: mod, typma
    character(len=7) :: etatf(3)
!
!     POUR LCMATE (MONOCRISTAL)
    integer :: nbcomm(1), nfs, nsg
    integer :: numhsr(1)
    real(kind=8) :: toutms(1), hsr(1)
    character(len=24) :: cpmono(1)
!
    common /tdim/   ndt  , ndi
    common /meti/   meting
! ----------------------------------------------------------------------
!
!
!-- 1. INITIALISATIONS :
!----------------------
    itmax = int(crit(1))
    ier=0
    iret=0
!
    if (itmax .le. 0) itmax = -itmax
    toler = crit(3)
    loi = compor(1)
    mod = typmod(1)
    cplan = typmod(1) .eq. 'C_PLAN'
    meting = 'NEWTON'
    dt = instap - instam
    etatf(1) = 'ELASTIC'
    etatf(2) = 'EXPONEN'
    etatf(3) = 'DAMMAXN'
    call r8inir(nb, 0.d0, dsedb, 1)
!
!-- 1.1. INCONNUES DU MODELES
!----------------------------
    do 111 i = 1, nb
        beta(i) = zero
111  end do
!
    do 112 i = 1, np
        p(i) = zero
112  end do
!
!-- 1.2. RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
!        NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
!-----------------------------------------------------------------------
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret2)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret3)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret4)
    if (((iret2+iret3).eq.0) .and. (iret4.eq.1)) then
        call utmess('F', 'CALCULEL_31')
    endif
!
    call lcmate(fami, kpg, ksp, compor, mod,&
                imate, nmat, tm, tp, 0,&
                typma, hsr, matm, mate, matcst,&
                nbcomm, cpmono, angmas, pgl, itmax,&
                toler, ndt, ndi, nrv, crit,&
                nvi, vind, nfs, nsg, toutms,&
                1, numhsr, sigm)
    ASSERT(ndt.eq.nb.or.nvi.eq.ni.or.nrv.eq.nr)
    if ((iret2+iret3) .eq. 0) then
        epsthp = mate(3,1)*(tp-tref)
        epsthm = matm(3,1)*(tm-tref)
    else
        epsthp = 0.d0
        epsthm = 0.d0
    endif
!
!-- 1.3. OPERATEUR DE HOOK
!-------------------------
    call lcopli('ISOTROPE', mod, mate, hook)
!
    if (.not.(loi(1:4) .eq. 'ELAS' .or. loi .eq. 'VENDOCHAB' .or. loi .eq. 'VISC_ENDO_LEMA'&
        )) then
        call utmess('F', 'ALGORITH4_50', sk=loi)
    endif
!
!-- 1.4. DEFORMATIONS TOTALES, THERMIQUES ET VISCOPLASTIQUES
!-----------------------------------------------------------
!-- VARIABLES D'ETAT DU MODELE A T-
    rm = vim (nb+2)
    dm = vim (nb+3)
    if (dm .eq. un) dm=dammax
    do 141 i = 1, 3
        ep(i)=0.d0
        epthm(i)=0.d0
        ep(i)=ep(i)+epsthp
        epthm(i)=epthm(i)+epsthm
        ep(3+i)=0.d0
        epthm(3+i)=0.d0
141  end do
!
    if (compor(3) .eq. 'PETIT_REAC') then
        call lcopli('ISOTROPE', mod, matm, hookm)
        call r8inir(nb*nb, 0.d0, a, 1)
        call r8inir(nb, 0.d0, b, 1)
        if (ndim .eq. 2) then
            sigm(5)=0.d0
            sigm(6)=0.d0
        endif
        call lceqvn(nb, sigm, b)
        do 142 i = 1, nb
            do 142 k = 1, nb
                a(i,k) = a(i,k)+ (un-dm)*hookm(i,k)
142          continue
        call mgauss('NFVP', a, b, nb, nb,&
                    1, det, iret1)
!
        do 143 i = 1, nb
            ep(6+i)=0.d0
            ep(6+i) = ep(6+i)+ epsm(i)- b(i)- epthm(i)
143      continue
    else
        call lceqvn(nb, vim(1), ep(7))
    endif
    do 144 i = 1, nb
        ep(12+i) = epsm(i)+deps(i)
144  end do
!
! CALCUL DIRECT DE LA SOLUTION DANS LE CAS OU LES EQUATIONS SE
! REDUISENT A UNE SEULE : SI R_D=K_D ET ALPHA=BETA=0
!
    isimp=0
!
!-- 2. CALCULS:
!---------------
!              - DES RESIDUS (RB ET RP) ET LEURS DERIVEES
!              - DES VARIABLE D'ETAT
!              - DES CONTRAINTES ET DES DERIVEES
!              - ARCHIVAGE DES VARIABLES
!-----------------------------------------------------------------------
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if (loi .eq. 'VISC_ENDO_LEMA') then
            if (.not.cplan) then
                call nmvend(fami, kpg, ksp, matm, mate,&
                            nmat, dt, epsm, deps, sigm,&
                            vim, ndim, crit, dammax, etatf,&
                            p, np, beta, nb, iter,&
                            ier)
                isimp=1
                if (ier .gt. 0) then
                    goto 801
                else
                    call nmvecd(imate, mate, nmat, matcst, loi,&
                                hook, dt, tp, p, np,&
                                beta, nb, ep, rm, dm,&
                                dsgde, dsgdb, dsgdp, drbde, drpde,&
                                rb, rp, drbdb, drbdp, drpdb,&
                                drpdp, etatf, ier)
                    goto 230
                endif
            endif
        endif
        do 200 iter = 1, itmax
!
            call nmvecd(imate, mate, nmat, matcst, loi,&
                        hook, dt, tp, p, np,&
                        beta, nb, ep, rm, dm,&
                        dsgde, dsgdb, dsgdp, drbde, drpde,&
                        rb, rp, drbdb, drbdp, drpdb,&
                        drpdp, etatf, ier)
!
            if (ier .ne. 0) then
                goto 801
            endif
!
!-- 2.1. RESOLUTION DU SYSTEME
!               DRBDB(NB,NB) DRBDP(NB,NP)   DB(NB)    -RB(NB)
!                                         *        =
!               DRPDB(NP,NB) DRPDP(NP,NP)   DP(NP)    -RP(NP)
!
            call nmveso(rb, nb, rp, np, drbdb,&
                        drbdp, drpdb, drpdp, dp, dbeta,&
                        nr, cplan)
!
            if (cplan) then
                deps(3) = zero
                dbeta(3) = zero
            endif
!
!-- 2.3. TEST DE CONVERGENCE
!-------------------------
!
            deltb = zero
            sumb = zero
            do 210 i = 1, nb
                if (isimp .eq. 0) then
                    beta(i)=beta(i)+dbeta(i)
                else
                    isimp=0
                endif
                deltb=deltb+abs(dbeta(i))
                sumb=sumb+abs(beta(i))
210          continue
            if (sumb .gt. toler) deltb=deltb/sumb
!
            deltx = zero
            sumx = zero
            do 220 i = 1, np
                if (isimp .eq. 0) then
                    p(i)=p(i)+dp(i)
                else
                    isimp=0
                endif
                deltx=deltx+abs(dp(i))
                sumx=sumx+abs(p(i))
220          continue
!
            if (sumx .gt. toler) deltx=deltx/sumx
!
            deltx=max(deltx,deltb)
!
            if (deltx .lt. toler) goto 00230
!
200      continue
!-- NOMBRE D'ITERATIONS MAXI ATTEINT: ARRET DU PROGRAMME
        vip(nb+4) = dble(iter)
        goto 801
!
230      continue
        if (etatf(2) .eq. 'TANGENT') then
            call utmess('A', 'ALGORITH8_66')
        endif
!-- STOCKAGE DANS L'INDICATEUR DU NOMBRE D'ITERATIONS
        vip(nb+4) = max(vip(nb+4),dble(iter))
!
!-- 2.4 ACTUALISATION DES CONTRAINTES ET DES VARIABLES INTERNES
!--------------------------------------------------------------
        call lceqvn(nb, beta, sigp)
        vip(nb+2) = vim(nb+2) + dt * p(1)
        call lcdvmi(beta, 0.d0, se2, dsedb, dsedb2,&
                    se)
!
        if (etatf(3) .eq. 'DAMMAXO') then
            call utmess('A', 'ALGORITH8_67')
            vip(nb+3) = dammax
            vip(nb+1) = vim(nb+1) + dt * p(1)/(un-dammax)
            do 240 i = 1, nb
                vip(i) = vim(i) + dt * p(1)/(un-dammax) * dsedb(i)
240          continue
        else
            vip(nb+3) = vim(nb+3) + dt * p(2)
            vip(nb+1) = vim(nb+1) + dt * p(1)/(un-vip(nb+3))
            do 241 i = 1, nb
                vip(i) = vim(i) + dt * p(1)/(un-vip(nb+3)) * dsedb(i)
241          continue
        endif
!
    endif
!-- 3. MISE A JOUR DE L'OPERATEUR TANGENT
!----------------------------------------
!
    if (option .eq. 'FULL_MECA') then
!
        if (etatf(1) .eq. 'ELASTIC') then
            call lceqmn(nb, hook, dsidep)
        else
            if (typma .eq. 'COHERENT') then
                call nmveot(drbdb, drbdp, drpdb, drpdp, drbde,&
                            drpde, dsgde, dsgdb, dsgdp, np,&
                            nb, nr, dsidep)
            else
                ASSERT(.false.)
            endif
        endif
!         ENDIF
!
!-- RIGIDITE TANGENTE (RIGI_MECA_TANG) -> MATRICE ELASTIQUE
    else if (option.eq.'RIGI_MECA_TANG') then
        if (typma .eq. 'COHERENT') then
            call lceqmn(nb, hook, dsidep)
        else
            ASSERT(.false.)
        endif
!        ENDIF
!
!-- RIGIDITE TANGENTE (RIGI_MECA_ELAS,FULL_MECA_ELAS)->MATRICE ELASTIQUE
    else if (option(10:14).eq.'_ELAS') then
!             MATRICE SECANTE=MATRICE ELASTIQUE
!
        if (option .eq. 'FULL_MECA_ELAS') then
            unmd=1.d0 - vip(nb+3)
        else
            unmd=1.d0 - vim(nb+3)
        endif
!             MATRICE SECANTE=MATRICE ELASTIQUE*(1-D)
        call lcprsm(unmd, hook, dsidep)
    endif
!
!-- MODIFICATION EN CONTRAINTE PLANES POUR TENIR COMPTE DE
!   SIG3=0 ET DE LA CONSERVATION DE L'ENERGIE
    if (mod(1:6) .eq. 'C_PLAN') then
        do 310 k = 1, nb
            if (k .eq. 3) goto 00310
            do 320 l = 1, nb
                if (l .eq. 3) goto 00320
                dsidep(k,l)=dsidep(k,l) - 1.d0/dsidep(3,3)*dsidep(k,3)&
                *dsidep(3,l)
320          continue
310      continue
    endif
!
!
    goto 900
!
!-- ERREURS
!
801  continue
    iret = 1
900  continue
end subroutine
