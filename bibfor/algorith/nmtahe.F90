subroutine nmtahe(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, iret)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'asterfort/nmtaac.h'
    include 'asterfort/nmtacr.h'
    include 'asterfort/nmtadp.h'
    include 'asterfort/nmtael.h'
    include 'asterfort/nmtama.h'
    include 'asterfort/nmtari.h'
    include 'asterfort/nmtarl.h'
    include 'asterfort/nmtasp.h'
    include 'asterfort/nmtaxi.h'
    integer :: kpg, ksp, ndim, imate
    character(len=*) :: fami
    character(len=16) :: compor(*), option
    real(kind=8) :: crit(*), instam, instap
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), vim(9), sigp(6), vip(9), dsidep(6, 6)
! ----------------------------------------------------------------------
!     REALISE LA LOI DE TAHERI POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
!
!
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE TANGENTE
!
! OUT IRET    : CODE RETOUR DE L'INTEGRATION DE LA LOI DE TAHERI
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ABSENCE DE CONVERGENCE
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX YY ZZ XY XZ YZ
! ----------------------------------------------------------------------
!
    integer :: ndimsi, niter, k, iret
    integer :: ind
!
    real(kind=8) :: rac2
    real(kind=8) :: matm(3), mat(14)
    real(kind=8) :: sigel(6), epm(6)
    real(kind=8) :: dp, sp, xi
    real(kind=8) :: f, g, fdp, fds, gdp, gds, fdx, gdx, dpmax, sig(6)
    real(kind=8) :: tang(6, 6)
    real(kind=8) :: det, dirdp, dirsp, dirxi, ener, min, rho, rhomax, interi
!
    parameter (rhomax = 2.d0, interi = 0.99999D0)
!
!
!
! - INITIALISATION
!
!    DIMENSION DES TENSEURS ET MISE AUX NORMES
    ndimsi = ndim*2
    rac2 = sqrt(2.d0)
    do 10 k = 4, ndimsi
        vim(2+k) = vim(2+k) * rac2
10  end do
!
!    LECTURE DES CARACTERISTIQUES
    call nmtama(fami, kpg, ksp, imate, instam,&
                instap, matm, mat)
!
!
!    CALCUL DES CONTRAINTES ELASTIQUES
    call nmtael(fami, kpg, ksp, imate, ndimsi,&
                matm, mat, sigm, epsm, deps,&
                epm, sigel, sigp)
!
!
! - CALCUL DES CONTRAINTES REELLES ET DES VARIABLES INTERNES
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
!      PREDICTION ELASTIQUE
        dp = 0.d0
        xi = 1.d0
        call nmtasp(ndimsi, crit, mat, sigel, vim,&
                    epm, dp, sp, xi, f,&
                    iret)
        if (iret .eq. 1) goto 9999
!
!      CHARGE
        if (f .gt. 0.d0) then
!
!        CALCUL DE DP : EQUATION SCALAIRE F=0 AVEC  0 < DP < DPMAX
            call nmtadp(ndimsi, crit, mat, sigel, vim,&
                        epm, dp, sp, xi, g,&
                        iret)
!
!        PLASTICITE CLASSIQUE (G<0)
            if (g .le. 0.d0) then
                call nmtaac(2, ndimsi, mat, sigel, vim,&
                            epm, dp, sp, xi, sigp,&
                            vip)
!
!        PLASTICITE A DEUX SURFACES (G>0) -> NEWTON
            else
!
!          ITERATIONS DE NEWTON
                sp = vim(2)
                xi = 1.d0
                do 200 niter = 1, int(crit(1))
!
!            DIRECTION DE DESCENTE
                    call nmtacr(2, ndimsi, mat, sigel, vim,&
                                epm, dp, sp, xi, f,&
                                g, fds, gds, fdp, gdp,&
                                fdx, gdx, dpmax, sig, tang)
                    det = fdp*gds - fds*gdp
                    dirdp = (g*fds - f*gds) / det
                    dirsp = (f*gdp - g*fdp) / det
                    dirxi = 0.d0
!
!            CORRECTION DE LA DIRECTION POUR RESTER DS P>P- ET S>SP>SP-
                    if (dp+rhomax*dirdp .lt. 0.d0) dirdp= ( -dp)/rhomax
                    if (sp+rhomax*dirsp .lt. vim(2)) dirsp= (vim(2) -sp ) /rhomax
                    if (sp+rhomax*dirsp .gt. mat(11)) dirsp= (mat(11)- sp )/rhomax
!
!            RECHERCHE LINEAIRE
                    ener = (f**2+g**2)/2.d0
                    min = (f*fdp+g*gdp)*dirdp + (f*fds+g*gds)*dirsp
                    rho = rhomax*interi
                    call nmtarl(2, ndimsi, mat, sigel, vim,&
                                epm, dp, sp, xi, dirdp,&
                                dirsp, dirxi, min, rho, ener)
!
!            ACTUALISATION
                    dp = dp + rho*dirdp
                    sp = sp + rho*dirsp
!
                    if (ener/mat(4)**2 .lt. crit(3)**2) goto 210
200              continue
                iret = 1
                goto 9999
210              continue
!
                call nmtaac(3, ndimsi, mat, sigel, vim,&
                            epm, dp, sp, xi, sigp,&
                            vip)
            endif
!
!
!      DECHARGE
        else
!
!        EXAMEN DE LA SOLUTION ELASTIQUE (XI=0)
            dp = 0.d0
            xi = 0.d0
            if (vim(9) .ne. 0.d0) then
                call nmtasp(ndimsi, crit, mat, sigel, vim,&
                            epm, dp, sp, xi, f,&
                            iret)
            endif
!
!        DECHARGE CLASSIQUE
            if (vim(9) .eq. 0.d0 .or. f .le. 0.d0) then
                call nmtaac(0, ndimsi, mat, sigel, vim,&
                            epm, dp, sp, xi, sigp,&
                            vip)
!
!
!
!        PSEUDO DECHARGE  -> EQUATION SCALAIRE F=0 AVEC  0<T<1
            else
!
!          CALCUL DE XI : EQUATION SCALAIRE F=0 AVEC  0 < XI < 1
                call nmtaxi(ndimsi, crit, mat, sigel, vim,&
                            epm, dp, sp, xi, g,&
                            iret)
!
!          ITERATIONS DE NEWTON
                if (g .gt. 0.d0) then
!
                    dp = 0.d0
                    sp = vim(2)
                    do 300 niter = 1, int(crit(1))
!
!              DIRECTION DE DESCENTE
                        call nmtacr(3, ndimsi, mat, sigel, vim,&
                                    epm, dp, sp, xi, f,&
                                    g, fds, gds, fdp, gdp,&
                                    fdx, gdx, dpmax, sig, tang)
                        det = fdx*gds - fds*gdx
                        dirxi = (g*fds - f*gds) / det
                        dirsp = (f*gdx - g*fdx) / det
                        dirdp = 0.d0
!
!              CORRECTION DIRECTION POUR RESTER DANS 0<XI<1 ET SP-<SP<S
                        if (xi+rhomax*dirxi .lt. 0.d0) dirxi=(-xi) / rhomax
                        if (xi+rhomax*dirxi .gt. 1.d0) dirxi=(1.d0-xi)/ rhomax
                        if (sp+rhomax*dirsp .lt. vim(2)) dirsp=(vim(2)- sp )/rhomax
                        if (sp+rhomax*dirsp .gt. mat(11)) dirsp=(mat(11)- sp )/rhomax
!
!              RECHERCHE LINEAIRE
                        ener = (f**2+g**2)/2.d0
                        min = (f*fdx+g*gdx)*dirxi + (f*fds+g*gds)* dirsp
                        rho = rhomax*interi
                        call nmtarl(3, ndimsi, mat, sigel, vim,&
                                    epm, dp, sp, xi, dirdp,&
                                    dirsp, dirxi, min, rho, ener)
!
!              ACTUALISATION
                        xi = xi + rho*dirxi
                        sp = sp + rho*dirsp
!
                        if (ener/mat(4)**2 .lt. crit(3)**2) goto 310
300                  continue
                    iret = 1
                    goto 9999
310                  continue
                endif
!
                call nmtaac(1, ndimsi, mat, sigel, vim,&
                            epm, dp, sp, xi, sigp,&
                            vip)
!
!
            endif
!
        endif
!
    endif
!
!
! -- RIGIDITE TANGENTE (FULL_MECA)
!
    if (option(1:9) .eq. 'FULL_MECA') then
        ind = int(vip(9)+0.5d0)
        call nmtari(ind, ndimsi, mat, sigel, vim,&
                    epm, dp, sp, xi, dsidep)
    endif
!
!
! -- RIGIDITE TANGENTE (RIGI_MECA_TANG) -> MATRICE ELASTIQUE
!
    if (option(1:14) .eq. 'RIGI_MECA_TANG') then
        ind = 0
        dp = 0.d0
        sp = vim(2)
        xi = 1.d0
        call nmtari(ind, ndimsi, mat, sigel, vim,&
                    epm, dp, sp, xi, dsidep)
    endif
!
!
! REMISE AUX NORMES
    if (option(1:14) .eq. 'RIGI_MECA_TANG') then
        do 400 k = 4, ndimsi
            vim(2+k) = vim(2+k) / rac2
400      continue
    else
        do 410 k = 4, ndimsi
            vim(2+k) = vim(2+k) / rac2
            vip(2+k) = vip(2+k) / rac2
410      continue
    endif
!
!
9999  continue
! FIN ------------------------------------------------------------------
end subroutine
