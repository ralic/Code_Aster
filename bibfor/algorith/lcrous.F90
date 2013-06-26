subroutine lcrous(fami, kpg, ksp, toler, itmax,&
                  imat, nmat, materd, materf, nvi,&
                  deps, sigd, vind, theta, loi,&
                  dt, sigf, vinf, irtet)
    implicit none
!       ================================================================
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
!       ================================================================
!       INTEGRATION DE LA LOI DE ROUSSELIER
!
!       VIN = (P,F,INDICATEUR DE PLASTICITE)
!       ----------------------------------------------------------------
!
!       IN  FAMI   :  FAMILLE DU POINT DE GAUSS
!           KPG    :  POINT DE GAUSS
!           KSG    :  SOUS-POINT DE GAUSS
!           TOLER  :  TOLERANCE DE CONVERGENCE LOCALE NEWT
!           ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           NVI    :  NB VARIABLES INTERNES
!           DEPS   :  INCREMENT DE DEFORMATION
!           SIGD   :  CONTRAINTE A T
!           VIND   :  VARIABLES INTERNES A T
!           THETA  :  PARAMETRE THETA DE LA THETA-METHODE
!           LOI    :  MODELE DE COMPORTEMENT
!           DT     :  INTERVALLE DE TEMPS DT
!       OUT SIGF   :  CONTRAINTE A T+DT
!           VINF   :  VARIABLES INTERNES A T+DT
!           IRTET  :  CONTROLE DU REDECOUPAGE INTERNE DU PAS DE TEMPS
!
    include 'asterfort/lchydr.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcnrte.h'
    include 'asterfort/lcnrts.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcsomh.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/rsliso.h'
    include 'asterfort/rslphi.h'
    include 'asterfort/utexcp.h'
    integer :: kpg, ksp, imat, nmat, irtet, itmax, ncompt, nvi
    integer :: nint, testcv, convp
!
    real(kind=8) :: mun, zero, un, deux, trois, d13, ann, dt
    real(kind=8) :: toler, delta, d, s1, acc
    real(kind=8) :: p, pi, dp, f0, f, fi, df
    real(kind=8) :: ddp, rp, drdp
    real(kind=8) :: df1, df2, ddf, fitot, ftot
    real(kind=8) :: petit, ddfm, moyddf
    real(kind=8) :: phi, phi1, phi2, phip, phi1p, phi2p
    real(kind=8) :: nu, e, deuxmu, troimu, troisk, theta
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    real(kind=8) :: num, em, deumum, troikm
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    real(kind=8) :: deps(6), depsmo, depsdv(6)
    real(kind=8) :: sigd(6), sigf(6), sig0, eps0, mexpo
    real(kind=8) :: rigd(6), rigdmo, rigddv(6), rigdd2(6)
    real(kind=8) :: rigf(6), rigfdv(6)
    real(kind=8) :: rigm0, rigm, argmin, argmax
    real(kind=8) :: rigeq, rigel(6), rieleq, dsig
    real(kind=8) :: vind(nvi), vinf(nvi)
    real(kind=8) :: materf(nmat, 2), materd(nmat, 2), unrhod, rhof
    real(kind=8) :: ndeps, nsigd, demuth
    real(kind=8) :: seuil, dseuil, puiss, dpuiss, asinh
    real(kind=8) :: dp1, dp2, beta
    real(kind=8) :: terme1, terme2, terme3, sigeq, ebloc
!
    logical :: overfl
!
    parameter       ( mun  =-1.d0 )
    parameter       ( zero = 0.d0 )
    parameter       ( un   = 1.d0 )
    parameter       ( deux = 2.d0 )
    parameter       ( d13 = .3333333333d0 )
    parameter       ( trois = 3.d0 )
!
    character(len=*) :: fami
    character(len=16) :: loi
!
!       ---------------------------------------------------------------
!
! -- INITIALISATION-----------------------------------------------
!
    nu = materf(2,1)
    e = materf(1,1)
    d = materf(1,2)
    s1 = materf(2,2)
    f0 = materf(3,2)
!CCCCCCCCCCCCCCCCCCCCCCCCC
    num = materd(2,1)
    em = materd(1,1)
    deumum = em/(un+num)
    troikm = em/(un-deux*num)
!CCCCCCCCCCCCCCCCCCCCCCCCC
    if (loi(1:10) .eq. 'ROUSS_VISC') then
        ann = 0.d0
        beta = materf(8,2)
        sig0 = materf(9,2)
        eps0 = materf(10,2)
        mexpo = materf(11,2)
    else if (loi(1:10).eq.'ROUSS_PR') then
        ann = materf(8,2)
        beta= materf(9,2)
        sig0 = 0.d0
        eps0 = 0.d0
        mexpo = 0.d0
    endif
    deuxmu = e/(un+nu)
    demuth = deuxmu*theta
    troisk = e/(un-deux*nu)
    troimu = 1.5d0*deuxmu
    pi = vind(1)
    fi = vind(2)
    fitot =fi + ann*pi
!
! -- CAS DU MATERIAU CASSE----------------------------------------
    if (fitot .ge. materf(6,2)) then
        ndeps = lcnrte(deps)
        nsigd = lcnrts(sigd)
        dsig = materf(7,2)*e*ndeps
        if (dsig .ge. nsigd) then
            call lcinve(zero, sigf)
        else
            call lcprsv(un-dsig/nsigd, sigd, sigf)
        endif
        vinf(1) = pi
        vinf(2) = un
        vinf(3) = 0.d0
        vinf(4) = 0.d0
        vinf(nvi) = un
        irtet=0
        goto 9999
    endif
!
! ---INTEGRATION IMPLICITE DE LA LOI DE COMPORTEMENT-------------
!
! -- DEPSMO : INCREMENT DE DEFORMATION MOYENNE
! -- DEPSDV : INCREMENT DE DEFORMATION DEVIATORIQUE
    call lchydr(deps, depsmo)
    call lcsomh(deps, -depsmo, depsdv)
! -- REDECOUPAGE SI L'INCREMENT DE DEFORMATION EST TROP GRAND
    if (depsmo .gt. 10.d0) then
        goto 50
    endif
!
! -- RIG : CONTRAINTE REDUITE
    unrhod = (un-f0)/(un-fitot)
    call lcprsv(unrhod, sigd, rigd)
!
! -- RIGDMO : CONTRAINTE MOYENNE      REDUITE PRECEDENT
! -- RIGDDV : CONTRAINTE DEVIATORIQUE REDUITE PRECEDENT
    call lchydr(rigd, rigdmo)
    call lcsomh(rigd, -rigdmo, rigddv)
    call lcsomh(rigd, -rigdmo, rigdd2)
! -- CALCUL DE RIELEQ
!CCCCCCCCCCCCCCCCCCCCCCCCCCC
    call lcprsv((demuth+(un-theta)*deumum)/deumum, rigddv, rigddv)
    call lcprsv(deuxmu/deumum, rigdd2, rigdd2)
    rigdmo = rigdmo *(theta*troisk+(un-theta)*troikm)/troikm
!CCCCCCCCCCCCCCCCCCCCCCCCCCC
    call lcprsv(demuth, depsdv, rigel)
    call lcsove(rigddv, rigel, rigel)
    rieleq = lcnrts(rigel)
!
! ---CAS DU MATERIAU A POROSITE ACCELEREE--
    if (fitot .ge. materf(4,2)) then
        acc = materf(5,2)
    else
! ---CAS DU MATERIAU SAIN------------------
        acc = un
    endif
!
! ---DEBUT RESOLUTION----------------------
!
! - CONTROLE ARGUMENT DE L EXPO : DF1 ET DF2
    df1 = zero
!
    argmax = 200.d0
    rigm0 = rigdmo+troisk*depsmo
!
    if ((rigm0/s1) .gt. argmax) then
        df1 = (un-fi)*(rigm0-argmax*s1) /(theta*troisk/(trois*acc)+ rigm0-argmax*s1)
    endif
    argmin = -50.d0
    df2 = (un-fi)*(rigm0-argmin*s1) /(theta*troisk/(trois*acc)+rigm0-argmin*s1)
! -- SI POROSITE NULLE : VON MISES
    if (fi .eq. 0.d0) df2 = -10.d0
!
! -- SI POINT EN COMPRESSION : VON MISES
    if ((df2.lt.zero) .or. (df2.gt.(un-fi))) then
        rhof = un/unrhod
        df = zero
        f = fi
        rigm = rigdmo + troisk*theta*(depsmo - df/(trois*(un-f)*acc))
        dp = 0.05d0*pi
        ncompt = 0
        convp = 1
        if (loi(1:10) .eq. 'ROUSS_VISC') then
            convp = 0
        endif
        dp1 = 0.d0
! -- BOUCLE SUR DP
11      continue
        ncompt = ncompt + 1
        p = pi + theta*dp
        call rsliso(fami, kpg, ksp, '+', imat,&
                    p, rp, drdp)
        rigeq = rieleq - troimu*theta*dp
        phi = rigeq - rp + d*s1*f*exp(rigm/s1)
        phip = -(troimu + drdp)*theta
        if ((loi(1:10).eq.'ROUSS_VISC') .and. (convp.eq.1)) then
            seuil = phi
            dseuil = phip
            if (seuil .gt. zero) then
                puiss = (dp/(dt*eps0))**(un/mexpo)
                dpuiss = ((dp/(dt*eps0))**(un/mexpo-un))/(mexpo*(dt* eps0))
                asinh = log(puiss + sqrt(un + puiss**2))
                phi = seuil - sig0*asinh
                phip = dseuil - sig0*dpuiss/sqrt(un+puiss**2)*theta
            endif
            if (phi .gt. zero) dp1 = dp
        endif
        if (phi .lt. zero) dp2 = dp
!
! -- SI CONVERGENCE
        if (abs(phi/s1) .lt. toler) then
            if (convp .eq. 1) then
                goto 21
            else
                dp2 = dp
                convp = 1
            endif
        endif
! -- SI RECHERCHE TROP LONGUE
        if (ncompt .ge. itmax) then
            goto 60
        endif
! -- SINON CONTINUER
        ddp = - phi/phip
! -- BORNE INF CONTROLEE
        if ((dp+ddp) .lt. 0.d0) then
            dp = (dp1 + dp2)/deux
        else
            dp = dp + ddp
        endif
        goto 11
21      continue
        p = pi + dp
        irtet = 0
        goto 20
    endif
! -- CALCUL DE PHI1 ET PHI2
    call rslphi(fami, kpg, ksp, loi, imat,&
                troisk, troimu, depsmo, rigdmo, rieleq,&
                pi, d, s1, ann, theta,&
                acc, fi+df1, df1, sig0, eps0,&
                mexpo, dt, phi1, phi1p, rigeq,&
                rigm, p, overfl)
    if (overfl) goto 45
    call rslphi(fami, kpg, ksp, loi, imat,&
                troisk, troimu, depsmo, rigdmo, rieleq,&
                pi, d, s1, ann, theta,&
                acc, fi+df2, df2, sig0, eps0,&
                mexpo, dt, phi2, phi2p, rigeq,&
                rigm, p, overfl)
    if (overfl) goto 45
    if ((phi1.lt.0.d0) .or. (phi2.gt.0.d0)) then
        goto 50
    endif
! -- INITIALISATION DES INCREMENTS
    if (loi(1:10) .eq. 'ROUSS_VISC') then
        df = df2
        phi = phi2
        phip = phi2p
    else
        df = df1
        phi = phi1
        phip = phi1p
    endif
! -
    delta = un
    ncompt= 0
    nint = 0
    ddfm = 0.d0
    moyddf = 0.d0
    testcv = 1
    petit = 1.d-12
!
! -- BOUCLE PRINCIPALE---------------
10  continue
!
! -- CALCUL DE L INCREMENT
    ncompt=ncompt+1
    ddf = -phi/phip
! - CONTROLE VITESSE EVOLUTION DES DDF?
    nint = nint + 1
    moyddf = moyddf + (ddf-ddfm)
    ddfm = ddf
! - CALCUL DE DF
! - CONTROLE CONV NEWTON
    if (nint .eq. 5) then
! - SI NEWTON LENT : DICHOTOMIE POUR LA SUITE
        moyddf = moyddf*testcv/nint
        if (moyddf .le. petit) then
            df= df1 + (df2 - df1)/deux
            nint = 4
            testcv = 0
        else
            nint = 0
            moyddf = 0.d0
        endif
    endif
! - SI TESTS PRECEDENTS OK : NEWTON + BORNES CONTROLEES
    if (testcv .eq. 1) then
! - DF1<DF+DDF<DDF2? SINON CORDE
        if (((delta*ddf).le.zero) .or. ((delta*ddf).ge.(df2-df1))) then
            df= (phi1*df2 - phi2*df1)/(phi1-phi2)
        else
            df=df+ddf
        endif
    endif
    f=fi+df
    call rslphi(fami, kpg, ksp, loi, imat,&
                troisk, troimu, depsmo, rigdmo, rieleq,&
                pi, d, s1, ann, theta,&
                acc, f, df, sig0, eps0,&
                mexpo, dt, phi, phip, rigeq,&
                rigm, p, overfl)
    if (overfl) goto 45
!
! -- SI CONVERGENCE
    if (abs(phi/s1) .lt. toler) goto 20
    if (((df2-df1).lt.1.d-15) .and. (ncompt.eq.itmax)) then
        goto 20
    endif
!
! -- SI RECHERCHE TROP LONGUE
    if (ncompt .ge. itmax) then
        goto 60
    endif
!
! -- SINON CONTINUER
    if (phi .gt. zero) then
        df1 = df
        phi1 = phi
        delta= un
    else
        df2 = df
        phi2 = phi
        delta= mun
    endif
    goto 10
!
! -- CONVERGENCE---------------
20  continue
! -- CALCUL DE RIELEQ AVEC THETA =1----
    call lcprsv(deuxmu, depsdv, rigel)
    call lcsove(rigddv, rigel, rigel)
    rieleq = lcnrts(rigel)
    dp=p-pi
    rigeq = rieleq - troimu*dp
    rigm = rigdmo + troisk*(depsmo - d13*df/((un -f)*acc))
    vinf(1) = p
    ftot = f + ann*p
    vinf(2) = f
    vinf(nvi) = un
    rhof = (un-ftot)/(un-f0)
    call lcprsv(rigeq/rieleq, rigel, rigfdv)
    call lcsomh(rigfdv, rigm, rigf)
    call lcprsv(rhof, rigf, sigf)
!
!    CALL DE LA DISSIPATION PLASTIQUE  (CF. NOTE HT-26/04/027)
!----------------------------------
    sigeq= rhof * rigeq
!
    terme1 = dp/dt * sigeq
    terme2 = rigm * rhof * df/(1.d0-f)/acc/dt
    terme3 = rhof*s1*log(f0/f*rhof) *df/dt
    ebloc = vind(4)+((1-beta)*terme1+terme3)*dt
!
    if (ebloc .ge. 0.d0) then
        vinf(3) = beta*terme1+terme2-terme3
        vinf(4)= ebloc
    else
        vinf(3)= terme1+terme2
        vinf(4)=0.d0
    endif
!
    irtet=0
    goto 9999
!
! -- ERREURS--------------------------------------------------------
45  continue
    call utexcp(23, 'COMPOR1_16')
    goto 9999
!
! -- PROBABLEMENT UN INCREMENT TROP GRAND DE DEFORMATION-----------
50  continue
!
    irtet=1
    goto 9999
!
! -- NON CONVERGENCE------------------------------------------------
60  continue
!
    irtet=1
    goto 9999
!
! ------------------------------------------------------------------
9999  continue
end subroutine
