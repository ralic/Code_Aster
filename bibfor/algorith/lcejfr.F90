subroutine lcejfr(fami, kpg, ksp, ndim, mate,&
                  option, epsm, deps, sigma, dsidep,&
                  vim, vip, coorot, typmod, instam,&
                  instap)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: kyrylo.kazymyrenko at edf.fr
!
! aslint: disable=W1306
    implicit none
    include 'asterfort/matinv.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    integer :: mate, ndim, kpg, ksp
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigma(6), dsidep(6, 6)
    real(kind=8) :: vim(*), vip(*), instam, instap, coorot(ndim+ndim*ndim)
    character(len=8) :: typmod(*)
    character(len=16) :: option
    character(len=*) :: fami
!
!-----------------------------------------------------------------------
!     LOI DE COMPORTEMENT  MOHR-COULOMB
!     POUR LES ELEMENTS DE JOINT ET JOINT_HYME 2D ET 3D.
!
! DEUX TYPES DE CALCUL SONT POSSIBLES:
!
! 1) MODELISATIONS *_JOINT_HYME, AVEC PARAMETRE HYDRO PRESENTS
!    CALCUL COUPLE HYDRO-MECANIQUE SUR UN MAILLAGE QUADRATIQUE
!
! 2) MODELISATIONS *_JOINT, PAS DE PARAMETRES HYDRO POSSIBLE
!    CALCUL MECA (AVEC ENVENTUELLEMENT PRES_FLUIDE)
!    SUR UN MAILLAGE LINEAIRE OU QUADRATIQUE
!
! IN : EPSM SAUT INSTANT MOINS ET GRAD PRESSION ET PRES FLUIDE SI HYME
! IN : DEPS INCR DE SAUT ET INCR GRAD PRESS ET INC PRES FL SI HYME
! IN : MATE, OPTION, VIM, COOROT,INSTAM, INSTAP
! OUT : SIGMA , DSIDEP , VIP
!-----------------------------------------------------------------------
    integer :: nbpa
    parameter (nbpa=9)
    integer :: cod(nbpa)
    integer :: i, j, n, ifplas, kronec, ifouv
    real(kind=8) :: kn, kt, kappa, mu, adhe, a(ndim), plasti(ndim), dplas(ndim)
    real(kind=8) :: lambda, dlam, val(nbpa), criter
    real(kind=8) :: abstau, tau(ndim), coefd, coefhd, r8bid
    real(kind=8) :: inst, valpar(ndim+1), rhof, visf, amin, presfl, presg
    real(kind=8) :: gp(ndim-1), gploc(ndim), gpglo(ndim), fhloc(ndim)
    real(kind=8) :: fhglo(ndim)
    real(kind=8) :: invrot(ndim, ndim), rigart
    character(len=8) :: nom(nbpa), nompar(ndim+1)
    character(len=1) :: poum
    logical :: resi, rigi, elas, ifpahm, ifhyme
!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
! CALCUL DE CONTRAINTE (RESIDU)
    resi = option(1:9).eq.'FULL_MECA' .or. option.eq.'RAPH_MECA'
! CALCUL DE LA MATRICE TANGEANTE (RIGIDITE)
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
! CALCUL DE LA MATRICE ELASTIQUE A LA PLACE DE LA MATRICE TANGENTE
    elas = option.eq.'FULL_MECA_ELAS' .or. option.eq.'RIGI_MECA_ELAS'
!
! INDICATEUR AVEC/SANS HYDRO
    if (typmod(2) .eq. 'EJ_HYME') ifhyme=.true.
    if (typmod(2) .eq. 'ELEMJOIN') ifhyme=.false.
!
! #####################################
! RECUPERATION DES PARAMETRES PHYSIQUES
! #####################################
    nom(1) = 'K_N'
    nom(2) = 'MU'
    nom(3) = 'ADHESION'
    nom(4) = 'K_T'
    nom(5) = 'PENA_TANG'
    nom(6) = 'PRES_FLUIDE'
    nom(7) = 'RHO_FLUIDE'
    nom(8) = 'VISC_FLUIDE'
    nom(9) = 'OUV_MIN'
!
    if (option .eq. 'RIGI_MECA_TANG') then
        poum = '-'
    else
        poum = '+'
    endif
!
! INSTANT DE CALCUL T- OU T+
    inst = instam
    if (resi) inst = instap
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', 0, ' ', 0.d0,&
                3, nom, val, cod, 2)
! DEFINITION DE PARAMETRES PHYSIQUE:
!     PENTE ELASTIQUE NORMALE
    kn=val(1)
!     COEFFICIENT DE FROTTEMENT
    mu=val(2)
!     ADHESION
    adhe=val(3)
!
! PENTE ELASTIQUE TANGENTIELLE
! (SI ELLE N'EST PAS DEFINI ALORS K_T=K_N)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', 0, ' ', 0.d0,&
                1, nom(4), val(4), cod(4), 0)
    if (cod(4) .eq. 0) then
        kt = val(4)
    else
        kt = kn
    endif
! PARAMETRE PENA_TANG
! (SI IL N'EST PAS DEFINI ALORS KAPPA=(K_N+K_T)*1E-6)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', 0, ' ', 0.d0,&
                1, nom(5), val(5), cod(5), 0)
    if (cod(5) .eq. 0) then
        kappa = val(5)
    else
        kappa = (kn+kt)*1.d-6
    endif
!
!
! DEFINITION DES PARAMETRES POUR LA RECUPERATION DES FONCTIONS
    nompar(1)='INST'
    nompar(2)='X'
    nompar(3)='Y'
    valpar(1)= inst
    valpar(2)= coorot(1)
    valpar(3)= coorot(2)
    if (ndim .eq. 3) then
        nompar(4)='Z'
        valpar(4)= coorot(3)
    endif
!
! RECUPERATION DE LA PRESS FLUIDE IMPOSEE (FCT DE L'ESPACE ET DU TEMPS)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', ndim+1, nompar, valpar,&
                1, nom(6), val(6), cod(6), 0)
    if (cod(6) .eq. 0) then
        presfl = val(6)
    else
        presfl = 0.d0
    endif
!
! RECUPERATION DE LA MASSE VOL ET DE LA VISCO (MODELISATION JOINT HM)
!--------------------------------------------------------------------
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', 0, ' ', 0.d0,&
                1, nom(7), val(7), cod(7), 0)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', 0, ' ', 0.d0,&
                1, nom(8), val(8), cod(8), 0)
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'JOINT_MECA_FROT', 0, ' ', 0.d0,&
                1, nom(9), val(9), cod(9), 0)
!
    if (cod(7) .eq. 0) rhof = val(7)
    if (cod(8) .eq. 0) visf = val(8)
    if (cod(9) .eq. 0) amin = val(9)
!
! INDICATEUR SI LES PARAMETRES HYDRO SONT RENSEIGNES
    ifpahm = (cod(7).eq.0).and.(cod(8).eq.0).and.(cod(9).eq.0)
!
! VERIFICATION DE LA PRESENCE/ABSENCE DES PARAMETRES
! EN FONCTION DE LA MODELISATION MECA PUR OU HYDRO MECA
!
    if (ifhyme) then
!       POUR LE CALCUL HYDRO => PAS DE PRES_FLUIDE
        if ((cod(6).eq.0)) call u2mess('F', 'ALGORITH17_14')
!       POUR LE CALCUL HYDRO => PRESENCE DE PARA HYDRO
        if (.not.ifpahm) call u2mess('F', 'ALGORITH17_15')
    else
!       POUR LE CALCUL MECA => PAS DE PARAMETRE HYDRO
        if (ifpahm) call u2mess('F', 'ALGORITH17_16')
    endif
!
!
! #####################################
! INITIALISATION DE VARIABLES
! #####################################
!
! CALCUL DU SAUT EN T+ OU T- EN FOCTION DE L'OPTION DE CALCUL
!     A=AM
    call dcopy(ndim, epsm, 1, a, 1)
!     A=A+DA
    if (resi) call daxpy(ndim, 1.d0, deps, 1, a,&
                         1)
!
! GRADIENT DE PRESSION ET PRESSION EN T- OU T+
    if (ifhyme) then
!
        do 10 n = 1, ndim-1
            gp(n) = epsm(ndim+n)
            if (resi) gp(n) = gp(n) + deps(ndim+n)
10      continue
!
        presg = epsm(2*ndim)
        if (resi) presg = presg + deps(2*ndim)
!
    endif
!
! INITIALISATION DE VARIABLE INTERNES
!     LAMBDA = accumutation de deplacement tang en valeur absolue
!     PLASTI = vecteur de deplacement plastique tangentiel
    lambda=vim(1)
    do 11 i = 2, ndim
        plasti(i) = vim(i+1)
11  end do
!     IDICATEUR DE PLASTIFICATION A L'INSTANT ACTUEL
    if (elas) then
        ifplas = 0
    else
        ifplas = nint(vim(2))
    endif
!     INDICATEUR D'OUVERTURE
    ifouv=nint(vim(5))
!
! #####################################
! CALCUL DE LA CONTRAINTE
! #####################################
!
!     INITIALISATION DE LA CONTRAINTE A ZERO
    call r8inir(6, 0.d0, sigma, 1)
!
! CALCUL DE LA CONTRAINTE HYDRO : DEBIT (LOI CUBIQUE)
    if (ifhyme) then
        do 44 n = 1, ndim-1
            sigma(ndim+n) = -rhof*gp(n)*(max(amin,a(1)+amin))**3/(12* visf)
44      continue
    endif
!
!     CONTRAINTE NORMALE DE CONTACT PENALISE +
!     INDICATEUR D'OUVERTURE COMPLETE
!     (CONTRAINTE TANGENTIEL EST MISE A ZERO)
    if (kn*a(1) .lt. (adhe/mu)) then
        ifouv=0
        sigma(1) = kn*a(1)
    else
        ifouv=1
        sigma(1) = adhe/mu
    endif
!
!     CONTRAINTE TANGENTIELLE
    do 20 i = 2, ndim
        sigma(i) = kt*( a(i)-plasti(i) )
20  end do
!
!     DIRECTION DE GLISSEMENT = SIGMA TANG SANS INCREMENT PLASTIQUE
    do 27 i = 2, ndim
        tau(i) = sigma(i)
27  end do
!
!     MODULE DE SIGMA TANGENTE SANS INCREMENT PLASTIQUE
!     NB:SI ABSTAU==0 ON EST TOUJOURS DANS LE REGIME ELASTIQUE
    abstau=0.d0
    do 30 i = 2, ndim
        abstau = abstau+tau(i)**2
30  end do
    abstau=sqrt(abstau)
!
! ###########################################################
! SI ON CALCULE LA RIGIDITE SEULEMENT, ON Y SAUTE DIRECTEMENT
! ###########################################################
    if (.not. resi) goto 5000
!
!     CRITERE DE PLASTICITE  NB: SIGMA(1)<0 EN COMPRESSION
    criter=abstau+mu*sigma(1)-kappa*lambda - adhe
!     VERIFICATION DE CRITERE DE PLASTICITE
    if (criter .le. 0.d0) then
!     PAS DE PLASTICITE
        ifplas=0
        do 32 i = 2, ndim
            dplas(i) = 0.d0
32      continue
        dlam=0.d0
    else
!     AVEC LA PLASTICITE
        ifplas=1
        do 34 i = 2, ndim
            dplas(i) = criter/(kt+kappa)*tau(i)/abstau
            sigma(i) = sigma(i)-kt*dplas(i)
34      continue
        dlam=criter/(kt+kappa)
    endif
!
!     PRISE EN COMPTE DE LA PRESSION DE FLUIDE EVENTUELLE
!     PRESFL : IMPOSEE, PRESG : CALCULEE
!
    if (ifhyme) then
        sigma(1) = sigma(1) - presg
    else
        sigma(1) = sigma(1) - presfl
    endif
!
!
! ACTUALISATION DES VARIABLES INTERNES
!   V1 :  LE DEPLACEMENT PLASTIQUE CUMULE (SANS ORIENTATION) LAMBDA:
!            LAMBDA NE PEUX QU'AUGMENTER
!   V2 : INDICATEUR DE PLASTIFICATION (0 : NON, 1 : OUI)
!   V3-V4 :  VECTEUR DE DEPLACEMENT TANG PAR RAPPORT AU POINT DE DEPART
!                        (INDIQUE LA POSITION D'EQUILIBRE ACTUELLE)
!   V5    : INDICATEUR D'OUVERTURE COMPLETE
!                         (CONTRAINTE TANGENTIEL EST MIS A ZERO)
!   V6    : MODULE DE LA CONTRAINTE TANGENTE
!   V7 A V9 : VALEURS DU SAUT
!   V10 : PAS UTILISEE DANS CETTE LOI
!   V11 : CONTRAINTE MECANIQUE NORMALE (SANS PRESSION DE FLUIDE)
!   V12 A V14 : COMPOSANTES DU GRAD DE PRESSION DANS LE REPERE GLOBAL
!   V15 A V17 : COMPOSANTES DU FLUX HYDRO DANS LE REPERE GLOBAL
!   V18 : PRESSION DE FLUIDE INTERPOLEE
!
    vip(1) = lambda+dlam
    vip(2) = ifplas
    do 36 i = 2, ndim
        vip(i+1) = vim(i+1)+dplas(i)
36  end do
    vip(5) = max(nint(vim(5)),ifouv)
!
    vip(6)=vim(6)
    do 37 i = 2, ndim
        vip(6) = vip(6)+sigma(i)**2
        vip(6)=sqrt(vip(6))
37  end do
    vip(7) = a(1)
    do 38 i = 2, ndim
        vip(i+6)=a(i)
38  end do
!
    vip(10) = 0.d0
!
!     VISUALISATION DES FLUX, DES GRAD DE PRESSION ET DE LA PRESSION
!     DANS LE REPERE GLOBAL
    if (ifhyme) then
!
        gploc(1) = 0.d0
        gploc(2) = gp(1)
        if (ndim .eq. 3) then
            gploc(3) = gp(2)
        endif
!
        fhloc(1) = 0.d0
        fhloc(2) = sigma(ndim+1)
        if (ndim .eq. 3) then
            fhloc(3) = sigma(2*ndim-1)
        endif
!
        call matinv('S', ndim, coorot(ndim+1), invrot, r8bid)
        call pmavec('ZERO', ndim, invrot, gploc, gpglo)
        call pmavec('ZERO', ndim, invrot, fhloc, fhglo)
!
!       CONTRAINTE MECANIQUE NORMALE SANS PRESSION DE FLUIDE CALCULEE
!       ON ANNULE SON INFLUENCE
        vip(11) = sigma(1) + presg
!
        vip(12) = gpglo(1)
        vip(13) = gpglo(2)
        vip(15) = fhglo(1)
        vip(16) = fhglo(2)
!
        if (ndim .eq. 3) then
            vip(14) = gpglo(3)
            vip(17) = fhglo(3)
        else
            vip(14) = 0.d0
            vip(17) = 0.d0
        endif
!
!       PRESSION DE FLUIDE CALCULEE AUX NOEUDS (DDL) ET INTERPOL AU PG
        vip(18) = presg
!
    else
!
!       CONTRAINTE MECANIQUE NORMALE SANS PRESSION DE FLUIDE IMPOSEE
!       ON ANNULE SON INFLUENCE
        vip(11) = sigma(1) + presfl
!
        vip(12) = 0.d0
        vip(13) = 0.d0
        vip(14) = 0.d0
        vip(15) = 0.d0
        vip(16) = 0.d0
        vip(17) = 0.d0
!
!       PRESSION DE FLUIDE IMPOSEE AU PG :
        vip(18) = presfl
!
    endif
!
!
! #####################################
! CALCUL DE LA MATRICE TANGENTE
! #####################################
!
5000  continue
    if (.not. rigi) goto 9999
!
!     INITIALISATION DE DSIDEP
    call r8inir(6*6, 0.d0, dsidep, 1)
!
! CALCUL DE LA MATRICE TANGENTE HYDRO
!------------------------------------
!
    if (ifhyme) then
!
!       TERME : DW/DGP  (POUR KTAN P P)
        do 42 n = 1, ndim-1
!
            dsidep(ndim+n,ndim+n)=-rhof*(max(amin,a(1)+amin))**3/(12*&
            visf)
!
42      continue
!
!       TERME : DW/DDELTA_N  (POUR KTAN P U)
        do 43 n = 1, ndim-1
!
            if (a(1) .lt. 0.d0) then
                dsidep(ndim+n,1) = 0.d0
            else
                dsidep(ndim+n,1) = -3*rhof*gp(n)*(a(1)+amin)**2/(12* visf)
            endif
!
43      continue
!
    endif
!
! DSIGMA_N/DDELTA_N
    if (ifouv .eq. 0) then
        dsidep(1,1)=kn
    else
        dsidep(1,1)=0.d0
    endif
! DSIGMA_N/DDELTA_T
    do 40 i = 2, ndim
        dsidep(1,i)=0.d0
40  end do
! DSIGMA_T/DDELTA_N
    do 50 i = 2, ndim
        if ((ifouv.eq.0) .and. (ifplas.eq.1)) then
            dsidep(i,1)=-tau(i)*mu*kn*kt/abstau/(kt+kappa)
        else
            dsidep(i,1)=0.d0
        endif
50  end do
! DSIGMA_T/DDELTA_T
    if (ifplas .eq. 1) then
        coefhd= - (kappa*lambda+adhe-mu*sigma(1)) *kt**2/abstau**3/(&
        kt+kappa)
        coefd=kappa*kt/(kt+kappa) - coefhd*abstau**2
        do 60 j = 2, ndim
            do 70 i = j, ndim
                if (i .eq. j) then
                    kronec=1
                else
                    kronec=0
                endif
                dsidep(j,i) = coefhd*tau(j)*tau(i) + coefd*kronec
                dsidep(i,j) = coefhd*tau(i)*tau(j) + coefd*kronec
70          continue
60      continue
    else
        do 80 i = 2, ndim
            dsidep(i,i)=kt
80      continue
    endif
!
!
!CC MATRICE TANGENTE DE CONTACT OUVERT
!
! DANS LE CAS OU L'ELEMENT EST TOTALEMENT CASSE ON INTRODUIT UNE
! RIGIDITE ARTIFICIELLE DANS LA MATRICE TANGENTE POUR ASSURER
! LA CONVERGENCE
!
    rigart=1.d-8
!
!     POUR LE JOINT OUVERT LA PARTIE NORMALE EST CORRIGEE
    if (ifouv .eq. 1) then
!       COMPLETEMENT CASSE NORMALE
        dsidep(1,1) = kn*rigart
    endif
!     POUR LE JOINT SANS ECROUISSAGE LA PARTIE TANGENTIELLE EST DECALEE
    if (kappa .eq. 0.d0) then
!     COMPLETEMENT CASSE TANGENTE
        do 90 i = 2, ndim
            dsidep(i,i) = dsidep(i,i) + kt*rigart
90      continue
    endif
!
!
9999  continue
!
end subroutine
