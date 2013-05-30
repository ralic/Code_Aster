subroutine irrmat(fami, kpg, ksp, model, imat,&
                  nmat, itmax, rela, materd, materf,&
                  matcst, ndt, ndi, nr, nvi)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-luc.flejou at edf.fr
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/irrnvi.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesr.h'
    character(len=8) :: model
    character(len=3) :: matcst
    character(len=*) :: fami
    integer :: imat, nmat, ndt, ndi, nr, nvi, kpg, ksp, iret, itmax
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), rela
!
!     ----------------------------------------------------------------
!     IRRAD3M   : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
!                 NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
!                MATER(*,1) = E , NU , ALPHA
!     ----------------------------------------------------------------
!     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!         IMAT   :  ADRESSE DU MATERIAU CODE
!         MODEL  :  TYPE DE MODELISATION
!         NMAT   :  DIMENSION  DE MATER
!         ITMAX  :  NOMBRE D ITERATION MAX
!         RELA   :  TOLERANCE RELATIVE DES VALEURS MATERIAUX
!         VIND   :  VARIABLES INTERNES A T
!     OUT MATERD :  COEFFICIENTS MATERIAU A T
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!                   MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
!                   MATER(*,2) = CARACTERISTIQUES   AUTRE
!         MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                   'NON' SINON
!         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!         NR     :  NB DE COMPOSANTES SYSTEME NL
!         NVI    :  NB DE VARIABLES INTERNES, DANS LE SYSTEME NL
!     ----------------------------------------------------------------
    integer :: iterat, nbcara
!     NOMBRE DE PARAMETRES DE LA LOI : NBCARA
    parameter   (nbcara = 12 )
    integer :: cerr(nbcara)
    character(len=8) :: nomcir(nbcara)
    real(kind=8) :: mat(nbcara)
    character(len=8) :: nomcel(3)
!
    real(kind=8) :: p0, irrad, irraf, pe, k, a, tempd, tempf
    real(kind=8) :: r02, eu, rm, ai0, etais, rg0, alpha, phi0, kappa, zetag
    real(kind=8) :: zetaf
    real(kind=8) :: n0, n1, f0, f1, fe, pasn, exph, exp0, spe, coeffa
!
    real(kind=8) :: valrm(12)
    integer :: valim(2)
    character(len=10) :: valkm(2)
!
    data pe    /2.0d-3/
!
    data nomcel /'E       ','NU      ','ALPHA   '/
!
    data nomcir /'R02     ','EPSI_U  ','RM      ','AI0     ',&
     &             'ETAI_S  ','RG0     ','ALPHA   ','PHI0    ',&
     &             'KAPPA   ','ZETA_F  ','ZETA_G  ','TOLER_ET'/
!
!     NOM                         a t-                 a t+ (t-+dt)
!     -------------------------------------------------------------
!     E                           MATERD(1,1)          MATERF(1,1)
!     NU                          MATERD(2,1)          MATERF(2,1)
!     ALPHA                       MATERD(3,1)          MATERF(3,1)
!
!     AI0                         MATERD(4,2)          MATERF(4,2)
!     ETAI_S                      MATERD(5,2)          MATERF(5,2)
!     AG                          MATERD(6,2)          MATERF(6,2)
!     K                           MATERD(7,2)          MATERF(7,2)
!     N                           MATERD(8,2)          MATERF(8,2)
!     P0                          MATERD(9,2)          MATERF(9,2)
!     KAPPA                       MATERD(10,2)         MATERF(10,2)
!     R02                         MATERD(11,2)         MATERF(11,2)
!     ZETAF                       MATERD(12,2)         MATERF(12,2)
!     PENTE EN PE                 MATERD(13,2)         MATERF(13,2)
!     PK                          MATERD(14,2)         MATERF(14,2)
!     PE                          MATERD(15,2)         MATERF(15,2)
!     CONTRAINTE EN PE            MATERD(16,2)         MATERF(16,2)
!     ZETAG                       MATERD(17,2)         MATERF(17,2)
!
!     IRRADIATION                 MATERD(18,2)         MATERF(18,2)
!     AGINT                       MATERD(19,2)         MATERF(19,2)
!
!     TOLER SUR SEUIL             MATERD(20,2)         MATERF(20,2)
!     ERREUR SUR SEUIL            MATERD(21,2)         MATERF(21,2)
!
!     TEMPERATURE                 MATERD(22,2)         MATERF(22,2)
!
!     INCREMENT IRRADIATION       MATERD(23,2)         MATERF(23,2)
!     INCREMENT TEMPERATURE       MATERD(24,2)         MATERF(24,2)
!
!
! -   PROTECTION SUR LA DIMENSION DES TABLEAUX : MATERD MATERF
    call assert(nmat.ge.30)
!
! -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
    call irrnvi(model, ndt, ndi, nr, nvi)
!
! === ================================================
!
!     RECUPERATION MATERIAU A TEMPD ET IRRAD
!
! === ================================================
!     CARACTERISTIQUES ELASTIQUES A TEMP- ET IRRA-
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS', 0, ' ', 0.0d0,&
                3, nomcel, materd(1, 1), cerr, 1)
!
!     TEMPERATURE A T-
    call rcvarc('F', 'TEMP', '-', fami, kpg,&
                ksp, tempd, iret)
!     IRRADIATION A T-
    call rcvarc('F', 'IRRA', '-', fami, kpg,&
                ksp, irrad, iret)
!     CARACTERISTIQUES MATERIAU A TEMP- ET IRRA-
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'IRRAD3M', 0, ' ', 0.0d0,&
                nbcara, nomcir, mat, cerr, 1)
!
!     POUR PLUS DE CLARETE, JE RENOMME LES GRANDEURS
    if (cerr(10) .eq. 0) then
        zetaf = mat(10)
    else
        zetaf = 1.0d0
    endif
    if (cerr(11) .eq. 0) then
        zetag = mat(11)
    else
        zetag = 1.0d0
    endif
    r02 = mat(1)
    eu = mat(2)
    rm = mat(3)
    ai0 = mat(4)
    etais = mat(5)
    rg0 = mat(6)
    alpha = mat(7)
    phi0 = mat(8)
    kappa = mat(9)
!
!     CALCUL DE LA PUISSANCE PAR DICHOTOMIE
!       - LA FONCTION EST MONOTONE DECROISSANTE
!       - NORMALISATION PAR R02
    coeffa = rm*exp(eu)/r02
!     F(n) = 1.0 - RM*EXP(EU)*((PE+n-EU)**n)/((n**n)*R02)
!     Finf = Limite F(n)       Fzero = Limite F(n)
!            n->infini                 n->0+
    n0 = eu - pe
    f1 = 1.0d0 - coeffa*exp(-n0)
!     L'équation peut ne pas avoir de solution, pour le vérifier on
!     calcule sa valeur FE à la 1ère borne de recherche +PE/10000.0
!     C'est avec FE que l'on vérifie que l'on à des solutions
    if (n0 .ge. 0.0d0) then
        n1 = n0 + pe/1000.0d0
    else
        n1 = pe/1000.0d0
    endif
    fe = 1.0d0 - coeffa*((n1-n0)**n1)/(n1**n1)
    if ((fe*f1.gt.0.0d0) .or. (n0.eq.0.0d0)) then
!        VALEURS PAR DEFAUT
        n1 = eu
!        VALEUR DE K , N
        if (n1 .gt. 0.0d0) then
            materd(7,2) = rm*exp(eu)/(n1**n1)
            materd(8,2) = n1
        else
            materd(7,2) = rm
            materd(8,2) = 0.0d0
        endif
!        VALEUR DE P0
        materd(9,2) = 0.0d0
!        -----------------
        k = materd(7,2)
        spe = k*(pe**n1)
        a = n1*k*(pe**(n1-1.d0))
    else
        if (n0 .gt. 0.0d0) then
            f0 = 1.0d0
            pasn = n0/10.0d0
            n1 = n0 - (pasn*0.9999d0)
        else
            f0 = 1.0d0 - coeffa
            pasn = pe/10.0d0
            n1 = - (pasn*0.9999d0)
        endif
        iterat = 0
!        WHILE TRUE
10      continue
        n1 = n1 + pasn
        f1 = 1.0d0 - coeffa*((n1-n0)**n1)/(n1**n1)
        if (abs(f1) .le. rela) goto 12
        iterat=iterat+1
        if (iterat .gt. itmax) then
            valkm(1) = 'PREMIERE'
            valim(1) = itmax
            valrm(1) = f0
            valrm(2) = f1
            valrm(3) = n1
            valrm(4) = pasn
            valrm(5) = rm
            valrm(6) = eu
            valrm(7) = r02
            valrm(8) = rela
!              VALEURS INITIALES
            valrm(9) = eu - pe
            valrm(10) = 1.0d0 - coeffa*exp(pe-eu)
            valrm(11) = 1.0d0 - coeffa
            valrm(12) = fe
            call u2mesg('F', 'COMPOR1_55', 1, valkm, 1,&
                        valim, 12, valrm)
        endif
        if (f1*f0 .gt. 0.0d0) then
            f0 = f1
        else
            n1 = n1 - pasn
            pasn = pasn * 0.5d0
        endif
        goto 10
12      continue
!        VALEUR DE K
        materd(7,2) = rm*exp(eu)/(n1**n1)
!        VALEUR DE N
        materd(8,2) = n1
!        VALEUR DE P0
        materd(9,2) = n1 - eu
!        ---------------------
        k = materd(7,2)
        p0 = materd(9,2)
        spe = k*((pe+p0)**n1)
        a = n1*k*((pe+p0)**(n1-1.d0))
    endif
    if (a .gt. 0.0d0) then
!        VALEUR DE LA PENTE EN PE
        materd(13,2) = a
!        VALEUR DE PK
        materd(14,2) = pe - (spe - kappa*r02)/a
    else
!        VALEUR DE LA PENTE EN PE
        materd(13,2) = 0.0d0
!        VALEUR DE PK
        materd(14,2) = 0.0d0
    endif
!     VALEUR DE AI0
    materd(4,2) = ai0
!     VALEUR DE ETAI_S
    materd(5,2) = etais
!     VALEUR DE AG
    exph = exp(alpha*(phi0-irrad))
    materd(6,2) = rg0/(1.0d0+exph)/3.0d0
!     VALEUR DE KAPPA
    materd(10,2) = kappa
!     VALEUR DE R02
    materd(11,2) = r02
!     VALEUR DE ZETAF
    materd(12,2) = zetaf
!     VALEUR DE PE
    materd(15,2) = pe
!     VALEUR DE LA CONTRAINTE EN PE
    materd(16,2) = spe
!     VALEUR DE ZETAG
    materd(17,2) = zetag
!     IRRADIATION
    materd(18,2) = irrad
!     VALEUR DE AG DEJA INTEGRE
    if (alpha .gt. 0.0d0) then
        exp0 = exp(alpha*phi0)
        exph = exp(alpha*irrad)
        materd(19,2) = rg0*log((exp0+exph)/(1.0d0+exp0))/(3.0d0*alpha)
    else
        materd(19,2) = 0.0d0
    endif
!     TOLERENCE ET ERREUR SUR LE FRANCHISSEMENT DU SEUIL
    materd(20,2) = mat(12)
    materd(21,2) = 0.0d0
!     TEMPERATURE
    materd(22,2) = tempd
!
! === ================================================
!
!     RECUPERATION MATERIAU A TEMPF ET IRRAF
!
! === ================================================
!     CARACTERISTIQUES ELASTIQUES A TEMP+ ET IRRA+
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'ELAS', 0, ' ', 0.0d0,&
                3, nomcel, materf(1, 1), cerr, 1)
!
!     TEMPERATURE A T+
    call rcvarc('F', 'TEMP', '+', fami, kpg,&
                ksp, tempf, iret)
!     IRRADIATION A T+
    call rcvarc('F', 'IRRA', '+', fami, kpg,&
                ksp, irraf, iret)
!     L'IRRADIATION NE PEUT PAS DECROITRE
    if (irrad .gt. irraf*1.00001D0) then
        valrm(1) = tempd
        valrm(2) = tempf
        call u2mesr('I', 'COMPOR1_57', 2, valrm)
        valrm(1) = irrad
        valrm(2) = irraf
        call u2mesr('I', 'COMPOR1_56', 2, valrm)
    endif
    if (irrad .gt. irraf) then
        irraf = irrad
    endif
!     CARACTERISTIQUES MATERIAU A TEMP+ ET IRRA+
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'IRRAD3M', 0, ' ', 0.0d0,&
                nbcara, nomcir, mat, cerr, 1)
!
!     POUR PLUS DE CLARETE
    if (cerr(10) .eq. 0) then
        zetaf = mat(10)
    else
        zetaf = 1.0d0
    endif
    if (cerr(11) .eq. 0) then
        zetag = mat(11)
    else
        zetag = 1.0d0
    endif
    r02 = mat(1)
    eu = mat(2)
    rm = mat(3)
    ai0 = mat(4)
    etais = mat(5)
    rg0 = mat(6)
    alpha = mat(7)
    phi0 = mat(8)
    kappa = mat(9)
!
!     CALCUL DE LA PUISSANCE PAR DICHOTOMIE
!       - LA FONCTION EST MONOTONE DECROISSANTE
!       - NORMALISATION PAR R02
    coeffa = rm*exp(eu)/r02
!     F(n) = 1.0 - RM*EXP(EU)*((PE+n-EU)**n)/((n**n)*R02)
!     Finf = Limite F(n)       Fzero = Limite F(n)
!            n->infini                 n->0+
    n0 = eu - pe
    f1 = 1.0d0 - coeffa*exp(-n0)
!     L'équation peut ne pas avoir de solution, pour le vérifier on
!     calcule sa valeur FE à la 1ère borne de recherche +PE/1000.0
!     C'est avec FE que l'on vérifie que l'on à des solutions
    if (n0 .ge. 0.0d0) then
        n1 = n0 + pe/1000.0d0
    else
        n1 = pe/1000.0d0
    endif
    fe = 1.0d0 - coeffa*((n1-n0)**n1)/(n1**n1)
    if ((fe*f1.ge.0.0d0) .or. (n0.eq.0.0d0)) then
!        VALEURS PAR DEFAUT
        n1 = eu
!        VALEUR DE K , N
        if (n1 .gt. 0.0d0) then
            materf(7,2) = rm*exp(eu)/(n1**n1)
            materf(8,2) = n1
        else
            materf(7,2) = rm
            materf(8,2) = 0.0d0
        endif
!        VALEUR DE P0
        materf(9,2) = 0.0d0
!        -----------------
        k = materf(7,2)
        spe = k*(pe**n1)
        a = n1*k*(pe**(n1-1.d0))
    else
        if (n0 .gt. 0.0d0) then
            f0 = 1.0d0
            pasn = n0/10.0d0
            n1 = n0 - (pasn*0.9999d0)
        else
            f0 = 1.0d0 - coeffa
            pasn = pe/10.0d0
            n1 = - (pasn*0.9999d0)
        endif
        iterat = 0
!        WHILE TRUE
20      continue
        n1 = n1 + pasn
        f1 = 1.0d0 - coeffa*((n1-n0)**n1)/(n1**n1)
        if (abs(f1) .le. rela) goto 22
        iterat=iterat+1
        if (iterat .gt. itmax) then
            valkm(1) = 'DEUXIEME'
            valim(1) = itmax
            valrm(1) = f0
            valrm(2) = f1
            valrm(3) = n1
            valrm(4) = pasn
            valrm(5) = rm
            valrm(6) = eu
            valrm(7) = r02
            valrm(8) = rela
!              VALEURS INITIALES
            valrm(9) = eu - pe
            valrm(10) = 1.0d0 - coeffa*exp(pe-eu)
            valrm(11) = 1.0d0 - coeffa
            valrm(12) = fe
            call u2mesg('F', 'COMPOR1_55', 1, valkm, 1,&
                        valim, 12, valrm)
        endif
        if (f1*f0 .gt. 0.0d0) then
            f0 = f1
        else
            n1 = n1 - pasn
            pasn = pasn * 0.5d0
        endif
        goto 20
22      continue
!        VALEUR DE K
        materf(7,2) = rm*exp(eu)/(n1**n1)
!        VALEUR DE N
        materf(8,2) = n1
!        VALEUR DE P0
        materf(9,2) = n1 - eu
!        ---------------------
        k = materf(7,2)
        p0 = materf(9,2)
        spe = k*((pe+p0)**n1)
        a = n1*k*((pe+p0)**(n1-1.d0))
    endif
    if (a .gt. 0.0d0) then
!        VALEUR DE LA PENTE EN PE
        materf(13,2) = a
!        VALEUR DE PK
        materf(14,2) = pe - (spe - kappa*r02)/a
    else
!        VALEUR DE LA PENTE EN PE
        materf(13,2) = 0.0d0
!        VALEUR DE PK
        materf(14,2) = 0.0d0
    endif
!     VALEUR DE AI0
    materf(4,2) = ai0
!     VALEUR DE ETAI_S
    materf(5,2) = etais
!     VALEUR DE AG
    exph = exp(alpha*(phi0-irraf))
    materf(6,2) = rg0/(1.0d0+exph)/3.0d0
!     VALEUR DE KAPPA
    materf(10,2) = kappa
!     VALEUR DE R02
    materf(11,2) = r02
!     VALEUR DE ZETAF
    materf(12,2) = zetaf
!     VALEUR DE PE
    materf(15,2) = pe
!     VALEUR DE LA CONTRAINTE EN PE
    materf(16,2) = spe
!     VALEUR DE ZETAG
    materf(17,2) = zetag
!     IRRADIATION
    materf(18,2) = irraf
!     VALEUR DE AG DEJA INTEGRE
    if (alpha .gt. 0.0d0) then
        exp0 = exp(alpha*phi0)
        exph = exp(alpha*irraf)
        materf(19,2) = rg0*log((exph+exp0)/(1.0d0+exp0))/(3.0d0*alpha)
    else
        materf(19,2) = 0.0d0
    endif
!     TOLERENCE ET ERREUR SUR LE FRANCHISSEMENT DU SEUIL
    materf(20,2) = mat(12)
    materf(21,2) = 0.0d0
!     TEMPERATURE
    materf(22,2) = tempf
!
!     INCREMENT IRRADIATION
    materd(23,2) = materf(18,2) - materd(18,2)
    materf(23,2) = materd(23,2)
!     INCREMENT TEMPERATURE
    materd(24,2) = materf(22,2) - materd(22,2)
    materf(24,2) = materd(24,2)
!
! -   MATERIAU CONSTANT ?
! -   ON NE PEUT PAS SAVOIR A L AVANCE DONC NON
    matcst = 'NON'
end subroutine
