subroutine hmlisa(perman, yachai, option, meca, thmc,&
                  ther, hydr, imate, ndim, dimdef,&
                  dimcon, nbvari, yamec, yate, addeme,&
                  adcome, advihy, advico, vihrho, vicphi,&
                  addep1, adcp11, addete, adcote, congem,&
                  congep, vintm, vintp, dsde, epsv,&
                  depsv, p1, dp1, t, dt,&
                  phi, rho11, phi0, sat, retcom,&
                  biot, rinstp)
!
! ======================================================================
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
! TOLE CRP_21
! ======================================================================
! ROUTINE HMLISA : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   DANS LE CAS OU THMC = 'LIQU_SATU'
! ======================================================================
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! COMMENTAIRE DE NMCONV :
!                       = 0 OK
!                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTAT
!                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! ======================================================================
    implicit none
    include 'asterfort/appmas.h'
    include 'asterfort/calor.h'
    include 'asterfort/capaca.h'
    include 'asterfort/dhdt.h'
    include 'asterfort/dhwdp1.h'
    include 'asterfort/dileau.h'
    include 'asterfort/dmdepv.h'
    include 'asterfort/dmwdp1.h'
    include 'asterfort/dmwdt.h'
    include 'asterfort/dqdeps.h'
    include 'asterfort/dqdp.h'
    include 'asterfort/dqdt.h'
    include 'asterfort/dspdp1.h'
    include 'asterfort/enteau.h'
    include 'asterfort/inithm.h'
    include 'asterfort/netbis.h'
    include 'asterfort/sigmap.h'
    include 'asterfort/thmrcp.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/viemma.h'
    include 'asterfort/viporo.h'
    include 'asterfort/virhol.h'
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yate
    integer :: adcome, adcp11, adcote, vihrho, vicphi
    integer :: addeme, addep1, addete, advihy, advico, retcom
    real(kind=8) :: congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nbvari), vintp(nbvari)
    real(kind=8) :: dsde(dimcon, dimdef), epsv, depsv, p1, dp1, t, dt
    real(kind=8) :: phi, rho11, phi0
    real(kind=8) :: rinstp
    character(len=16) :: option, meca, ther, thmc, hydr
    logical :: perman, yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i
    real(kind=8) :: epsvm, phim, rho11m, rho110, rho0, csigm, alp11
    real(kind=8) :: biot, k0, cs, alpha0, alpliq, cliq, cp11, sat
    real(kind=8) :: em, alp12, dpad
    real(kind=8) :: rho12, rho21, rho22, cp12, cp21, cp22, coeps, dsatp1
    real(kind=8) :: m11m, satm
    real(kind=8) :: eps
    parameter  ( eps = 1.d-21 )
    logical :: emmag
! ======================================================================
! --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
! ======================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid8, rbid10, rbid11, rbid12, rbid13, rbid14(3)
    real(kind=8) :: rbid15, rbid16, rbid17, rbid18, rbid19, rbid20
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid30, rbid31, rbid32
    real(kind=8) :: rbid33, rbid34, rbid35, rbid36, rbid37, rbid38
    real(kind=8) :: rbid39, rbid40, rbid45, rbid46, rbid47, rbid48, rbid49
    real(kind=8) :: rbid50, rbid51, r3bid(6)
    real(kind=8) :: dp2, signe
!
    logical :: net, bishop
!
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! =====================================================================
    call netbis(meca, net, bishop)
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, t, p1, rbid40, rbid6,&
                rbid7, rbid8, rbid10, rbid11, rho0,&
                csigm, biot, rbid12, sat, rbid13,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rho110, cliq, alpliq,&
                cp11, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid50, r3bid, rbid51, rinstp,&
                retcom)
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    emmag = .false.
    rho12 = 0.0d0
    rho21 = 0.0d0
    rho22 = 0.0d0
    cp12 = 0.0d0
    cp21 = 0.0d0
    cp22 = 0.0d0
    alp11 = 0.0d0
    alp12 = 0.0d0
    dp2 = 0.0d0
    dpad = 0.0d0
    signe = -1.0d0
    sat = 1.0d0
    satm = 1.0d0
    dsatp1 = 0.0d0
    m11m = congem(adcp11)
    retcom = 0
    rho11 = vintm(advihy+vihrho) + rho110
    rho11m = vintm(advihy+vihrho) + rho110
    phi = vintm(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
    if (em .gt. eps) then
        emmag = .true.
    endif
!
    if (emmag .and. yachai) call u2mess('F', 'CHAINAGE_5')
!
    call inithm(imate, yachai, yamec, phi0, em,&
                alpha0, k0, cs, biot, t,&
                epsv, depsv, epsvm)
! *********************************************************************
! *** LES VARIABLES INTERNES ******************************************
! *********************************************************************
    if ((option.eq.'RAPH_MECA') .or. (option.eq.'FORC_NODA') .or.&
        (option(1:9).eq.'FULL_MECA')) then
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
! =====================================================================
        if ((yamec.eq.1) .or. yachai) then
            call viporo(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, depsv, alpha0, dt, dp1,&
                        dp2, signe, sat, cs, biot,&
                        phi, phim, retcom)
        else if (yamec .eq. 2) then
            phi = vintp(advico+vicphi)
        endif
        if (emmag) then
            call viemma(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, dp1, dp2, signe, sat,&
                        em, phi, phim, retcom)
        endif
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE MASSE VOLUMIQUE DU FLUIDE ------
! --- SELON FORMULE DOCR ----------------------------------------------
! =====================================================================
        call virhol(nbvari, vintm, vintp, advihy, vihrho,&
                    rho110, dp1, dp2, dpad, cliq,&
                    dt, alpliq, signe, rho11, rho11m,&
                    retcom)
    endif
! =====================================================================
! --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
! =====================================================================
    if (retcom .ne. 0) then
        goto 30
    endif
! **********************************************************************
! *** LES CONTRAINTES GENERALISEES *************************************
! **********************************************************************
! ======================================================================
! --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ----------------------
! ======================================================================
    if (yate .eq. 1) then
! ======================================================================
! --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR --
! ======================================================================
        alp11 = dileau(sat,biot,phi,alpha0,alpliq)
! ======================================================================
! --- CALCUL DE LA CAPACITE CALORIFIQUE SELON FORMULE DOCR -------------
! --- RHO12, RHO21, RHO22 SONT NULLES ----------------------------------
! --- CP12, CP21, CP22 SONT NULLES -------------------------------------
! ======================================================================
        call capaca(rho0, rho11, rho12, rho21, rho22,&
                    sat, phi, csigm, cp11, cp12,&
                    cp21, cp22, k0, alpha0, t,&
                    coeps, retcom)
! =====================================================================
! --- PROBLEME LORS DU CALCUL DE COEPS --------------------------------
! =====================================================================
        if (retcom .ne. 0) then
            goto 30
        endif
! ======================================================================
! --- CALCUL DES ENTHALPIES SELON FORMULE DOCR -------------------------
! ======================================================================
        if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
            congep(adcp11+ndim+1) = congep(adcp11+ndim+1) + enteau(dt, alpliq,t,rho11,dp2,dp1,dpa&
                                    &d,signe,cp11)
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
! --- DP2 ET ALP12 SONT  NULLES ----------------------------------------
! ======================================================================
            congep(adcote) = congep(adcote) + calor(alpha0,k0,t,dt, depsv,dp1,dp2,signe,alp11,alp&
                             &12,coeps)
        endif
    endif
! ======================================================================
! --- CALCUL SI PAS RIGI_MECA_TANG -------------------------------------
! ======================================================================
    if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
! ======================================================================
! --- CALCUL DES CONTRAINTES DE PRESSIONS ------------------------------
! ======================================================================
        if (yamec .eq. 1) then
            congep(adcome+6)=congep(adcome+6) + sigmap(net,bishop,sat,&
            signe,biot,dp2,dp1)
        endif
! ======================================================================
! --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
! ======================================================================
        if (.not.perman) then
            congep(adcp11) = appmas( m11m,phi,phim,sat,satm,rho11, rho11m,epsv,epsvm)
        endif
    endif
!
! **********************************************************************
! *** CALCUL DES DERIVEES **********************************************
! **********************************************************************
! ======================================================================
! --- CALCUL DES DERIVEES PARTIELLES DES PRESSIONS SELON FORMULES DOCR -
! --- UNIQUEMENT POUR LES OPTIONS RIGI_MECA ET FULL_MECA ---------------
! ======================================================================
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (yamec .eq. 1) then
! ======================================================================
! --- CALCUL UNIQUEMENT EN PRESENCE DE MECANIQUE -----------------------
! ======================================================================
! --- CALCUL DES DERIVEES DE SIGMAP ------------------------------------
! ======================================================================
            dsde(adcome+6,addep1)=dsde(adcome+6,addep1) + dspdp1(net,&
            bishop,signe,biot,sat)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            if (.not.perman) then
                do 10 i = 1, 3
                    dsde(adcp11,addeme+ndim-1+i) = dsde(adcp11,addeme+ ndim-1+i) + dmdepv(rho11,s&
                                                   &at,biot)
10              continue
            endif
        endif
        if (yate .eq. 1) then
! ======================================================================
! --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
! ======================================================================
! --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
! ======================================================================
            dsde(adcp11+ndim+1,addep1)=dsde(adcp11+ndim+1,addep1)&
            + dhwdp1(signe,alpliq,t,rho11)
            dsde(adcp11+ndim+1,addete)=dsde(adcp11+ndim+1,addete)&
            + dhdt(cp11)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIE THERMIQUE ------------------------------
! ======================================================================
            dsde(adcp11,addete) = dsde(adcp11,addete) + dmwdt(rho11, phi,sat,cliq,0.0d0,alp11)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! ======================================================================
            dsde(adcote,addete)=dsde(adcote,addete)+dqdt(coeps)
            dsde(adcote,addep1)=dsde(adcote,addep1)+dqdp(signe,alp11,&
            t)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            if (yamec .eq. 1) then
                do 20 i = 1, 3
                    dsde(adcote,addeme+ndim-1+i) = dsde(adcote,addeme+ ndim-1+i) + dqdeps(alpha0,&
                                                   &k0,t)
20              continue
            endif
        endif
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- POUR LES AUTRES CAS ----------------------------------------------
! ======================================================================
        if (.not.perman) then
            dsde(adcp11,addep1) = dsde(adcp11,addep1) + dmwdp1(rho11, signe,sat,dsatp1,biot,phi,c&
                                  &s,cliq,1.0d0, emmag,em)
        endif
    endif
! ======================================================================
30  continue
! =====================================================================
end subroutine
