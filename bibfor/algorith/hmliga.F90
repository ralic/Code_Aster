subroutine hmliga(yachai, option, meca, ther, hydr,&
                  imate, ndim, dimdef, dimcon, nbvari,&
                  yamec, yate, addeme, adcome, advihy,&
                  advico, vihrho, vicphi, vicsat, addep1,&
                  adcp11, addep2, adcp21, addete, adcote,&
                  congem, congep, vintm, vintp, dsde,&
                  deps, epsv, depsv, p1, p2,&
                  dp1, dp2, t, dt, phi,&
                  rho11, phi0, sat, retcom, thmc,&
                  crit, biot, rinstp)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ROUTINE HMLIGA : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   DANS LE CAS OU THMC = 'LIQU_GAZ'
! ======================================================================
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! COMMENTAIRE DE NMCONV :
!                       = 0 OK
!                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! ======================================================================
    implicit none
    include 'asterfort/appmas.h'
    include 'asterfort/calor.h'
    include 'asterfort/capaca.h'
    include 'asterfort/dhdt.h'
    include 'asterfort/dhwdp1.h'
    include 'asterfort/dhwdp2.h'
    include 'asterfort/dileau.h'
    include 'asterfort/dilgaz.h'
    include 'asterfort/dmasp1.h'
    include 'asterfort/dmasp2.h'
    include 'asterfort/dmdepv.h'
    include 'asterfort/dmwdp1.h'
    include 'asterfort/dmwdp2.h'
    include 'asterfort/dmwdt.h'
    include 'asterfort/dqdeps.h'
    include 'asterfort/dqdp.h'
    include 'asterfort/dqdt.h'
    include 'asterfort/dspdp1.h'
    include 'asterfort/dspdp2.h'
    include 'asterfort/enteau.h'
    include 'asterfort/entgaz.h'
    include 'asterfort/inithm.h'
    include 'asterfort/masvol.h'
    include 'asterfort/netbis.h'
    include 'asterfort/nmbarc.h'
    include 'asterfort/sigmap.h'
    include 'asterfort/thmrcp.h'
    include 'asterfort/viemma.h'
    include 'asterfort/viporo.h'
    include 'asterfort/virhol.h'
    include 'asterfort/visatu.h'
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yate, retcom
    integer :: adcome, adcp11, adcp21, adcote, addeme, addep1, addep2
    integer :: addete, advihy, advico, vihrho, vicphi, vicsat
    real(kind=8) :: congem(dimcon), congep(dimcon), vintm(nbvari)
    real(kind=8) :: vintp(nbvari), dsde(dimcon, dimdef), epsv, depsv
    real(kind=8) :: p1, dp1, p2, dp2, t, dt, phi, rho11, phi0, rinstp
    character(len=16) :: option, meca, ther, hydr, thmc
    logical :: yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i
    real(kind=8) :: satm, epsvm, phim, rho11m, rho21m, rho110
    real(kind=8) :: biot, k0, cs, alpha0, alpliq, cliq
    real(kind=8) :: cp11, cp21, sat, dsatp1, mamolg, rho21, em
    real(kind=8) :: r, rho0, csigm, alp11, alp12, alp21
    real(kind=8) :: eps
    parameter  ( eps = 1.d-21 )
    logical :: emmag
! ======================================================================
! --- VARIABLES LOCALES POUR BARCELONE-------------------------------
! ======================================================================
    real(kind=8) :: tini, crit(*)
    real(kind=8) :: dsidp1(6), deps(6)
    real(kind=8) :: dsdeme(6, 6)
!CCC    SIP NECESSAIRE POUR CALCULER LES CONTRAINTES TOTALES
!CCC    ET ENSUITE CONTRAINTES NETTES POUR BARCELONE
    real(kind=8) :: sipm, sipp
! ======================================================================
! --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
! ======================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid8, rbid10, rbid14(3)
    real(kind=8) :: rbid15, rbid16, rbid17, rbid18, rbid19, rbid20
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid32
    real(kind=8) :: rbid33, rbid34, rbid35, rbid36, rbid37, rbid38
    real(kind=8) :: rbid39, rbid45, rbid46, rbid47, rbid48, rbid49
    real(kind=8) :: rbid50, rbid51, r3bid(6)
    real(kind=8) :: signe, m11m, m21m, coeps, rho12, rho22, dpad, cp12, cp22
!
    logical :: net, bishop
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! =====================================================================
    call netbis(meca, net, bishop)
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, t, p1, p1-dp1, rbid6,&
                rbid7, rbid8, rbid10, r, rho0,&
                csigm, biot, satm, sat, dsatp1,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rho110, cliq, alpliq,&
                cp11, rbid26, rbid27, rbid28, rbid29,&
                mamolg, cp21, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid50, r3bid, rbid51, rinstp,&
                retcom)
! ======================================================================
! --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
! --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
! --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
! ======================================================================
    emmag = .false.
!
    cp12 = 0.0d0
    cp22 = 0.0d0
    alp11 = 0.0d0
    alp12 = 0.0d0
    alp21 = 0.0d0
    rho12 = 0.0d0
    rho21 = 0.0d0
    rho22 = 0.0d0
    signe = 1.0d0
    dpad = 0.0d0
    retcom = 0
    m11m = congem(adcp11)
    m21m = congem(adcp21)
    rho11 = vintm(advihy+vihrho) + rho110
    rho11m = vintm(advihy+vihrho) + rho110
    phi = vintm(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
    if ((em.gt.eps) .and. (yamec.eq.0)) then
        emmag = .true.
    endif
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
        if ((yamec.eq.1)) then
            call viporo(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, depsv, alpha0, dt, dp1,&
                        dp2, signe, sat, cs, biot,&
                        phi, phim, retcom)
        endif
        if (emmag) then
            call viemma(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, dp1, dp2, signe, sat,&
                        em, phi, phim, retcom)
        endif
!
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE MASSE VOLUMIQUE DU FLUIDE ------
! --- SELON FORMULE DOCR ----------------------------------------------
! =====================================================================
        call virhol(nbvari, vintm, vintp, advihy, vihrho,&
                    rho110, dp1, dp2, dpad, cliq,&
                    dt, alpliq, signe, rho11, rho11m,&
                    retcom)
! =====================================================================
! --- RECUPERATION DE LA VARIABLE INTERNE DE SATURATION ---------------
! =====================================================================
        call visatu(nbvari, vintp, advico, vicsat, sat)
! =====================================================================
! --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
! =====================================================================
        if (retcom .ne. 0) then
            goto 30
        endif
    endif
! **********************************************************************
! *** LES CONTRAINTES GENERALISEES *************************************
! **********************************************************************
! ======================================================================
! --- CALCUL DES MASSES VOLUMIQUES DE PRESSION DE VAPEUR ---------------
! ----------------------------------- AIR SEC --------------------------
! ----------------------------------- AIR DISSOUS ----------------------
! ======================================================================
    rho21 = masvol(mamolg,p2 ,r,t )
    rho21m = masvol(mamolg,p2-dp2,r,t-dt)
! =====================================================================
! --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
! =====================================================================
    if (yate .eq. 1) then
! =====================================================================
! --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
! =====================================================================
        alp11 = dileau(sat,biot,phi,alpha0,alpliq)
        alp12 = dilgaz(sat,biot,phi,alpha0,t )
        alp21 = dilgaz(sat,biot,phi,alpha0,t )
! ======================================================================
! --- CALCUL DE LA CAPACITE CALORIFIQUE SELON FORMULE DOCR -------------
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
            congep(adcp21+ndim+1) = congep(adcp21+ndim+1) + entgaz(dt, cp21)
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
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
        congep(adcp11) = appmas( m11m,phi,phim,sat,satm,rho11, rho11m, epsv,epsvm)
        congep(adcp21) = appmas(&
                         m21m, phi, phim, 1.0d0-sat, 1.0d0-satm, rho21, rho21m, epsv, epsvm)
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
            dsde(adcome+6,addep2)=dsde(adcome+6,addep2) +dspdp2(net,&
            bishop,biot)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            do 10 i = 1, 3
                dsde(adcp11,addeme+ndim-1+i) = dsde(adcp11,addeme+ ndim-1+i) + dmdepv(rho11,sat,b&
                                               &iot)
                dsde(adcp21,addeme+ndim-1+i) = dsde(adcp21,addeme+ ndim-1+i) + dmdepv(rho21,1.0d0&
                                               &-sat,biot)
10          continue
        endif
        if (yate .eq. 1) then
! ======================================================================
! --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
! ======================================================================
! --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
! ======================================================================
            dsde(adcp11+ndim+1,addep2) = dsde(adcp11+ndim+1,addep2) + dhwdp2(alpliq,t,rho11)
            dsde(adcp11+ndim+1,addep1)=dsde(adcp11+ndim+1,addep1)&
            + dhwdp1(signe,alpliq,t,rho11)
            dsde(adcp11+ndim+1,addete)=dsde(adcp11+ndim+1,addete)&
            + dhdt(cp11)
            dsde(adcp21+ndim+1,addete)=dsde(adcp21+ndim+1,addete)&
            + dhdt(cp21)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
! ======================================================================
            dsde(adcp11,addete) = dsde(adcp11,addete) + dmwdt(rho11, phi,sat,cliq,0.0d0,alp11)
            dsde(adcp21,addete) = dsde(adcp21,addete) + dmwdt(rho21, phi,sat,cliq,0.0d0,alp21)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! ======================================================================
            dsde(adcote,addete)=dsde(adcote,addete)+dqdt(coeps)
            dsde(adcote,addep1)=dsde(adcote,addep1)+dqdp(signe,alp11,&
            t)
            dsde(adcote,addep2)=dsde(adcote,addep2) - dqdp(signe,&
            alp11+alp12,t)
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
        dsde(adcp11,addep1) = dsde(adcp11,addep1) + dmwdp1(rho11, signe,sat,dsatp1,biot,phi,cs,cl&
                              &iq,1.0d0, emmag,em)
        dsde(adcp11,addep2) = dsde(adcp11,addep2) + dmwdp2(rho11,sat, biot,phi,cs,cliq,1.0d0, emm&
                              &ag,em)
        dsde(adcp21,addep1) = dsde(adcp21,addep1) + dmasp1(rho11, 0.0d0,rho21,sat,dsatp1,biot,phi&
                              &,cs,1.0d0, emmag,em)
        dsde(adcp21,addep2) = dsde(adcp21,addep2) + dmasp2(rho11, 0.0d0,rho21,sat,biot,phi,cs,p2,&
                              & emmag,em)
    endif
! =====================================================================
! --- TERMES SPECIAL BARCELONE --------------------------------------
! =====================================================================
    if ((yamec.eq.1) .and. (meca.eq.'BARCELONE')) then
        tini = t-dt
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
        call nmbarc(ndim, imate, crit, sat, biot,&
                    tini, t, deps, congem(adcome), vintm,&
                    option, congep(adcome), vintp, dsdeme, p1,&
                    p2, dp1, dp2, dsidp1, sipm,&
                    sipp, retcom)
        if (retcom .eq. 1) goto 30
!
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
! --- DSIGM/DEPP1
            do 50 i = 1, 2*ndim
                dsde(adcome+i-1,addep1) = dsde(adcome+i-1,addep1) + dsidp1(i)
50          continue
        endif
    endif
! =====================================================================
30  continue
! =====================================================================
end subroutine
