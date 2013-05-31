subroutine hmlvag(yachai, option, meca, ther, hydr,&
                  imate, ndim, dimdef, dimcon, nbvari,&
                  yamec, yate, addeme, adcome, advihy,&
                  advico, vihrho, vicphi, vicpvp, vicsat,&
                  addep1, adcp11, adcp12, addep2, adcp21,&
                  addete, adcote, congem, congep, vintm,&
                  vintp, dsde, deps, epsv, depsv,&
                  p1, p2, dp1, dp2, t,&
                  dt, phi, pvp, h11, h12,&
                  rho11, phi0, pvp0, sat, retcom,&
                  thmc, crit, biot, rinstp)
! ======================================================================
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
! person_in_charge: sylvie.granet at edf.fr
! ======================================================================
! ROUTINE HMLVAG : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISE
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   DANS LE CAS OU THMC = 'LIQU_VAPE_GAZ'
! ======================================================================
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! COMMENTAIRE DE NMCONV :
!                       = 0 OK
!                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! ======================================================================
! aslint: disable=W1504
    implicit none
    include 'asterfort/appmas.h'
    include 'asterfort/calor.h'
    include 'asterfort/capaca.h'
    include 'asterfort/dhdt.h'
    include 'asterfort/dhwdp1.h'
    include 'asterfort/dhwdp2.h'
    include 'asterfort/dileau.h'
    include 'asterfort/dilgaz.h'
    include 'asterfort/dmasdt.h'
    include 'asterfort/dmasp1.h'
    include 'asterfort/dmasp2.h'
    include 'asterfort/dmdepv.h'
    include 'asterfort/dmvdp1.h'
    include 'asterfort/dmvdp2.h'
    include 'asterfort/dmvpdt.h'
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
    include 'asterfort/majpas.h'
    include 'asterfort/masvol.h'
    include 'asterfort/netbis.h'
    include 'asterfort/nmbarc.h'
    include 'asterfort/sigmap.h'
    include 'asterfort/thmrcp.h'
    include 'asterfort/viemma.h'
    include 'asterfort/viporo.h'
    include 'asterfort/vipvp1.h'
    include 'asterfort/virhol.h'
    include 'asterfort/visatu.h'
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec
    integer :: yate, retcom, adcome, adcp11, adcp12, advihy, advico
    integer :: vihrho, vicphi, vicpvp, vicsat
    integer :: adcp21, adcote, addeme, addep1, addep2, addete
    real(kind=8) :: congem(dimcon), congep(dimcon), vintm(nbvari), pvp0
    real(kind=8) :: vintp(nbvari), dsde(dimcon, dimdef), epsv, depsv
    real(kind=8) :: p1, dp1, p2, dp2, t, dt, phi, pvp, h11, h12, rho11, phi0
    real(kind=8) :: rinstp
    character(len=16) :: option, meca, ther, hydr, thmc
    logical :: yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i
    real(kind=8) :: satm, epsvm, phim, rho11m, rho12m, rho21m, pvpm
    real(kind=8) :: rho110, biot, k0, cs, alpha0, alpliq, cliq, rho12
    real(kind=8) :: rho21, cp11, cp12, cp21, sat, dsatp1, mamolv, mamolg
    real(kind=8) :: r, rho0, csigm, alp11, alp12, alp21, em, eps
    parameter  ( eps = 1.d-21 )
    logical :: emmag
! ======================================================================
! --- VARIABLES LOCALES POUR BARCELONE----------------------------------
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
    real(kind=8) :: rbid33, rbid34, rbid35, rbid38
    real(kind=8) :: rbid39, rbid45, rbid46, rbid47, rbid48, rbid49
    real(kind=8) :: rbid50, rbid51
    real(kind=8) :: r3bid(6)
    real(kind=8) :: signe, dpad, coeps, cp22, pas, rho22, m11m, m12m, m21m
!
    logical :: net, bishop
!
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
                rbid35, mamolv, cp12, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid50, r3bid, rbid51, rinstp,&
                retcom)
! ======================================================================
! --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
! --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
! --- ELLES NE SONT THEORIQUEMENT PAS UTILISEES ------------------------
! ======================================================================
    emmag = .false.
    retcom = 0
    dpad = 0.0d0
    signe = 1.0d0
    rho22 = 0.0d0
    cp22 = 0.0d0
    m11m = congem(adcp11)
    m12m = congem(adcp12)
    m21m = congem(adcp21)
    pvp = vintm(advico+vicpvp) + pvp0
    pvpm = vintm(advico+vicpvp) + pvp0
    phi = vintm(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
    rho11 = vintm(advihy+vihrho) + rho110
    rho11m = vintm(advihy+vihrho) + rho110
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
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE MASSE VOLUMIQUE DU FLUIDE ------
! --- SELON FORMULE DOCR ----------------------------------------------
! =====================================================================
        call virhol(nbvari, vintm, vintp, advihy, vihrho,&
                    rho110, dp1, dp2, dpad, cliq,&
                    dt, alpliq, signe, rho11, rho11m,&
                    retcom)
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE PRESSION DE VAPEUR -------------
! --- SELON FORMULE DOCR ----------------------------------------------
! =====================================================================
        call vipvp1(nbvari, vintm, vintp, advico, vicpvp,&
                    dimcon, p2, congem, adcp11, adcp12,&
                    ndim, pvp0, dp1, dp2, t,&
                    dt, mamolv, r, rho11, signe,&
                    cp11, cp12, yate, pvp, pvpm,&
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
!
! **********************************************************************
! *** LES CONTRAINTES GENERALISEES *************************************
! **********************************************************************
    rho12 = masvol(mamolv,pvp ,r,t )
    rho12m = masvol(mamolv,pvpm ,r,t-dt)
    rho21 = masvol(mamolg,p2-pvp ,r,t )
    rho21m = masvol(mamolg,p2-dp2-pvpm,r,t-dt)
    pas = majpas(p2,pvp)
! =====================================================================
! --- CALCUL DES AUTRES COEFFICIENTS DEDUITS : DILATATIONS ALPHA ------
! ---  DANS LE CAS D'UN SEUL FLUIDE ---------------------------
! =====================================================================
    if (yate .eq. 1) then
        alp11 = dileau(sat,biot,phi,alpha0,alpliq)
        alp12 = dilgaz(sat,biot,phi,alpha0,t )
        alp21 = dilgaz(sat,biot,phi,alpha0,t )
        h11 = congem(adcp11+ndim+1)
        h12 = congem(adcp12+ndim+1)
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
            congep(adcp12+ndim+1) = congep(adcp12+ndim+1) + entgaz(dt, cp12)
            congep(adcp21+ndim+1) = congep(adcp21+ndim+1) + entgaz(dt, cp21)
            h11 = congep(adcp11+ndim+1)
            h12 = congep(adcp12+ndim+1)
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
        congep(adcp12) = appmas(&
                         m12m, phi, phim, 1.0d0-sat, 1.0d0-satm, rho12, rho12m, epsv, epsvm)
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
                dsde(adcp12,addeme+ndim-1+i) = dsde(adcp12,addeme+ ndim-1+i) + dmdepv(rho12,1.0d0&
                                               &-sat,biot)
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
            dsde(adcp12+ndim+1,addete)=dsde(adcp12+ndim+1,addete)&
            + dhdt(cp12)
            dsde(adcp21+ndim+1,addete)=dsde(adcp21+ndim+1,addete)&
            + dhdt(cp21)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
! ======================================================================
            dsde(adcp11,addete) = dsde(adcp11,addete) + dmwdt(rho11, phi,sat,cliq,0.0d0,alp11)
            dsde(adcp12,addete) = dsde(adcp12,addete) + dmvpdt(rho12, sat,phi,h11,h12,pvp,t,alp12&
                                  &)
            dsde(adcp21,addete) = dsde(adcp21,addete) + dmasdt(rho12, rho21,sat,phi,pas,h11,h12,t&
                                  &,alp21)
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
        dsde(adcp12,addep1) = dsde(adcp12,addep1) + dmvdp1(rho11, rho12,sat,dsatp1,biot,phi,cs,pv&
                              &p, emmag,em)
        dsde(adcp12,addep2) = dsde(adcp12,addep2) + dmvdp2(rho11, rho12,sat,biot,phi,cs,pvp, emma&
                              &g,em)
        dsde(adcp21,addep1) = dsde(adcp21,addep1) + dmasp1(rho11, rho12,rho21,sat,dsatp1,biot,phi&
                              &,cs,pas, emmag,em)
        dsde(adcp21,addep2) = dsde(adcp21,addep2) + dmasp2(rho11, rho12,rho21,sat,biot,phi,cs,pas&
                              &, emmag,em)
    endif
!
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
