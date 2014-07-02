subroutine hmgazp(yachai, option, meca, thmc, ther,&
                  hydr, imate, ndim, dimdef, dimcon,&
                  nbvari, yamec, yate, addeme, adcome,&
                  advico, vicphi, addep1, adcp11, addete,&
                  adcote, congem, congep, vintm, vintp,&
                  dsde, epsv, depsv, p1, dp1,&
                  t, dt, phi, rho11, phi0,&
                  sat, retcom, tbiot, rinstp, angmas,&
                  deps, aniso, phenom)
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
! ======================================================================
! ROUTINE HMGAZP : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   DANS LE CAS OU THMC = 'GAZ'
! ======================================================================
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! COMMENTAIRE DE NMCONV :
!                       = 0 OK
!                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! ======================================================================
! aslint: disable=W1504
    implicit none
! aslint: disable=W1306
#include "asterf_types.h"
#include "asterfort/appmas.h"
#include "asterfort/calor.h"
#include "asterfort/capaca.h"
#include "asterfort/dhdt.h"
#include "asterfort/dilata.h"
#include "asterfort/dilgaz.h"
#include "asterfort/dmasp2.h"
#include "asterfort/dmdepv.h"
#include "asterfort/dmwdt.h"
#include "asterfort/dqdeps.h"
#include "asterfort/dqdp.h"
#include "asterfort/dqdt.h"
#include "asterfort/dspdp2.h"
#include "asterfort/entgaz.h"
#include "asterfort/inithm.h"
#include "asterfort/masvol.h"
#include "asterfort/netbis.h"
#include "asterfort/sigmap.h"
#include "asterfort/thmrcp.h"
#include "asterfort/unsmfi.h"
#include "asterfort/viporo.h"
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yate
    integer :: adcome, adcp11, adcote
    integer :: addeme, addep1, addete, advico, vicphi, retcom
    real(kind=8) :: congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nbvari), vintp(nbvari)
    real(kind=8) :: dsde(dimcon, dimdef), epsv, depsv, p1, dp1, t, dt
    real(kind=8) :: phi, rho11, phi0, rac2
    real(kind=8) :: angmas(3)
    character(len=16) :: option, meca, ther, hydr, thmc, phenom
    aster_logical :: yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, aniso
    real(kind=8) :: epsvm, phim
    real(kind=8) :: tbiot(6), cs, cp12, sat, mamolg
    real(kind=8) :: mdal(6), dalal, alphfi, cbiot, unsks, alpha0
    real(kind=8) :: r, rho0, csigm, alp11, em
    real(kind=8) :: eps, rinstp, deps(6)
    parameter  ( eps = 1.d-21 )
    aster_logical :: emmag
! ======================================================================
! --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
! ======================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid8, rbid10, rbid12, rbid13, rbid14(3)
    real(kind=8) :: rbid15(ndim, ndim), rbid16, rbid17, rbid18, rbid19
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid32(ndim, ndim)
    real(kind=8) :: rbid33(ndim, ndim), rbid34, rbid35, rbid36, rbid37
    real(kind=8) :: rbid39, rbid40, rbid41, rbid42, rbid43, rbid44
    real(kind=8) :: rbid45, rbid46, rbid47, rbid48, rbid49, rbid50(ndim, ndim)
    real(kind=8) :: rbid51, rbid20, rbid38, dsdp2(6)
    real(kind=8) :: signe, dp2, cliq, coeps, rho12, alp21, rho21, rho21m
    real(kind=8) :: cp21, p2, satm, rho22, cp11, cp22, m11m, dmdeps(6)
    real(kind=8) :: dqeps(6)
    real(kind=8) :: sigmp(6)
!
    aster_logical :: net, bishop
!
    rac2 = sqrt(2.d0)
!
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! --- DANS LE CALCUL ON LAISSE LES INDICES 11 POUR LE STOCKAGE DES ----
! --- VARIABLES. EN REVANCHE POUR UNE MEILLEURE COMPREHENSION PAR -----
! --- RAPPORT A LA DOC R7.01.11 ON NOTE LES INDICES 21 POUR LES -------
! --- VARIABLES DE CALCUL ---------------------------------------------
! =====================================================================
    emmag = .false.
    call netbis(meca, net, bishop)
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, t, p1, rbid6, rbid44,&
                rbid7, rbid8, rbid10, r, rho0,&
                csigm, tbiot, rbid12, sat, rbid13,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rbid43, rbid40, rbid41,&
                rbid42, rbid26, rbid27, rbid28, rbid29,&
                mamolg, cp21, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid50, rbid51, rinstp, retcom,&
                angmas, aniso, ndim)
! ======================================================================
! --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
! --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
! --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
! =====================================================================
! --- ON INVERSE POSE DP2 = DP1 ET DP1 = 0 POUR CONFOMITE A LA FORMULE-
! =====================================================================
    retcom = 0
    signe = 1.0d0
    p2 = p1
    dp2 = dp1
    dp1 = 0.0d0
    sat = 0.0d0
    satm = 0.0d0
    alp11 = 0.0d0
    rho11 = 0.0d0
    rho21 = 0.0d0
    rho22 = 0.0d0
    cp11 = 0.0d0
    cp12 = 0.0d0
    cp22 = 0.0d0
    cliq = 0.0d0
    m11m = congem(adcp11)
    phi = vintm(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
    if ((em.gt.eps) .and. (yamec.eq.0)) then
        emmag = .true.
    endif
!
    call inithm(imate, yachai, yamec, phi0, em,&
                cs, tbiot, t, epsv, depsv,&
                epsvm, angmas, aniso, mdal, dalal,&
                alphfi, cbiot, unsks, alpha0, ndim,&
                phenom)
!
! *********************************************************************
! *** LES VARIABLES INTERNES ******************************************
! *********************************************************************
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
! =====================================================================
        if ((yamec.eq.1) .or. emmag) then
            call viporo(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, deps, depsv, alphfi, dt,&
                        dp1, dp2, signe, sat, cs,&
                        tbiot, phi, phim, retcom, cbiot,&
                        unsks, alpha0, aniso, phenom)
        else if (yamec .eq. 2) then
            phi = vintp(advico+vicphi)
        endif
! =====================================================================
! --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
! =====================================================================
        if (retcom .ne. 0) then
            goto 30
        endif
    endif
! =====================================================================
! --- ACTUALISATION DE CS ET ALPHFI -----------------------------------
! =====================================================================
    if (yamec .eq. 1) then
        call dilata(imate, phi, alphfi, t, aniso,&
                    angmas, tbiot, phenom)
        call unsmfi(imate, phi, cs, t, tbiot,&
                    aniso, ndim, phenom)
    endif
! =====================================================================
! --- CALCUL DE LA MASSE VOLUMIQUE DU GAZ AUX INSTANT PLUS ET MOINS ---
! =====================================================================
    rho21 = masvol(mamolg,p2 ,r,t )
    rho21m = masvol(mamolg,p2-dp2,r,t-dt)
!
! =====================================================================
! --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
! =====================================================================
    if (yate .eq. 1) then
! =====================================================================
! --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
! =====================================================================
        alp21 = dilgaz(sat,phi,alphfi,t)
! ======================================================================
! --- CALCUL DE LA CAPACITE CALORIFIQUE SELON FORMULE DOCR -------------
! ======================================================================
        call capaca(rho0, rho11, rho12, rho21, rho22,&
                    sat, phi, csigm, cp11, cp12,&
                    cp21, cp22, dalal, t, coeps,&
                    retcom)
! =====================================================================
! --- PROBLEME LORS DU CALCUL DE COEPS --------------------------------
! =====================================================================
        if (retcom .ne. 0) then
            goto 30
        endif
! ======================================================================
! --- CALCUL DES ENTHALPIES SELON FORMULE DOCR -------------------------
! ======================================================================
        if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            congep(adcp11+ndim+1)=congep(adcp11+ndim+1)+entgaz(dt,&
            cp21)
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
! ======================================================================
            congep(adcote) = congep(adcote) + calor(mdal,t,dt,deps, dp1,dp2,signe,alp11,alp21, co&
                             &eps,ndim)
        endif
    endif
! ======================================================================
! --- CALCUL SI PAS RIGI_MECA_TANG -------------------------------------
! ======================================================================
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
! ======================================================================
! --- CALCUL DES CONTRAINTES DE PRESSIONS ------------------------------
! ======================================================================
        if (yamec .eq. 1) then
            call sigmap(net, bishop, sat, signe, tbiot,&
                        dp2, dp1, sigmp)
            do 10 i = 1, 3
                congep(adcome+6+i-1)=congep(adcome+6+i-1)+sigmp(i)
 10         continue
            do 14 i = 4, 6
                congep(adcome+6+i-1)=congep(adcome+6+i-1)+sigmp(i)*&
                rac2
 14         continue
        endif
! ======================================================================
! --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
! ======================================================================
        congep(adcp11) = appmas(m11m,phi,phim,1.0d0-sat, 1.0d0-satm, rho21,rho21m,epsv,epsvm)
    endif
!
! **********************************************************************
! *** CALCUL DES DERIVEES **********************************************
! **********************************************************************
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (yamec .eq. 1) then
! ======================================================================
! --- CALCUL UNIQUEMENT EN PRESENCE DE MECANIQUE -----------------------
! ======================================================================
! --- CALCUL DES DERIVEES DE SIGMAP ------------------------------------
! ======================================================================
            call dspdp2(net, bishop, tbiot, dsdp2)
            do 11 i = 1, 3
                dsde(adcome+6+i-1,addep1)=dsde(adcome+6+i-1,addep1)+&
                dsdp2(i)
 11         continue
            do 22 i = 4, 6
                dsde(adcome+6+i-1,addep1)=dsde(adcome+6+i-1,addep1)+&
                dsdp2(i)*rac2
 22         continue
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            call dmdepv(rho21, 1.0d0-sat, tbiot, dmdeps)
            do 12 i = 1, 6
                dsde(adcp11,addeme+ndim-1+i) = dsde(adcp11,addeme+ ndim-1+i) + dmdeps(i)
 12         continue
        endif
        if (yate .eq. 1) then
! ======================================================================
! --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
! ======================================================================
! --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
! ======================================================================
            dsde(adcp11+ndim+1,addete)=dsde(adcp11+ndim+1,addete)&
            + dhdt(cp21)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
! ======================================================================
            dsde(adcp11,addete) = dsde(adcp11,addete) + dmwdt(rho21, phi,sat,cliq,0.0d0,alp21)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! ======================================================================
            dsde(adcote,addete)=dsde(adcote,addete)+dqdt(coeps)
            dsde(adcote,addep1)=dsde(adcote,addep1)-dqdp(signe,alp21,&
            t)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            if (yamec .eq. 1) then
                call dqdeps(mdal, t, dqeps)
                do 20 i = 1, 6
                    dsde(adcote,addeme+ndim-1+i) = dsde(adcote,addeme+ ndim-1+i) + dqeps(i)
 20             continue
            endif
        endif
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- POUR LES AUTRES CAS ----------------------------------------------
! ======================================================================
        dsde(adcp11,addep1) = dsde(adcp11,addep1) + dmasp2(1.0d0,0.0d0,rho21,sat,phi,cs,p2,emmag &
                              &,em)
    endif
! =====================================================================
! --- MISE A JOUR DES VARIABLES P1 ET DP1 POUR CONFOMITE AUX FORMULES -
! =====================================================================
    p1 = p2
    dp1 = dp2
    rho11 = rho21
! =====================================================================
 30 continue
! =====================================================================
end subroutine
