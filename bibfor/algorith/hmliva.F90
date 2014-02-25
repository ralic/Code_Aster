subroutine hmliva(yachai, option, meca, ther, hydr,&
                  imate, ndim, dimdef, dimcon, nbvari,&
                  yamec, yate, addeme, adcome, advihy,&
                  advico, vihrho, vicphi, vicpvp, vicsat,&
                  addep1, adcp11, adcp12, addete, adcote,&
                  congem, congep, vintm, vintp, dsde,&
                  epsv, depsv, p1, dp1, t,&
                  dt, phi, pvp, h11, h12,&
                  rho11, phi0, pvp0, sat, retcom,&
                  thmc, tbiot, rinstp, angmas, deps,&
                  aniso, phenom)
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
! ROUTINE HMLIVA : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   DANS LE CAS OU THMC = 'LIQU_VAPE'
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
#include "asterc/r8maem.h"
#include "asterfort/appmas.h"
#include "asterfort/calor.h"
#include "asterfort/capaca.h"
#include "asterfort/dhdt.h"
#include "asterfort/dhwdp1.h"
#include "asterfort/dilata.h"
#include "asterfort/dileau.h"
#include "asterfort/dilgaz.h"
#include "asterfort/dmdepv.h"
#include "asterfort/dmvpd2.h"
#include "asterfort/dmvpp1.h"
#include "asterfort/dmwdt2.h"
#include "asterfort/dmwp1v.h"
#include "asterfort/dqvpdp.h"
#include "asterfort/dqvpdt.h"
#include "asterfort/enteau.h"
#include "asterfort/entgaz.h"
#include "asterfort/inithm.h"
#include "asterfort/masvol.h"
#include "asterfort/netbis.h"
#include "asterfort/sigmap.h"
#include "asterfort/thmrcp.h"
#include "asterfort/unsmfi.h"
#include "asterfort/viemma.h"
#include "asterfort/viporo.h"
#include "asterfort/vipvp1.h"
#include "asterfort/virhol.h"
#include "asterfort/visatu.h"
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yate, retcom
    integer :: adcome, adcp11, adcp12, adcote, addeme, addep1, addete
    integer :: advihy, advico, vihrho, vicphi, vicpvp, vicsat
    real(kind=8) :: congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nbvari), vintp(nbvari)
    real(kind=8) :: dsde(dimcon, dimdef)
    real(kind=8) :: epsv, depsv, p1, dp1, t, dt
    real(kind=8) :: phi, pvp, h11, h12, rho11
    real(kind=8) :: phi0, pvp0
    real(kind=8) :: ums, phids, rinstp, angmas(3)
    character(len=16) :: option, meca, ther, hydr, thmc, phenom
    logical :: yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, aniso
    real(kind=8) :: satm, epsvm, phim, rho11m, rho12m, pvpm, rho110, dpvp
    real(kind=8) :: dpvpt, dpvpl, tbiot(6), cs, alpliq, cliq
    real(kind=8) :: cp11, cp12, sat, dsatp1, mamolv, em
    real(kind=8) :: r, rho0, csigm, alp11, alp12, rho12, alpha0
    real(kind=8) :: eps, deps(6), mdal(6), dalal, alphfi, cbiot, unsks
    parameter  ( eps = 1.d-21 )
    logical :: emmag
! ======================================================================
! --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
! ======================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid8, rbid10, rbid14(3),rbid9
    real(kind=8) :: rbid15(ndim, ndim), rbid16, rbid17, rbid18, rbid19
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid30, rbid31, rbid32(ndim, ndim)
    real(kind=8) :: rbid33(ndim, ndim), rbid34, rbid35, rbid38, rbid20
    real(kind=8) :: rbid39, rbid40, rbid41, rbid42, rbid43,rbid37(6)
    real(kind=8) :: rbid51, rbid52, rbid53, rbid54
    real(kind=8) :: rbid45, rbid46, rbid47, rbid48, rbid49, rbid56
    real(kind=8) :: rbid57(ndim, ndim), rbid58, rbid55
    real(kind=8) :: m11m, m12m, coeps, pinf, dp2, cp21, cp22, rho21
    real(kind=8) :: rho22, dpad, signe, rac2
!
    logical :: net, bishop
!
    rac2 = sqrt(2.d0)
!
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! --- UN PREMIER APPEL A THMRCP POUR RECUPERE SATM --------------------
! =====================================================================
    call netbis(meca, net, bishop)
    pvp = vintm(advico+vicpvp) + pvp0
    pvpm = vintm(advico+vicpvp) + pvp0
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, t, rbid40, pvpm-p1+dp1, rbid6,&
                rbid7, rbid8, rbid10, r, rho0,&
                csigm, tbiot, satm, rbid42, rbid43,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rho110, cliq, alpliq,&
                cp11, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, mamolv, cp12, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid57, rbid58, rinstp, retcom,&
                angmas, aniso, ndim)
! ======================================================================
! --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
! --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
! --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
! ======================================================================
    emmag = .false.
    dpvp = 0.0d0
    dpvpl = 0.0d0
    dpvpt = 0.0d0
    signe = -1.0d0
    dp2 = 0.0d0
    dpad = 0.0d0
    rho21 = 0.0d0
    rho22 = 0.0d0
    cp21 = 0.0d0
    cp22 = 0.0d0
    retcom = 0
    rho11 = vintm(advihy+vihrho) + rho110
    rho11m = vintm(advihy+vihrho) + rho110
    phi = vintm(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
    h11 = congem(adcp11+ndim+1)
    h12 = congem(adcp12+ndim+1)
    m11m = congem(adcp11)
    m12m = congem(adcp12)
! =====================================================================
! --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
! =====================================================================
    if ((em.gt.eps) .and. (yamec.eq.0)) then
        emmag = .true.
    endif
    call inithm(imate, yachai, yamec, phi0, em,&
                cs, tbiot, t, epsv, depsv,&
                epsvm, angmas, aniso, mdal, dalal,&
                alphfi, cbiot, unsks, alpha0, ndim,&
                phenom)
! *********************************************************************
! *** LES VARIABLES INTERNES ******************************************
! *********************************************************************
    if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
! =====================================================================
! --- EN LIQU_VAPE CALCUL DE RHO11, DES ENTHALPIES DE PVP ET RHOVP ----
! =====================================================================
        call virhol(nbvari, vintm, vintp, advihy, vihrho,&
                    rho110, dp1, dp2, dpad, cliq,&
                    dt, alpliq, signe, rho11, rho11m,&
                    retcom)
! =====================================================================
! --- EN LIQU_VAPE CALCUL DE RHO11, DES ENTHALPIES DE PVP ET RHOVP ----
! =====================================================================
        pinf = r8maem()
        call vipvp1(nbvari, vintm, vintp, advico, vicpvp,&
                    dimcon, pinf, congem, adcp11, adcp12,&
                    ndim, pvp0, dp1, dp2, t,&
                    dt, mamolv, r, rho11, signe,&
                    cp11, cp12, yate, pvp, pvpm,&
                    retcom)
! =====================================================================
! --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
! =====================================================================
        if (retcom .ne. 0) then
            goto 30
        endif
    endif
    dpvp = pvp - pvpm
! =====================================================================
! --- ON PEUT MAINTENANT CALCULER SAT DANS LE CAS LIQU_VAPE -----------
! =====================================================================
    call thmrcp('SATURATI', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, rbid9, pvp-p1, rbid41, rbid6,&
                rbid7, rbid8, rbid10, rbid51, rbid52,&
                rbid53, rbid37, rbid41, sat, dsatp1,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rho110, rbid53, rbid52,&
                rbid51, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid54, rbid55, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                rbid56, rbid57, rbid58, rinstp, retcom,&
                angmas, aniso, ndim)
    if ((option.eq.'RAPH_MECA') .or. (option.eq.'FORC_NODA') .or.&
        (option(1:9).eq.'FULL_MECA')) then
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
! =====================================================================
!        if ((yamec.eq.1)) then
! =====================================================================
! --- ON POSE ICI P2 = PVP ET P1 = - (PVP - PW) (ON CHANGE LE SIGNE ---
! --- CAR ON MULTIPLIE DANS VIPORO PAR -1) ----------------------------
! =====================================================================
!        endif
        if (emmag) then
            call viemma(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, dp1-dpvp, dpvp, signe, sat,&
                        em, phi, phim, retcom)
        endif
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
! =====================================================================
! --- QUELQUES INITIALISATIONS ----------------------------------------
! =====================================================================
    ums = 1.d0 - sat
    phids = phi*dsatp1
! **********************************************************************
! *** LES CONTRAINTES GENERALISEES *************************************
! **********************************************************************
! ======================================================================
! --- CALCUL DES MASSES VOLUMIQUES DE PRESSION DE VAPEUR ---------------
! ----------------------------------- AIR SEC --------------------------
! ----------------------------------- AIR DISSOUS ----------------------
! ======================================================================
    rho12 = masvol(mamolv,pvp ,r,t )
    rho12m = masvol(mamolv,pvpm,r,t-dt)
! =====================================================================
! --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
! =====================================================================
    if (yate .eq. 1) then
! =====================================================================
! --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
! =====================================================================
        alp11 = dileau(sat,phi,alphfi,alpliq)
        alp12 = dilgaz(sat,phi,alphfi,t)
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
        if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
            congep(adcp11+ndim+1) = congep(adcp11+ndim+1) + enteau(dt, alpliq,t,rho11,dp2,dp1,dpa&
                                    &d,signe,cp11)
            congep(adcp12+ndim+1) = congep(adcp12+ndim+1) + entgaz(dt, cp12)
            h11 = congep(adcp11+ndim+1)
            h12 = congep(adcp12+ndim+1)
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
! =====================================================================
! --- ON POSE ICI P2 = PVP ET P1 = - (PVP - PW) (ON CHANGE LE SIGNE ---
! --- CAR ON MULTIPLIE DANS VIPORO PAR -1) ----------------------------
! ======================================================================
            congep(adcote) = congep(adcote) + calor(mdal,t,dt,deps, dp1-dpvp,dpvp,signe,alp11,alp&
                             &12,coeps, ndim)
        endif
    endif
! =====================================================================
! --- DPVPL DERIVEE PRESSION DE VAPEUR / PRESSION DE LIQUIDE ----------
! --- DPVPT DERIVEE PRESSION DE VAPEUR / TEMP -------------------------
! =====================================================================
    if (option(1:9) .eq. 'RIGI_MECA') then
        dpvpl = rho12m/rho11m
        if (yate .eq. 1) then
            dpvpt = rho12m * (congem(adcp12+ndim+1) - congem(adcp11+ ndim+1)) / t
        endif
    else
        dpvpl = rho12/rho11
        if (yate .eq. 1) then
            dpvpt = rho12 * (congep(adcp12+ndim+1) - congep(adcp11+ ndim+1)) / t
        endif
    endif
! ======================================================================
! --- CALCUL SI PAS RIGI_MECA_TANG -------------------------------------
! ======================================================================
    if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
! ======================================================================
! --- CALCUL DES CONTRAINTES DE PRESSIONS : PAS D ACTUALITE TANT QUE
!      PAS COUPLE
! ======================================================================
! ======================================================================
! --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
! ======================================================================
        congep(adcp11) = appmas(m11m,phi,phim,sat,satm,rho11, rho11m, epsv,epsvm)
        congep(adcp12) = appmas(m12m,phi,phim,1.0d0-sat, 1.0d0-satm, rho12,rho12m,epsv,epsvm)
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
!        if (yamec .eq. 1) then
!        endif
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
            dsde(adcp12+ndim+1,addete)=dsde(adcp12+ndim+1,addete)&
            + dhdt(cp12)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
! ======================================================================
            dsde(adcp11,addete) = dsde(adcp11,addete) + dmwdt2(rho11, alp11,phids,sat,cs,dpvpt)
            dsde(adcp12,addete) = dsde(adcp12,addete) + dmvpd2(rho12, alp12,dpvpt,phi,ums,pvp,phi&
                                  &ds,cs)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! ======================================================================
            dsde(adcote,addete)=dsde(adcote,addete) + dqvpdt(coeps,&
            alp12,t,dpvpt)
            dsde(adcote,addep1)=dsde(adcote,addep1) + dqvpdp(alp11,&
            alp12,t,dpvpl)
! ======================================================================
! --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE : AUJOURD'HUI NON PREVUE 
! ======================================================================
!            if (yamec .eq. 1) then
!    a completer le cas echeant
!            endif
        endif
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- POUR LES AUTRES CAS ----------------------------------------------
! ======================================================================
        dsde(adcp11,addep1) = dsde(adcp11,addep1) + dmwp1v(rho11, phids,sat,cs,dpvpl,phi,cliq)
        dsde(adcp12,addep1) = dsde(adcp12,addep1) + dmvpp1(rho11, rho12,phids,ums,cs,dpvpl,sat,ph&
                              &i,pvp)
    endif
! =====================================================================
30  continue
! =====================================================================
end subroutine
