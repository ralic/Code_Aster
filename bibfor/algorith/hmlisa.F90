subroutine hmlisa(perman, yachai, option, meca, thmc,&
                  ther, hydr, imate, ndim, dimdef,&
                  dimcon, nbvari, yamec, yate, addeme,&
                  adcome, advihy, advico, vihrho, vicphi,&
                  addep1, adcp11, addete, adcote, congem,&
                  congep, vintm, vintp, dsde, epsv,&
                  depsv, p1, dp1, t, dt,&
                  phi, rho11, phi0, sat, retcom,&
                  tbiot, rinstp, angmas, deps, aniso,&
                  phenom)
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
! aslint: disable=W1504
    implicit none
! aslint: disable=W1306
#include "asterfort/appmas.h"
#include "asterfort/calor.h"
#include "asterfort/capaca.h"
#include "asterfort/dhdt.h"
#include "asterfort/dhwdp1.h"
#include "asterfort/dilata.h"
#include "asterfort/dileau.h"
#include "asterfort/dmdepv.h"
#include "asterfort/dmwdp1.h"
#include "asterfort/dmwdt.h"
#include "asterfort/dqdeps.h"
#include "asterfort/dqdp.h"
#include "asterfort/dqdt.h"
#include "asterfort/dspdp1.h"
#include "asterfort/enteau.h"
#include "asterfort/inithm.h"
#include "asterfort/netbis.h"
#include "asterfort/sigmap.h"
#include "asterfort/thmrcp.h"
#include "asterfort/u2mess.h"
#include "asterfort/unsmfi.h"
#include "asterfort/viemma.h"
#include "asterfort/viporo.h"
#include "asterfort/virhol.h"
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yate
    integer :: adcome, adcp11, adcote, vihrho, vicphi
    integer :: addeme, addep1, addete, advihy, advico, retcom
    real(kind=8) :: congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nbvari), vintp(nbvari)
    real(kind=8) :: dsde(dimcon, dimdef), epsv, depsv, p1, dp1, t, dt
    real(kind=8) :: phi, rho11, phi0, rac2
    real(kind=8) :: rinstp, angmas(3)
    character(len=16) :: option, meca, ther, thmc, hydr, phenom
    logical :: perman, yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, aniso
    real(kind=8) :: epsvm, phim, rho11m, rho110, rho0, csigm, alp11
    real(kind=8) :: tbiot(6), cs, alpliq, cliq, cp11, sat
    real(kind=8) :: em, alp12, dpad, alpha0
    real(kind=8) :: rho12, rho21, rho22, cp12, cp21, cp22, coeps, dsatp1
    real(kind=8) :: m11m, satm, mdal(6), dalal, alphfi, cbiot, unsks
    real(kind=8) :: eps, deps(6)
    parameter  ( eps = 1.d-21 )
    logical :: emmag
! ======================================================================
! --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
! ======================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid8, rbid10, rbid11, rbid12, rbid13, rbid14(3)
    real(kind=8) :: rbid15(ndim, ndim), rbid16, rbid17, rbid18, rbid19
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid30, rbid31, rbid38
    real(kind=8) :: rbid33(ndim, ndim), rbid34, rbid35, rbid36, rbid37
    real(kind=8) :: rbid39, rbid40, rbid45, rbid46, rbid47, rbid48, rbid49
    real(kind=8) :: rbid50(ndim, ndim), rbid51, rbid20, rbid32(ndim, ndim)
    real(kind=8) :: dp2, signe
    real(kind=8) :: dmdeps(6), dsdp1(6), sigmp(6)
    real(kind=8) :: dqeps(6)
!
    logical :: net, bishop
!
    rac2 = sqrt(2.d0)
!
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! =====================================================================
    call netbis(meca, net, bishop)
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, t, p1, rbid40, rbid6,&
                rbid7, rbid8, rbid10, rbid11, rho0,&
                csigm, tbiot, rbid12, sat, rbid13,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rho110, cliq, alpliq,&
                cp11, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid50, rbid51, rinstp, retcom,&
                angmas, aniso, ndim)
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
                cs, tbiot, t, epsv, depsv,&
                epsvm, angmas, aniso, mdal, dalal,&
                alphfi, cbiot, unsks, alpha0, ndim,&
                phenom)
!
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
                        phi0, deps, depsv, alphfi, dt,&
                        dp1, dp2, signe, sat, cs,&
                        tbiot, phi, phim, retcom, cbiot,&
                        unsks, alpha0, aniso, phenom)
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
! =====================================================================
! --- ACTUALISATION DE CS ET ALPHFI -----------------------------------
! =====================================================================
    if (yamec .eq. 1) then
        call dilata(imate, phi, alphfi, t, aniso,&
                    angmas, tbiot, phenom)
        call unsmfi(imate, phi, cs, t, tbiot,&
                    aniso, ndim, phenom)
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
        alp11 = dileau(sat,phi,alphfi,alpliq)
! ======================================================================
! --- CALCUL DE LA CAPACITE CALORIFIQUE SELON FORMULE DOCR -------------
! --- RHO12, RHO21, RHO22 SONT NULLES ----------------------------------
! --- CP12, CP21, CP22 SONT NULLES -------------------------------------
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
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
! --- DP2 ET ALP12 SONT  NULLES ----------------------------------------
! ======================================================================
            congep(adcote) = congep(adcote) + calor(mdal,t,dt,deps, dp1,dp2,signe,alp11,alp12,coe&
                             &ps,ndim)
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
            call sigmap(net, bishop, sat, signe, tbiot,&
                        dp2, dp1, sigmp)
            do 10 i = 1, 3
                congep(adcome+6+i-1)=congep(adcome+6+i-1)+sigmp(i)
10          continue
            do 14 i = 4, 6
                congep(adcome+6+i-1)=congep(adcome+6+i-1)+sigmp(i)*&
                rac2
14          continue
        endif
! ======================================================================
! --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
! ======================================================================
        if (.not.perman) then
            congep(adcp11) = appmas(m11m,phi,phim,sat,satm,rho11, rho11m,epsv,epsvm)
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
            call dspdp1(net, bishop, signe, tbiot, sat,&
                        dsdp1)
            do 11 i = 1, 3
                dsde(adcome+6+i-1,addep1)=dsde(adcome+6+i-1,addep1)&
                + dsdp1(i)
11          continue
!
            do 88 i = 4, 6
                dsde(adcome+6+i-1,addep1)=dsde(adcome+6+i-1,addep1)&
                + dsdp1(i)*rac2
88          continue
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            if (.not.perman) then
                call dmdepv(rho11, sat, tbiot, dmdeps)
                do 12 i = 1, 6
                    dsde(adcp11,addeme+ndim-1+i) = dsde(adcp11,addeme+ ndim-1+i) + dmdeps(i)
12              continue
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
                call dqdeps(mdal, t, dqeps)
                do 20 i = 1, 6
                    dsde(adcote,addeme+ndim-1+i) = dsde(adcote,addeme+ ndim-1+i) + dqeps(i)

20              continue
            endif

        endif
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- POUR LES AUTRES CAS ----------------------------------------------
! ======================================================================
        if (.not.perman) then
            dsde(adcp11,addep1) = dsde(adcp11,addep1) + dmwdp1(rho11, signe,sat,dsatp1,phi,cs,cli&
                                  &q,1.0d0, emmag,em)
        endif
    endif
! ======================================================================
30  continue
! =====================================================================
end subroutine
