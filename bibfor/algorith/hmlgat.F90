subroutine hmlgat(yachai, option, meca, ther, hydr,&
                  imate, ndim, dimdef, dimcon, nbvari,&
                  yamec, yate, addeme, adcome, advihy,&
                  advico, vihrho, vicphi, vicsat, addep1,&
                  adcp11, addete, adcote, congem, congep,&
                  vintm, vintp, dsde, epsv, depsv,&
                  p1, dp1, t, dt, phi,&
                  rho11, phi0, sat, retcom, thmc,&
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
! ROUTINE HMLGAT : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEE
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   DANS LE CAS OU THMC = 'LIQU_GAZ_ATM'
! ======================================================================
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! COMMENTAIRE DE NMCONV :
!                       = 0 OK
!                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! ======================================================================
    implicit none
! aslint: disable=W1306
#include "asterf_types.h"
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
#include "asterfort/unsmfi.h"
#include "asterfort/viemma.h"
#include "asterfort/viporo.h"
#include "asterfort/virhol.h"
#include "asterfort/visatu.h"
    integer :: ndim, dimdef, dimcon, nbvari, imate, retcom, yamec
    integer :: yate, adcome, adcp11, adcote, addeme, addep1, addete
    integer :: advihy, advico, vihrho, vicphi, vicsat
    real(kind=8) :: congem(dimcon), congep(dimcon), vintm(nbvari)
    real(kind=8) :: vintp(nbvari), dsde(dimcon, dimdef), epsv, depsv
    real(kind=8) :: p1, dp1, t, dt, phi, rho11, phi0, rinstp
    real(kind=8) :: angmas(3)
    character(len=16) :: option, meca, ther, hydr, thmc, phenom
    aster_logical :: yachai
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, aniso
    real(kind=8) :: satm, epsvm, phim, rho11m, rho110, tbiot(6), cs
    real(kind=8) :: alpliq, cliq, cp11, sat, dsatp1, rho0, csigm
    real(kind=8) :: alp11, rho12, rho21
    real(kind=8) :: em, mdal(6), dalal, alphfi, cbiot, unsks, alpha0
    real(kind=8) :: eps, deps(6)
    parameter  ( eps = 1.d-21 )
    aster_logical :: emmag
! ======================================================================
! --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
! ======================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid8, rbid10, rbid11, rbid14(3)
    real(kind=8) :: rbid15(ndim, ndim), rbid16, rbid17, rbid18, rbid19
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid30, rbid31
    real(kind=8) :: rbid33(ndim, ndim), rbid34, rbid35, rbid36, rbid37
    real(kind=8) :: rbid39, rbid45, rbid46, rbid47, rbid48, rbid49, rbid38
    real(kind=8) :: rbid50(ndim, ndim), rbid20, rbid32(ndim, ndim)
    real(kind=8) :: dp2, signe, dpad, coeps, cp21, m11m, rho22, alp12, cp12
    real(kind=8) :: dqeps(6)
    real(kind=8) :: dsdp1(6), sigmp(6)
    real(kind=8) :: dmdeps(6), cp22, rac2
!
    aster_logical :: net, bishop
!
    rac2 = sqrt(2.d0)
!
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! =====================================================================
    call netbis(meca, net, bishop)
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                ther, rbid1, rbid2, rbid3, rbid4,&
                rbid5, t, p1, p1-dp1, rbid6,&
                rbid7, rbid8, rbid10, rbid11, rho0,&
                csigm, tbiot, satm, sat, dsatp1,&
                rbid14, rbid15, rbid16, rbid17, rbid18,&
                rbid19, rbid20, rbid21, rbid22, rbid23,&
                rbid24, rbid25, rho110, cliq, alpliq,&
                cp11, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                em, rbid50, rinstp, retcom,&
                angmas, aniso, ndim)
!
! ======================================================================
! --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
! --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
! --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
! ======================================================================
    emmag = .false.
    cp12 = 0.0d0
    cp21 = 0.0d0
    cp22 = 0.0d0
    alp11 = 0.0d0
    alp12 = 0.0d0
    rho12 = 0.0d0
    rho21 = 0.0d0
    rho22 = 0.0d0
    signe = 1.0d0
    dp2 = 0.0d0
    dpad = 0.0d0
    retcom = 0
    m11m = congem(adcp11)
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
    if ((option.eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA') .or.&
        (option.eq.'FORC_NODA')) then
! =====================================================================
! --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
! =====================================================================
        if (yamec .eq. 1) then
            call viporo(nbvari, vintm, vintp, advico, vicphi,&
                        phi0, deps, depsv, alphfi, dt,&
                        dp1, dp2, signe, sat, cs,&
                        tbiot, phi, phim, retcom, cbiot,&
                        unsks, alpha0, aniso, phenom)
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
! =====================================================================
! --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
! =====================================================================
    if (yate .eq. 1) then
! =====================================================================
! --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
! =====================================================================
        alp11 = dileau(sat,phi,alphfi,alpliq)
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
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
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
 10         continue
            do 14 i = 4, 6
                congep(adcome+6+i-1)=congep(adcome+6+i-1)+sigmp(i)*&
                rac2
 14         continue
        endif
! ======================================================================
! --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
! ======================================================================
        congep(adcp11) = appmas(m11m,phi,phim,sat,satm,rho11, rho11m, epsv,epsvm)
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
            do 111 i = 1, 3
                dsde(adcome+6+i-1,addep1)=dsde(adcome+6+i-1,addep1)&
                + dsdp1(i)
111         continue
            do 112 i = 4, 6
                dsde(adcome+6+i-1,addep1)=dsde(adcome+6+i-1,addep1)&
                + dsdp1(i)*rac2
112         continue
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
! ======================================================================
            call dmdepv(rho11, sat, tbiot, dmdeps)
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
            dsde(adcp11+ndim+1,addep1)=dsde(adcp11+ndim+1,addep1)&
            + dhwdp1(signe,alpliq,t,rho11)
            dsde(adcp11+ndim+1,addete)=dsde(adcp11+ndim+1,addete)&
            + dhdt(cp11)
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
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
 20             continue
            endif
        endif
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
! --- POUR LES AUTRES CAS ----------------------------------------------
! ======================================================================
        dsde(adcp11,addep1) = dsde(adcp11,addep1) + dmwdp1(rho11, signe,sat,dsatp1,phi,cs,cliq,1.&
                              &0d0, emmag,em)
    endif
! =====================================================================
 30 continue
! =====================================================================
!
end subroutine
