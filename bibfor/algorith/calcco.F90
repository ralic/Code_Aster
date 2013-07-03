subroutine calcco(option, yachai, perman, meca, thmc,&
                  ther, hydr, imate, ndim, dimdef,&
                  dimcon, nbvari, yamec, yate, addeme,&
                  adcome, advihy, advico, addep1, adcp11,&
                  adcp12, addep2, adcp21, adcp22, addete,&
                  adcote, congem, congep, vintm, vintp,&
                  dsde, deps, epsv, depsv, p1,&
                  p2, dp1, dp2, t, dt,&
                  phi, pvp, pad, h11, h12,&
                  kh, rho11, phi0, pvp0, sat,&
                  retcom, crit, biot, vihrho, vicphi,&
                  vicpvp, vicsat, rinstp)
! ======================================================================
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! **********************************************************************
! ROUTINE CALCCO : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
!   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
!   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
!   ELLE RENVOIE POUR CELA A DIFFERENTES ROUTINES SUIVANT
!   LA VALEUR DE THMC
! **********************************************************************
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! COMMENTAIRE DE NMCONV :
!                       = 0 OK
!                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTAT
!                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! ======================================================================
! aslint: disable=W1504
    implicit none
#include "asterfort/hmgazp.h"
#include "asterfort/hmladg.h"
#include "asterfort/hmlgat.h"
#include "asterfort/hmliga.h"
#include "asterfort/hmlisa.h"
#include "asterfort/hmliva.h"
#include "asterfort/hmlvag.h"
#include "asterfort/hmlvga.h"
    integer :: ndim, dimdef, dimcon, nbvari, imate
    integer :: yamec, yate
    integer :: adcome, adcp11, bdcp11, adcp12, adcp21, adcp22, adcote
    integer :: addeme, addep1, addep2, addete, retcom
    integer :: advihy, advico, vihrho, vicphi, vicpvp, vicsat
    real(kind=8) :: congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nbvari), vintp(nbvari)
    real(kind=8) :: dsde(dimcon, dimdef), epsv, depsv, p1, dp1, p2, dp2, t, dt
    real(kind=8) :: phi, pvp, pad, h11, h12, kh, rho11, phi0
    real(kind=8) :: pvp0, sat, rinstp
    character(len=16) :: option, meca, thmc, ther, hydr
    logical :: perman, yachai
! ======================================================================
! --- VARIABLES LOCALES POUR BARCELONE-------------------------------
! ======================================================================
    real(kind=8) :: deps(6), biot, crit(*)
!
! INITIALISATION ADRESSE SELON QUE LA PARTIE THH EST TRANSITOIRE OU NON
    if (perman) then
        bdcp11 = adcp11 - 1
    else
        bdcp11 = adcp11
    endif
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_SATU ----------------------
! ======================================================================
    if (thmc .eq. 'LIQU_SATU') then
        call hmlisa(perman, yachai, option, meca, thmc,&
                    ther, hydr, imate, ndim, dimdef,&
                    dimcon, nbvari, yamec, yate, addeme,&
                    adcome, advihy, advico, vihrho, vicphi,&
                    addep1, bdcp11, addete, adcote, congem,&
                    congep, vintm, vintp, dsde, epsv,&
                    depsv, p1, dp1, t, dt,&
                    phi, rho11, phi0, sat, retcom,&
                    biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE GAZ ----------------------------
! ======================================================================
    else if (thmc.eq.'GAZ') then
        call hmgazp(yachai, option, meca, thmc, ther,&
                    hydr, imate, ndim, dimdef, dimcon,&
                    nbvari, yamec, yate, addeme, adcome,&
                    advico, vicphi, addep1, bdcp11, addete,&
                    adcote, congem, congep, vintm, vintp,&
                    dsde, epsv, depsv, p1, dp1,&
                    t, dt, phi, rho11, phi0,&
                    sat, retcom, biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE ----------------------
! ======================================================================
    else if (thmc.eq.'LIQU_VAPE') then
        call hmliva(yachai, option, meca, ther, hydr,&
                    imate, ndim, dimdef, dimcon, nbvari,&
                    yamec, yate, addeme, adcome, advihy,&
                    advico, vihrho, vicphi, vicpvp, vicsat,&
                    addep1, bdcp11, adcp12, addete, adcote,&
                    congem, congep, vintm, vintp, dsde,&
                    epsv, depsv, p1, dp1, t,&
                    dt, phi, pvp, h11, h12,&
                    rho11, phi0, pvp0, sat, retcom,&
                    thmc, biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ------------------
! ======================================================================
    else if (thmc.eq.'LIQU_VAPE_GAZ') then
        call hmlvag(yachai, option, meca, ther, hydr,&
                    imate, ndim, dimdef, dimcon, nbvari,&
                    yamec, yate, addeme, adcome, advihy,&
                    advico, vihrho, vicphi, vicpvp, vicsat,&
                    addep1, bdcp11, adcp12, addep2, adcp21,&
                    addete, adcote, congem, congep, vintm,&
                    vintp, dsde, deps, epsv, depsv,&
                    p1, p2, dp1, dp2, t,&
                    dt, phi, pvp, h11, h12,&
                    rho11, phi0, pvp0, sat, retcom,&
                    thmc, crit, biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ -----------------------
! ======================================================================
    else if (thmc.eq.'LIQU_GAZ') then
        call hmliga(yachai, option, meca, ther, hydr,&
                    imate, ndim, dimdef, dimcon, nbvari,&
                    yamec, yate, addeme, adcome, advihy,&
                    advico, vihrho, vicphi, vicsat, addep1,&
                    bdcp11, addep2, adcp21, addete, adcote,&
                    congem, congep, vintm, vintp, dsde,&
                    deps, epsv, depsv, p1, p2,&
                    dp1, dp2, t, dt, phi,&
                    rho11, phi0, sat, retcom, thmc,&
                    crit, biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM -------------------
! ======================================================================
    else if (thmc.eq.'LIQU_GAZ_ATM') then
        call hmlgat(yachai, option, meca, ther, hydr,&
                    imate, ndim, dimdef, dimcon, nbvari,&
                    yamec, yate, addeme, adcome, advihy,&
                    advico, vihrho, vicphi, vicsat, addep1,&
                    bdcp11, addete, adcote, congem, congep,&
                    vintm, vintp, dsde, epsv, depsv,&
                    p1, dp1, t, dt, phi,&
                    rho11, phi0, sat, retcom, thmc,&
                    biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE ---------------
! ======================================================================
    else if (thmc.eq.'LIQU_AD_GAZ_VAPE') then
        call hmlvga(yachai, option, meca, ther, hydr,&
                    imate, ndim, dimdef, dimcon, nbvari,&
                    yamec, yate, addeme, adcome, advihy,&
                    advico, vihrho, vicphi, vicpvp, vicsat,&
                    addep1, bdcp11, adcp12, addep2, adcp21,&
                    adcp22, addete, adcote, congem, congep,&
                    vintm, vintp, dsde, epsv, depsv,&
                    p1, p2, dp1, dp2, t,&
                    dt, phi, pad, pvp, h11,&
                    h12, kh, rho11, phi0, pvp0,&
                    sat, retcom, thmc, biot, rinstp)
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE ---------------
! ======================================================================
    else if (thmc.eq.'LIQU_AD_GAZ') then
        call hmladg(yachai, option, meca, ther, hydr,&
                    imate, ndim, dimdef, dimcon, nbvari,&
                    yamec, yate, addeme, adcome, advihy,&
                    advico, vihrho, vicphi, vicpvp, vicsat,&
                    addep1, bdcp11, adcp12, addep2, adcp21,&
                    adcp22, addete, adcote, congem, congep,&
                    vintm, vintp, dsde, epsv, depsv,&
                    p1, p2, dp1, dp2, t,&
                    dt, phi, pad, h11, h12,&
                    kh, rho11, phi0, sat, retcom,&
                    thmc, biot, rinstp)
! ======================================================================
    endif
! ======================================================================
end subroutine
