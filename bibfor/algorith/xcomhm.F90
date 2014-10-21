subroutine xcomhm(option, imate, compor,instap,&
                  ndim, dimdef, dimcon,nbvari,&
                  yamec, yap1, yap2, yate,&
                  addeme, adcome, addep1, adcp11,&
                  addep2, addete, defgem,&
                  defgep, congem, congep, vintm,&
                  vintp, dsde, pesa, retcom, kpi,&
                  npg, p10, p20, yaenrm, dimenr,&
                  idecpg, angmas, yaenrh, adenhy)
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE AU POINT
! DE GAUSS SUIVANT LES OPTIONS DEFINIES
! ======================================================================
! IN OPTION : OPTION DE CALCUL
! IN COMPOR : COMPORTEMENT
! IN IMATE  : MATERIAU CODE
! IN NDIM   : DIMENSION DE L'ESPACE
! IN DIMDEF : DIMENSION DU TABLEAU DES DEFORMATIONS GENERALISEES
!             AU POINT DE GAUSS CONSIDERE
! IN DIMCON : DIMENSION DU TABLEAU DES CONTRAINTES GENERALISEES
!             AU POINT DE GAUSS CONSIDERE
! IN NBVARI : NOMBRE TOTAL DE VARIABLES INTERNES AU POINT DE GAUSS
! IN YAMEC  : =1 S'IL Y A UNE EQUATION DE DEFORMATION MECANIQUE
! IN YAP1   : =1 S'IL Y A UNE EQUATION DE PRESSION DE FLUIDE
! IN YAP2   : =1 S'IL Y A UNE DEUXIEME EQUATION DE PRESSION DE FLUIDE
! IN YATE   : =1 S'IL YA UNE EQUATION THERMIQUE
! IN ADDEME : ADRESSE DES DEFORMATIONS MECANIQUES
! IN ADDEP1 : ADRESSE DES DEFORMATIONS CORRESPONDANT A LA PRESSION 1
! IN ADDEP2 : ADRESSE DES DEFORMATIONS CORRESPONDANT A LA PRESSION 2
! IN ADDETE : ADRESSE DES DEFORMATIONS THERMIQUES
! IN ADCOME : ADRESSE DES CONTRAINTES MECANIQUES
! IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 1
! IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 2
! IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 1
! IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 2
! IN ADCOTE : ADRESSE DES CONTRAINTES THERMIQUES
! IN DEFGEM : DEFORMATIONS GENERALISEES A L'INSTANT MOINS
! IN DEFGEP : DEFORMATIONS GENERALISEES A L'INSTANT PLUS
! IN CONGEM : CONTRAINTES GENERALISEES A L'INSTANT MOINS
! IN VINTM  : VARIABLES INTERNES A L'INSTANT MOINS
! IN TYPMOD : MODELISATION (D_PLAN, AXI, 3D ?)
!
! OUT CONGEP : CONTRAINTES GENERALISEES A L'INSTANT PLUS
! OUT VINTP  : VARIABLES INTERNES A L'INSTANT PLUS
! OUT DSDE   : MATRICE TANGENTE CONTRAINTES DEFORMATIONS
!
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! ======================================================================
! VARIABLES IN / OUT
! ======================================================================
    implicit none
!
! aslint: disable=W1306
#include "asterf_types.h"
#include "asterfort/kitdec.h"
#include "asterfort/nvithm.h"
#include "asterfort/thmlec.h"
#include "asterfort/xcalfh.h"
#include "asterfort/xcalme.h"
#include "asterfort/xhmsat.h"
    aster_logical :: yachai
    integer :: retcom, kpi, npg, vicpr1, vicpr2
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yap1
    integer :: yap2, yate, addeme, addep1, addep2, addete
    integer :: adcome, adcp11
    real(kind=8) :: defgem(1:dimdef), defgep(1:dimdef), congep(1:dimcon)
    real(kind=8) :: congem(1:dimcon), vintm(1:nbvari), vintp(1:nbvari)
    real(kind=8) :: instap
    character(len=16) :: compor(*), option
!
! DECLARATION POUR XFEM
    integer :: yaenrm, dimenr, idecpg
    integer :: yaenrh, adenhy
    real(kind=8) :: dsde(1:dimcon, 1:dimenr)
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: nvim, advime, advith, advihy, advico, anisof, aniso
    integer :: vihrho, vicphi, vicpvp, vicsat, nvih, nvic, nvit
    real(kind=8) :: p1, dp1, grap1(3), p2, dp2, grap2(3), t, dt, grat(3)
    real(kind=8) :: phi, pvp, pad, rho11, epsv, deps(6), depsv
    real(kind=8) :: t0, p10, p20, phi0, pvp0, sat, mamovg
    real(kind=8) :: rgaz, tbiot(6), satur, dsatur, pesa(3)
    real(kind=8) :: tperm(ndim, ndim), permli, dperml, permgz, dperms, dpermp
    real(kind=8) :: dfickt, dfickg, lambp, dlambp, unsurk, fick
    real(kind=8) :: lambs, dlambs, viscl, dviscl
    real(kind=8) :: viscg, dviscg, mamolg
    real(kind=8) :: fickad, dfadt, alpha
    real(kind=8) :: tlambt(ndim, ndim), tlamct(ndim, ndim), tdlamt(ndim, ndim)
    real(kind=8) :: dficks
    real(kind=8) :: angmas(3)
    character(len=16) :: meca, thmc, ther, hydr, phenom
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    retcom = 0
! ======================================================================
! --- MISE AU POINT POUR LES VARIABLES INTERNES ------------------------
! --- DEFINITION DES POINTEURS POUR LES DIFFERENTES RELATIONS DE -------
! --- COMPORTEMENTS ET POUR LES DIFFERENTES COMPOSANTES ----------------
! ======================================================================
    call nvithm(compor, meca, thmc, ther, hydr,&
                nvim, nvit, nvih, nvic, advime,&
                advith, advihy, advico, vihrho, vicphi,&
                vicpvp, vicsat, vicpr1, vicpr2)
! ======================================================================
! --- RECUPERATION DES DONNEES INITIALES -------------------------------
! ======================================================================
    call kitdec(kpi, yachai, yamec, yate, yap1,&
                yap2, meca, thmc, ther, hydr,&
                imate, defgem, defgep, addeme, addep1,&
                addep2, addete, ndim, t0, p10,&
                p20, phi0, pvp0, depsv, epsv,&
                deps, t, p1, p2, dt,&
                dp1, dp2, grat, grap1, grap2,&
                retcom, instap)
    if (retcom .ne. 0) then
        goto 900
    endif
! ======================================================================
! --- CALCUL DES RESIDUS ET DES MATRICES TANGENTES ---------------------
! ======================================================================
    call xhmsat(yachai, option, meca, thmc, ther,&
                hydr, imate, ndim, yaenrm, dimenr,&
                dimcon, nbvari, yamec, addeme,&
                adcome, advihy, advico, vihrho, vicphi,&
                addep1, adcp11, congem, congep, vintm,&
                vintp, dsde, epsv, depsv, p1,&
                dp1, t, phi, rho11, phi0,&
                sat, retcom, tbiot, instap,&
                angmas, aniso, phenom, yaenrh, adenhy)
    if (retcom .ne. 0) then
        goto 900
    endif
! ======================================================================
! --- CALCUL DES GRANDEURS MECANIQUES PURES UNIQUEMENT SI YAMEC = 1 -
! ET SI ON EST SUR UN POINT DE GAUSS (POUR L'INTEGRATION REDUITE)
!  C'EST A DIRE SI KPI<NPG
! ======================================================================
    if (yamec .eq. 1 .and. kpi .le. npg) then
        call xcalme(option, meca, imate, ndim, dimenr,&
                    dimcon, addeme, adcome, congep,&
                    dsde, deps, t, idecpg,&
                    kpi, angmas, aniso, phenom)
        if (retcom .ne. 0) then
            goto 900
        endif
    endif
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU FINALES ------------------------
! ======================================================================
    call thmlec(imate, thmc, meca, hydr, ther,&
                t, p1, p2, phi, vintp(1),&
                pvp, pad, rgaz, tbiot, satur,&
                dsatur, pesa, tperm, permli, dperml,&
                permgz, dperms, dpermp, fick, dfickt,&
                dfickg, lambp, dlambp, unsurk, alpha,&
                lambs, dlambs, viscl, dviscl, mamolg,&
                tlambt, tdlamt, viscg, dviscg, mamovg,&
                fickad, dfadt, tlamct, dficks, instap,&
                angmas, anisof, ndim)
! ======================================================================
! --- CALCUL DES FLUX HYDRAULIQUES UNIQUEMENT SI YAP1 = 1 --------------
! ======================================================================
    if ((yap1.eq.1).and.(yaenrh.eq.1)) then
        call xcalfh(option, thmc, ndim, dimcon, yamec,&
                    addep1, adcp11, addeme, congep, dsde,&
                    grap1, rho11, pesa, tperm, unsurk,&
                    viscl, dviscl, dimenr,&
                    adenhy)
        if (retcom .ne. 0) then
            goto 900
        endif
    endif
! ======================================================================
900 continue
! ======================================================================
end subroutine
