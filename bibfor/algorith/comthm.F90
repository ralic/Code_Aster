subroutine comthm(option, perman, vf, ifa, valfac,&
                  valcen, imate, typmod, compor, crit,&
                  instam, instap, ndim, dimdef, dimcon,&
                  nbvari, yamec, yap1, yap2, yate,&
                  addeme, adcome, addep1, adcp11, adcp12,&
                  addep2, adcp21, adcp22, addete, adcote,&
                  defgem, defgep, congem, congep, vintm,&
                  vintp, dsde, pesa, retcom, kpi,&
                  npg, p10, p20, angmas)
! ======================================================================
!
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
!
! VERSION DU 07/06/99  ECRITE PAR PASCAL CHARLES
! ROUTINE COMTHM
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE AU POINT
! DE GAUSS SUIVANT LES OPTIONS DEFINIES
!
! **********************************************************************
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (RESI_INTE_PAS == ITEDEC )
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
! ======================================================================
! IN OPTION : OPTION DE CALCUL
! IN PERMAN : TRUE SI PERMANENT
! IN VF : TRUE SI VOLUMES FINIS
! IN IFA : UTILISE EN VF ET POUR LES VALEURS AUX ARETES
!      -> NUMERO DE LA FACE. LES INFORMATIONS SONT STOCKES
!       DS VALFAC(1:6,1:4,1:NBFACE)
! VALFAC : SOCKAGE DES VALEURS CALSULEES AUX ARETES EN VF IFA!=0
! DES VALEURS AU CENTRE
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
! aslint: disable=W1504
    implicit none
!
#include "asterfort/calcco.h"
#include "asterfort/calcfh.h"
#include "asterfort/calcft.h"
#include "asterfort/calcme.h"
#include "asterfort/kitdec.h"
#include "asterfort/nvithm.h"
#include "asterfort/thmlec.h"
    logical :: yachai
    real(kind=8) :: valcen(14, 6)
    integer :: maxfa
    parameter     (maxfa=6)
    real(kind=8) :: valfac(maxfa, 14, 6)
    integer :: masse, dmasp1, dmasp2
    integer :: eau, air
    integer :: vkint, kxx, kyy, kzz, kxy, kyz, kzx
!      PARAMETER(CON=1,DCONP1=2,DCONP2=3,DIFFU=4,DDIFP1=5,DDIFP2=6)
!      PARAMETER(MOB=7,DMOBP1=8,DMOBP2=9,MASSE=10,DMASP1=11,DMASP2=12)
!      PARAMETER(RHOGA=1,RHOLQ=2,RHOGA1=3,RHOGA2=4,RHOLQ1=5,RHOLQ2=6)
    parameter     (masse=10,dmasp1=11,dmasp2=12)
    parameter     (vkint=13)
!      PARAMETER(DENSIT=14)
    parameter     (kxx=1,kyy=2,kzz=3,kxy=4,kyz=5,kzx=6)
    parameter     (eau=1,air=2)
    integer :: retcom, kpi, npg
    integer :: ndim, dimdef, dimcon, nbvari, imate, yamec, yap1
    integer :: yap2, yate, addeme, addep1, addep2, addete
    integer :: adcome, adcp11, adcp12, adcp21, adcp22, adcote
    real(kind=8) :: defgem(1:dimdef), defgep(1:dimdef), congep(1:dimcon)
    real(kind=8) :: congem(1:dimcon), vintm(1:nbvari), vintp(1:nbvari)
    real(kind=8) :: dsde(1:dimcon, 1:dimdef), crit(*), instam, instap
    character(len=8) :: typmod(2)
    character(len=16) :: compor(*), option
    logical :: perman, vf
    integer :: ifa
    integer :: vicpr1, vicpr2
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: nvim, advime, advith, advihy, advico, anisof, aniso
    integer :: vihrho, vicphi, vicpvp, vicsat, nvih, nvic, nvit
    real(kind=8) :: p1, dp1, grap1(3), p2, dp2, grap2(3), t, dt, grat(3)
    real(kind=8) :: phi, pvp, pad, h11, h12, rho11, epsv, deps(6), depsv
    real(kind=8) :: t0, p10, p20, phi0, pvp0, sat, mamovg
    real(kind=8) :: rgaz, tbiot(6), satur, dsatur, pesa(3)
    real(kind=8) :: tperm(ndim, ndim), permli, dperml, permgz, dperms, dpermp
    real(kind=8) :: dfickt, dfickg, lambp, dlambp, unsurk, fick
    real(kind=8) :: lambs, dlambs, viscl, dviscl
    real(kind=8) :: viscg, dviscg, mamolg
    real(kind=8) :: fickad, dfadt, kh, alpha
    real(kind=8) :: tlambt(ndim, ndim), tlamct(ndim, ndim),tdlamt(ndim, ndim)
    real(kind=8) :: dficks
    real(kind=8) :: deltat
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
        goto 9000
    endif
! ======================================================================
! --- CALCUL DES RESIDUS ET DES MATRICES TANGENTES ---------------------
! ======================================================================
!
    call calcco(option, yachai, perman, meca, thmc,&
                ther, hydr, imate, ndim, dimdef,&
                dimcon, nbvari, yamec, yate, addeme,&
                adcome, advihy, advico, addep1, adcp11,&
                adcp12, addep2, adcp21, adcp22, addete,&
                adcote, congem, congep, vintm, vintp,&
                dsde, deps, epsv, depsv, p1,&
                p2, dp1, dp2, t, dt,&
                phi, pvp, pad, h11, h12,&
                kh, rho11, phi0, pvp0, sat,&
                retcom, crit, tbiot, vihrho, vicphi,&
                vicpvp, vicsat, instap, angmas, aniso,&
                phenom)
!
    if (retcom .ne. 0) then
        goto 9000
    endif
!
! VOLUMES FINIS
!
    if (vf .and. (ifa.eq.0)) then
        deltat=instap-instam
        if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9) .eq.'RAPH_MECA')) then
!
            valcen(masse ,eau)=( congep(adcp11)+congep(adcp12)&
            -congem(adcp11)-congem(adcp12))/deltat
            valcen(masse ,air)=( congep(adcp21)+congep(adcp22)&
            -congem(adcp21)-congem(adcp22))/deltat
!
        endif
        if ((option(1:9) .eq. 'RIGI_MECA') .or. (option(1:9) .eq. 'FULL_MECA')) then
            valcen(dmasp1,eau)= (dsde(adcp11,addep1)+ dsde(adcp12,&
            addep1))/deltat
            valcen(dmasp2,eau)= (dsde(adcp11,addep2)+ dsde(adcp12,&
            addep2))/deltat
            valcen(dmasp1,air)= (dsde(adcp22,addep1)+ dsde(adcp21,&
            addep1))/deltat
            valcen(dmasp2,air)= (dsde(adcp22,addep2)+ dsde(adcp21,&
            addep2))/deltat
        endif
!
    endif
!
! ======================================================================
! --- CALCUL DES GRANDEURS MECANIQUES PURES UNIQUEMENT SI YAMEC = 1 -
! ET SI ON EST SUR UN POINT DE GAUSS (POUR L'INTEGRATION REDUITE)
!  C'EST A DIRE SI KPI<NPG
! ======================================================================
    if (yamec .eq. 1 .and. kpi .le. npg) then
        call calcme(option, compor, thmc, meca, imate,&
                    typmod, crit, instam, instap, t0,&
                    ndim, dimdef, dimcon, nvim, yate,&
                    addeme, adcome, addete, defgem, congem,&
                    congep, vintm, vintp, addep1, addep2,&
                    dsde, deps, p1, p2,&
                    t, dt, retcom, dp1, dp2,&
                    sat, tbiot, angmas, aniso, phenom)
        if (retcom .ne. 0) then
            goto 9000
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
!
! CONDUCTIVITES EN VF
!
    if (vf .and. (ifa.eq.0)) then
        if (ndim .eq. 3) then
            valcen(vkint ,kxx)=tperm(1,1)
            valcen(vkint ,kyy)=tperm(2,2)
            valcen(vkint ,kzz)=tperm(3,3)
            valcen(vkint ,kxy)=tperm(1,2)
            valcen(vkint ,kyz)=tperm(1,3)
            valcen(vkint ,kzx)=tperm(2,3)
        else
            valcen(vkint ,kxx)=tperm(1,1)
            valcen(vkint ,kyy)=tperm(1,1)
            valcen(vkint ,kzz)=tperm(2,2)
            valcen(vkint ,kxy)=tperm(1,2)
            valcen(vkint ,kyz)=0.d0
            valcen(vkint ,kzx)=0.d0
        endif
    endif
! ======================================================================
! --- CALCUL DES FLUX HYDRAULIQUES UNIQUEMENT SI YAP1 = 1 --------------
! ======================================================================
    if (yap1 .eq. 1) then
!
        call calcfh(option, perman, thmc, ndim, dimdef,&
                    dimcon, yamec, yate, addep1, addep2,&
                    adcp11, adcp12, adcp21, adcp22, addeme,&
                    addete, congep, dsde, p1, p2,&
                    grap1, grap2, t, grat, pvp,&
                    pad, rho11, h11, h12, rgaz,&
                    dsatur, pesa, tperm, permli, dperml,&
                    permgz, dperms, dpermp, fick, dfickt,&
                    dfickg, fickad, dfadt, kh, unsurk,&
                    alpha, viscl, dviscl, mamolg, viscg,&
                    dviscg, mamovg, dficks, vf, ifa,&
                    valfac, valcen)
        if (retcom .ne. 0) then
            goto 9000
        endif
    endif
! ======================================================================
! --- CALCUL DU FLUX THERMIQUE UNIQUEMENT SI YATE = 1 ------------------
! ======================================================================
    if (yate .eq. 1) then
        call calcft(option, thmc, imate, ndim, dimdef,&
                    dimcon, yamec, yap1, yap2, addete,&
                    addeme, addep1, addep2, adcote, congep,&
                    dsde, t, grat, phi, pvp,&
                    rgaz, tbiot, satur, dsatur, lambp,&
                    dlambp, lambs, dlambs, tlambt, tdlamt,&
                    mamovg, tlamct, rho11, h11, h12,&
                    angmas, anisof, phenom)
        if (retcom .ne. 0) then
            goto 9000
        endif
    endif
! ======================================================================
9000  continue
! ======================================================================
end subroutine
