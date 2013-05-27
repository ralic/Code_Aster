subroutine rdif01(fami, kpg, ksp, comp, mod,&
                  imat, matcst, nbcomm, cpmono, nfs,&
                  nsg, toutms, nvi, nmat, vini,&
                  cothe, coeff, dcothe, dcoeff, pgl,&
                  nbphas, coel, x, dtime, neps,&
                  epsd, detot, dvin, nhsr, numhsr,&
                  hsr, itmax, toler, iret)
    implicit none
!     ================================================================
!          CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21 CRS_1404
!     ----------------------------------------------------------------
!     ROUTINE D AIGUILLAGE
!     ----------------------------------------------------------------
!     INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE
!     PAR UNE METHODE DE RUNGE KUTTA
!     ----------------------------------------------------------------
!     IN  FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!     IN  KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!     IN  COMP     :  NOM DU MODELE DE COMPORTEMENT
!         MOD     :  TYPE DE MODELISATION
!         IMAT    :  ADRESSE DU MATERIAU CODE
!         MATCST  :  NATURE DES PARAMETRES INELASTIQUES
!         NVI     :  NOMBRE DE VARIABLES INTERNES
!         NMAT    :  NOMBRE DE PARAMETRES MATERIAU INELASTIQUE
!         VINI    :  VARIABLES INTERNES A T
!         COTHE   :  COEFFICIENTS MATERIAU ELASTIQUE A T
!         COEFF   :  COEFFICIENTS MATERIAU INELASTIQUE A T
!         DCOTHE  :  DELTA COEFFICIENTS MATERIAU ELASTIQUE A T+DT
!         DCOEFF  :  DELTA COEFFICIENTS MATERIAU INELASTIQUE A T+DT
!         COEL    :  COEFFICIENTS D'ELASTICITE
!         X       :  INTERVALE DE TEMPS ADAPTATIF
!         DTIME   :  INTERVALE DE TEMPS
!         EPSD    :  DEFORMATION TOTALE A T
!         DETOT   :  INCREMENT DE DEFORMATION TOTALE
!         DVIN    :  DERIVEES DES VARIABLES INTERNES A T
!     ----------------------------------------------------------------
    include 'asterfort/calsig.h'
    include 'asterfort/coefft.h'
    include 'asterfort/lcdvin.h'
    include 'asterfort/lcmmon.h'
    include 'asterfort/lcmmop.h'
    integer :: kpg, ksp, imat, nmat, nvi, nbcomm(nmat, 3), itens
    integer :: nbphas, nfs, iret, itmax, nsg, nhsr, numhsr(*), neps
    character(len=16) :: loi, comp(*)
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: mod
    character(len=*) :: fami
    character(len=3) :: matcst
    real(kind=8) :: pgl(3, 3), toler, x, dtime, coel(nmat)
    real(kind=8) :: cothe(nmat), dcothe(nmat), coeff(nmat), dcoeff(nmat)
    real(kind=8) :: epsd(6), detot(6), coeft(nmat), xm, sigi(6)
    real(kind=8) :: vini(nvi), dvin(nvi), hsr(nsg, nsg, nhsr), evi(6)
!     POUR GAGNER EN TEMPS CPU
    real(kind=8) :: toutms(*)
!
    loi=comp(1)
    if (loi(1:8) .eq. 'MONOCRIS') then
!       PAS DE VARIATION DES COEF AVEC LA TEMPERATURE
        xm=0.d0
        call coefft(cothe, coeff, dcothe, dcoeff, xm,&
                    dtime, coeft, nmat, coel)
        call lcmmon(fami, kpg, ksp, comp, nbcomm,&
                    cpmono, nmat, nvi, vini, x,&
                    dtime, pgl, mod, coeft, neps,&
                    epsd, detot, coel, dvin, nfs,&
                    nsg, toutms, hsr(1, 1, 1), itmax, toler,&
                    iret)
!
    else if (loi(1:8).eq.'POLYCRIS') then
!       PAS DE VARIATION DES COEF AVEC LA TEMPERATURE
        xm=0.d0
        call coefft(cothe, coeff, dcothe, dcoeff, xm,&
                    dtime, coeft, nmat, coel)
        call lcmmop(fami, kpg, ksp, comp, nbcomm,&
                    cpmono, nmat, nvi, vini, x,&
                    dtime, mod, coeft, epsd, detot,&
                    coel, nbphas, nfs, nsg, toutms,&
                    dvin, nhsr, numhsr, hsr, itmax,&
                    toler, iret)
!
    else
!
        do 5 itens = 1, 6
            evi(itens) = vini(itens)
 5      continue
!
        call coefft(cothe, coeff, dcothe, dcoeff, x,&
                    dtime, coeft, nmat, coel)
!
!
        call calsig(fami, kpg, ksp, evi, mod,&
                    comp, vini, x, dtime, epsd,&
                    detot, nmat, coel, sigi)
!
        call lcdvin(fami, kpg, ksp, comp, mod,&
                    imat, matcst, nvi, nmat, vini,&
                    coeft, x, dtime, sigi, dvin,&
                    iret)
!
    endif
end subroutine
