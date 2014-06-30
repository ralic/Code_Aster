subroutine fonoei(ndim, dt, fnoevo, dimdef, dimcon,&
                  yamec, yap1, yap2, yate, addeme,&
                  addep1, addep2, addete, addlh1, adcome,&
                  adcp11, adcp12, adcp21, adcp22, adcote,&
                  adcop1, adcop2, nbpha1, nbpha2, congem,&
                  r)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! ======================================================================
!
!    BUT : CALCUL DU TERME DE CHARGEMENT EXTERIEUR AUX POINTS
!    D'INTEGRATION
!
!
! ======================================================================
! IN NDIM  : DIMENSION ESPACE
! IN DT    : INCREMENT DE TEMPS
! IN FNOEVO : APPLE DEPUIS STAT_NON_LINE
! IN DIMDEF : DIMENSION DEFORMATIONS GENERALISEES
! IN DIMCON : DIMENSION VECTEUR CONTRAINTES GENERALISEES
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
! IN ADCP12 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 2
! IN ADCOP1 : ADRESSE DES CONTRAINTES CORRESPONDANT AU SAUT DE PRE1
! IN ADCP21 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 1
! IN ADCP22 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 2
! IN ADCOP2 : ADRESSE DES CONTRAINTES CORRESPONDANT AU SAUT DE PRE2
! IN ADCOTE : ADRESSE DES CONTRAINTES THERMIQUES
! IN NBPHA1 : = 1 SI DEUXIEME PHASE
! IN NBPHA2 : = 1 SI DEUXIEME PHASE
! IN CONGEM : CONTRAINTES GENERALISEES AU TEMPS MOINS
! ======================================================================
! OUT R : VECTEUR FORCES EXTERIEURES
! ======================================================================
! aslint: disable=W1504
    implicit     none
    logical(kind=1) :: fnoevo
    integer :: dimdef, dimcon
    integer :: ndim
    real(kind=8) :: dt, congem(dimcon), r(dimdef)
! ======================================================================
    integer :: yamec, yap1, yap2, yate, addeme, adcome, nbpha1
    integer :: addep1, adcp11, adcp12, nbpha2, addep2, adcp21, adcp22
    integer :: adcote, i, adcop1, adcop2, f, addete, addlh1
!
! ======================================================================
! --- CALCUL DU RESIDU R -----------------------------------------------
! ======================================================================
! ======================================================================
! -------------------------------------
! ======================================================================
    do 6 i = 1, ndim
        r(addeme+i-1)= r(addeme+i-1)+congem(adcome-1+i)
 6  continue
    r(addeme) = r(addeme) + congem(adcome+ndim)
!
! ======================================================================
! --- CONTRIBUTION A R DEPENDANTE DE YAP1 ------------------------------
! ======================================================================
    if (yap1 .eq. 1) then
        do 8 f = 1, 2
            r(addlh1+1+f)=r(addlh1+1+f)+congem(adcop1+1+f)
 8      continue
    endif
! ======================================================================
! --- CONTRIBUTIONS A R DEPENDANTE DE YAP2 -----------------------------
! ======================================================================
    if (yap2 .eq. 1) then
        do 11 f = 1, 2
            r(addep2+ndim+1+f)=r(addep1+ndim+1+f)+congem(adcop2+1+f)
11      continue
    endif
!
! ======================================================================
    if (fnoevo) then
! ======================================================================
! --- TERMES DEPENDANT DE DT DANS FORC_NODA POUR STAT_NON_LINE ---------
! ======================================================================
        if (yap1 .eq. 1) then
            do 12 f = 1, 2
                r(addep1) = r(addep1) + dt*congem(adcop1-1+f)
                r(addlh1-1+f) = -dt*congem(adcop1-1+f)
12          continue
            do 13 i = 1, ndim-1
                r(addep1+i) = dt*congem(adcp11+i)
13          continue
        endif
    endif
!
! ======================================================================
end subroutine
