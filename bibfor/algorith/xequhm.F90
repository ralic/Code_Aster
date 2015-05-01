subroutine xequhm(imate, option, ta, ta1, ndim,&
                  compor, kpi, npg, dimenr,&
                  enrmec, dimdef, dimcon, nbvari, defgem,&
                  congem, vintm, defgep, congep, vintp,&
                  mecani, press1, press2, tempe,&
                  rinstp, dt, r, drds,&
                  dsde, retcom, idecpg, angmas, enrhyd)
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
! ======================================================================
!     BUT:  CALCUL DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN HM AVEC LA METHODE XFEM
!.......................................................................
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  OPTION  : OPTION DE CALCUL
! IN  DIMDEF  : DIMENSION DU TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS
! IN  DIMCON  : DIMENSION DU TABLEAU DES CONTRAINTES GENERALISEES
!               AU POINT DE GAUSS
! IN  NBVARI  : NOMBRE TOTAL DE VARIABLES INTERNES "MECANIQUES"
! IN  DEFGEP  : TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS AU TEMPS PLUS
! IN  DEFGEM  : TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS AU TEMPS MOINS
!             : EPSXY = (DV/DX+DU/DY)/SQRT(2)
! IN  CONGEM  : TABLEAU DES CONTRAINTES GENERALISEES
!               AU POINT DE GAUSS AU TEMPS MOINS
! IN  VINTM   : TABLEAU DES VARIABLES INTERNES (MECANIQUES ET
!               HYDRAULIQUES)AU POINT DE GAUSS AU TEMPS MOINS
! OUT CONGEP  : TABLEAU DES CONTRAINTES GENERALISEES
!               AU POINT DE GAUSS AU TEMPS PLUS
!             : SIGXY = LE VRAI
! OUT VINTP   : TABLEAU DES VARIABLES INTERNES (MECANIQUES ET HYDRAULIQU
!               AU POINT DE GAUSS AU TEMPS PLUS
! OUT R       : TABLEAU DES RESIDUS
! OUT DRDE    : TABLEAU DE LA MATRICE TANGENTE AU POINT DE GAUSS
! OUT         : RETCOM RETOUR DES LOIS DE COMPORTEMENT
! ======================================================================
#   include "asterfort/xcomhm.h"
#   include "asterfort/vecini.h"
    integer :: imate, ndim, nbvari, kpi, npg, dimdef, dimcon, retcom
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: yamec, addeme, adcome, yate, addete, i, j
    integer :: yap1, nbpha1, addep1, adcp11
    integer :: yap2, nbpha2, addep2
    real(kind=8) :: defgem(1:dimdef), defgep(1:dimdef), congem(1:dimcon)
    real(kind=8) :: congep(1:dimcon), vintm(1:nbvari), vintp(1:nbvari)
    real(kind=8) :: pesa(3), dt, rinstp
    real(kind=8) :: deux, rac2, ta, ta1, p10, p20
    real(kind=8) :: angmas(3)
    parameter    (deux = 2.d0)
    character(len=16) :: option, compor(*)
!
! DECLARATIONS POUR XFEM
    integer :: dimenr, enrmec(3), enrhyd(3)
    integer :: yaenrm, adenme, idecpg
    integer :: yaenrh, adenhy
    real(kind=8) :: r(1:dimenr), drds(1:dimenr, 1:dimcon)
    real(kind=8) :: dsde(1:dimcon, 1:dimenr)
! ======================================================================
! --- INITIALISATIONS DES VARIABLES DEFINISSANT LE PROBLEME ------------
! ======================================================================
    rac2 = sqrt(deux)
    yamec = mecani(1)
    addeme = mecani(2)
    adcome = mecani(3)
    yap1 = press1(1)
    nbpha1 = press1(2)
    addep1 = press1(3)
    adcp11 = press1(4)
    yap2 = press2(1)
    nbpha2 = press2(2)
    addep2 = press2(3)
    yate = tempe(1)
    addete = tempe(2)
!
    yaenrm = enrmec(1)
    adenme = enrmec(2)
    yaenrh = enrhyd(1)
    adenhy = enrhyd(2)
!
    p10 = 0.d0
    p20 = 0.d0
    call vecini(3, 0.d0, pesa)
!
! ============================================================
! --- COMME CONGEM CONTIENT LES VRAIES CONTRAINTES ET --------
! --- COMME PAR LA SUITE ON TRAVAILLE AVEC SQRT(2)*SXY -------
! --- ON COMMENCE PAR MODIFIER LES CONGEM EN CONSEQUENCE -----
! ============================================================
    if (yamec .eq. 1) then
        do 100 i = 4, 6
            congem(adcome+i-1)= congem(adcome+i-1)*rac2
            congem(adcome+6+i-1)= congem(adcome+6+i-1)*rac2
100     continue
    endif
! ============================================================
! --- INITIALISATION DES TABLEAUX A ZERO ---------------------
! --- ET DU TABLEAU CONGEP A CONGEM --------------------------
! ============================================================
    if ((option .eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 1 i = 1, dimcon
            congep(i)=congem(i)
  1     continue
    endif
!
    do 2 i = 1, dimenr
        do 3 j = 1, dimcon
            drds(i,j)=0.d0
            dsde(j,i)=0.d0
  3     continue
        r(i)=0.d0
  2 continue 
!
    retcom = 0
!
    call xcomhm(option, imate, compor,&
                rinstp, ndim, dimdef, dimcon,&
                nbvari, yamec, yap1, yap2, yate,&
                addeme, adcome, addep1, adcp11,&
                addep2, addete,&
                defgem, defgep, congem, congep, vintm,&
                vintp, dsde, pesa, retcom, kpi,&
                npg, p10, p20, yaenrm, dimenr,&
                idecpg, angmas, yaenrh, adenhy)
!
    if (retcom .ne. 0) then
        goto 900
    endif
! ======================================================================
! --- CALCUL DE LA CONTRAINTE VIRTUELLE R ------------------------------
! ======================================================================
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
! ======================================================================
! --- SI PRESENCE DE MECANIQUE -----------------------------------------
! ======================================================================
        if (yamec .eq. 1) then
            do 6 i = 1, 6
                r(addeme+ndim+i-1)= r(addeme+ndim+i-1) +congep(&
                adcome-1+i)
  6         continue
!
            do 7 i = 1, 6
                r(addeme+ndim-1+i)=r(addeme+ndim-1+i)+congep(&
                adcome+6+i-1)
  7         continue
!
            if (yap1 .eq. 1) then
                do 8 i = 1, ndim
                    r(addeme+i-1)=r(addeme+i-1) - pesa(i)*congep(&
                    adcp11)
  8             continue
            endif
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS1 --------------------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
!
            r(addep1)=r(addep1)-congep(adcp11)+congem(adcp11)
!
            do 12 i = 1, ndim
                r(addep1+i)=r(addep1+i) +dt*(ta*congep(adcp11+i)+ta1*&
                congem(adcp11+i))
 12         continue
        endif
! ======================================================================
! --- SI PRESENCE DE MECANIQUE AVEC XFEM -------------------------------
! ======================================================================
        if (yaenrm .eq. 1) then
            if (yamec .eq. 1) then
                if (yap1 .eq. 1) then
                    do 15 i = 1, ndim
                        r(adenme+i-1)=r(adenme+i-1) - pesa(i)*congep(&
                        adcp11)
 15                 continue
                endif
            endif
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS1 AVEC XFEM ----------------------------------
! ======================================================================
         if(yaenrh.eq.1) then
            if(yap1.eq.1) then

               r(adenhy)=r(adenhy)-congep(adcp11)+congem(adcp11)

            endif
         endif
    endif
! ======================================================================
! --- CALCUL DES MATRICES DERIVEES CONSTITUTIVES DE DF -----------------
! ======================================================================
    if ((option(1:9) .eq. 'RIGI_MECA') .or. (option(1:9) .eq. 'FULL_MECA')) then
! ======================================================================
! --- SI PRESENCE DE MECANIQUE -----------------------------------------
! ======================================================================
        if (yamec .eq. 1) then
            do 25 i = 1, 6
                drds(addeme+ndim-1+i,adcome+i-1)= drds(addeme+ndim-1+&
                i,adcome+i-1)+1.d0
 25         continue
!
            do 26 i = 1, 6
                drds(addeme+ndim-1+i,adcome+6+i-1)= drds(addeme+ndim-&
                1+i,adcome+6+i-1)+1.d0
 26         continue
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS1 --------------------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
            if (yamec .eq. 1) then
                do 27 i = 1, ndim
                    drds(addeme+i-1,adcp11)= drds(addeme+i-1,adcp11)-&
                    pesa(i)
 27             continue
            endif
!
            drds(addep1,adcp11)=drds(addep1,adcp11)-1.d0
!
            do 28 i = 1, ndim
                drds(addep1+i,adcp11+i)=drds(addep1+i,adcp11+i)+ta*dt
 28         continue
        endif
! ======================================================================
! --- SI PRESENCE DE PRESS1 AVEC XFEM ----------------------------------
! ======================================================================
        if ((yap1.eq.1).and.(yaenrh.eq.1)) then
            if ((yamec.eq.1).and.(yaenrm.eq.1)) then
                    do 31 i = 1, ndim
                        drds(adenme+i-1,adcp11)= drds(adenme+i-1,&
                        adcp11)-pesa(i)
 31                 continue
            endif
            drds(adenhy,adcp11)=drds(adenhy,adcp11)-1.d0
        endif
    endif
! ======================================================================
! --- FIN DU CALCUL DE DF ----------------------------------------------
! ======================================================================
! --- COMME CONGEP DOIT FINALEMENT CONTENIR LES VRAIES CONTRAINTES -----
! --- ET COMME  ON A TRAVAILLE AVEC SQRT(2)*SXY ------------------------
! --- ON MODIFIE LES CONGEP EN CONSEQUENCE -----------------------------
! ======================================================================
    if ((yamec.eq.1) .and. ((option .eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA'))) then
        do 110 i = 4, 6
            congep(adcome+i-1)= congep(adcome+i-1)/rac2
            congep(adcome+6+i-1)= congep(adcome+6+i-1)/rac2
            congem(adcome+i-1)= congem(adcome+i-1)/rac2
            congem(adcome+6+i-1)= congem(adcome+6+i-1)/rac2
110     continue
    endif
! ======================================================================
900 continue
! ======================================================================
end subroutine
