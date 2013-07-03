subroutine fonoda(imate, perman, mecani, press1, press2,&
                  tempe, dimdef, dimcon, ndim, dt,&
                  fnoevo, congem, r)
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
    implicit     none
#include "asterfort/rcvalb.h"
    logical :: fnoevo
    logical :: perman
    integer :: mecani(5), press1(7), press2(7), tempe(5), dimdef, dimcon
    integer :: ndim, imate
    real(kind=8) :: dt, congem(dimcon), r(dimdef+1)
! ======================================================================
    integer :: nhom, yamec, yap1, yap2, yate, addeme, adcome, nbpha1, addete
    integer :: addep1, adcp11, adcp12, nbpha2, addep2, adcp21, adcp22
    integer :: adcote, i
    parameter   (nhom=3)
    real(kind=8) :: hom(nhom), pesa(3), rac2
    integer :: icodre(nhom)
    character(len=8) :: ncra5(nhom)
    data ncra5 / 'PESA_X','PESA_Y','PESA_Z' /
! ======================================================================
! --- RECUPERATION DE LA PESANTEUR DANS DEFI_MATERIAU ------------------
! ======================================================================
    call rcvalb('FPG1', 1, 1, '+', imate,&
                ' ', 'THM_DIFFU', 0, ' ', 0.d0,&
                nhom, ncra5, hom, icodre, 1)
    pesa(1)=hom(1)
    pesa(2)=hom(2)
    pesa(3)=hom(3)
    rac2 = sqrt(2.0d0)
! ======================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
! ======================================================================
    yamec = mecani(1)
    addeme = mecani(2)
    yap1 = press1(1)
    nbpha1 = press1(2)
    addep1 = press1(3)
    if (perman) then
        i = 1
    else
        i = 0
    endif
    adcp11 = press1(4) - i
    adcp12 = press1(5) - i
    adcome = mecani(3)
    yap2 = press2(1)
    nbpha2 = press2(2)
    addep2 = press2(3)
    adcp21 = press2(4) - i
    adcp22 = press2(5) - i
    yate = tempe(1)
    addete = tempe(2)
    adcote = tempe(3)
! ======================================================================
! --- COMME CONGEM CONTIENT LES VRAIES CONTRAINTES ET ------------------
! --- COMME PAR LA SUITE ON TRAVAILLE AVEC SQRT(2)*SXY -----------------
! --- ON COMMENCE PAR MODIFIER LES CONGEM EN CONSEQUENCE ---------------
! ======================================================================
    if (yamec .eq. 1) then
        do 100 i = 4, 6
            congem(adcome+i-1) = congem(adcome+i-1)*rac2
100      continue
    endif
! ======================================================================
! --- CALCUL DU RESIDU R -----------------------------------------------
! ======================================================================
    if (yamec .eq. 1) then
! ======================================================================
! --- CONTRIBUTIONS A R2 INDEPENDANTE DE YAP1, YAP2 ET YATE ------------
! --- CONTRAINTES SIGPRIMPLUS PAGE 33 ----------------------------------
! ======================================================================
        do 6 i = 1, 6
            r(addeme+ndim+i-1)= r(addeme+ndim+i-1)+congem(adcome-1+i)
 6      continue
! ======================================================================
! --- SCALAIRE SIGPPLUS MULTIPLIE PAR LE TENSEUR UNITE -----------------
! ======================================================================
        do 7 i = 1, 3
            r(addeme+ndim-1+i)=r(addeme+ndim-1+i)+congem(adcome+6)
 7      continue
! ======================================================================
! --- CONTRIBUTION A R DEPENDANTE DE YAP1 ------------------------------
! ======================================================================
        if (yap1 .eq. 1) then
            do 8 i = 1, ndim
                r(addeme+i-1)=r(addeme+i-1) - pesa(i)*congem(adcp11)
 8          continue
            if (nbpha1 .gt. 1) then
                do 9 i = 1, ndim
                    r(addeme+i-1)=r(addeme+i-1)- pesa(i)*congem(&
                    adcp12)
 9              continue
            endif
        endif
! ======================================================================
! --- CONTRIBUTIONS A R DEPENDANTE DE YAP2 -----------------------------
! ======================================================================
        if (yap2 .eq. 1) then
            do 11 i = 1, ndim
                r(addeme+i-1)=r(addeme+i-1)- pesa(i)*congem(adcp21)
11          continue
            if (nbpha2 .gt. 1) then
                do 12 i = 1, ndim
                    r(addeme+i-1)=r(addeme+i-1)- pesa(i)*congem(&
                    adcp22)
12              continue
            endif
        endif
    endif
! ======================================================================
    if (fnoevo) then
! ======================================================================
! --- TERMES DEPENDANT DE DT DANS FORC_NODA POUR STAT_NON_LINE ---------
! ======================================================================
        if (yap1 .eq. 1) then
!
            do 112 i = 1, ndim
                r(addep1+i)=r(addep1+i)+dt*congem(adcp11+i)
112          continue
!
            if (nbpha1 .gt. 1) then
                do 13 i = 1, ndim
                    r(addep1+i)=r(addep1+i)+dt*congem(adcp12+i)
13              continue
            endif
!
            if (yate .eq. 1) then
!
                do 14 i = 1, ndim
                    r(addete)=r(addete)+dt*congem(adcp11+i)*pesa(i)
14              continue
!
                if (nbpha1 .gt. 1) then
                    do 15 i = 1, ndim
                        r(addete)=r(addete) +dt*congem(adcp12+i)*pesa(&
                        i)
15                  continue
                endif
!
                do 16 i = 1, ndim
                    r(addete+i)=r(addete+i)+ dt*congem(adcp11+ndim+1)*&
                    congem(adcp11+i)
16              continue
!
                if (nbpha1 .gt. 1) then
                    do 17 i = 1, ndim
                        r(addete+i)=r(addete+i)+ dt*congem(adcp12+&
                        ndim+1)*congem(adcp12+i)
17                  continue
                endif
!
            endif
        endif
!
        if (yap2 .eq. 1) then
            do 18 i = 1, ndim
                r(addep2+i)=r(addep2+i)+dt*congem(adcp21+i)
18          continue
            if (nbpha2 .gt. 1) then
                do 19 i = 1, ndim
                    r(addep2+i)=r(addep2+i)+dt*congem(adcp22+i)
19              continue
            endif
!
            if (yate .eq. 1) then
                do 20 i = 1, ndim
                    r(addete)=r(addete)+dt*congem(adcp21+i)*pesa(i)
20              continue
                if (nbpha2 .gt. 1) then
                    do 21 i = 1, ndim
                        r(addete)=r(addete)+dt*congem(adcp22+i)*pesa(&
                        i)
21                  continue
                endif
                do 122 i = 1, ndim
                    r(addete+i)=r(addete+i)+ dt*congem(adcp21+ndim+1)*&
                    congem(adcp21+i)
122              continue
                if (nbpha2 .gt. 1) then
                    do 23 i = 1, ndim
                        r(addete+i)=r(addete+i)+ dt*congem(adcp22+&
                        ndim+1)*congem(adcp22+i)
23                  continue
                endif
            endif
        endif
!
        if (yate .eq. 1) then
            do 24 i = 1, ndim
                r(addete+i)=r(addete+i)+dt*congem(adcote+i)
24          continue
        endif
    endif
! ======================================================================
end subroutine
