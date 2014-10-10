subroutine xfnoda(imate, mecani, press1, enrmec, dimenr,&
                  dimcon, ndim, dt, fnoevo, congem,&
                  r)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/rcvalb.h"
    aster_logical :: fnoevo
    integer :: mecani(5), press1(7), enrmec(3), dimenr, dimcon
    integer :: ndim, imate, yaenrm, adenme
    real(kind=8) :: dt, congem(dimcon), r(dimenr)
! ======================================================================
    integer :: nhom, yamec, yap1, addeme, adcome
    integer :: addep1, adcp11, i
    parameter    (nhom=3)
    real(kind=8) :: hom(nhom), pesa(3), rac2
    integer :: icodre(nhom)
    character(len=8) :: ncra5(nhom)
    data ncra5 / 'PESA_X','PESA_Y','PESA_Z' /
! ======================================================================
! --- RECUPERATION DE LA PESANTEUR DANS DEFI_MATERIAU ------------------
! ======================================================================
    call rcvalb('FPG1', 1, 1, '+', imate,&
                ' ', 'THM_DIFFU', 0, ' ', [0.d0],&
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
    addep1 = press1(3)
    adcp11 = press1(4)
    adcome = mecani(3)
    yaenrm = enrmec(1)
    adenme = enrmec(2)
! ======================================================================
! --- COMME CONGEM CONTIENT LES VRAIES CONTRAINTES ET ------------------
! --- COMME PAR LA SUITE ON TRAVAILLE AVEC SQRT(2)*SXY -----------------
! --- ON COMMENCE PAR MODIFIER LES CONGEM EN CONSEQUENCE ---------------
! ======================================================================
    if (yamec .eq. 1) then
        do 100 i = 4, 6
            congem(adcome+6+i-1) = congem(adcome+6+i-1)*rac2
            congem(adcome+i-1) = congem(adcome+i-1)*rac2
100     continue
    endif
! ======================================================================
! --- CALCUL DU RESIDU R (TERMES CLASSIQUES ) --------------------------
! ======================================================================
    if (yamec .eq. 1) then
        do 6 i = 1, 6
            r(addeme+ndim+i-1)= r(addeme+ndim+i-1)+congem(adcome-1+i)
  6     continue
        do 7 i = 1, 6
            r(addeme+ndim-1+i)=r(addeme+ndim-1+i)+congem(adcome+6+i-1)
  7     continue
!
        if (yap1 .eq. 1) then
            do 8 i = 1, ndim
                r(addeme+i-1)=r(addeme+i-1) - pesa(i)*congem(adcp11)
  8         continue
        endif
    endif
! ======================================================================
! --- CALCUL DU RESIDU R (TERMES HEAVISIDE ) ---------------------------
! ======================================================================
    if (yaenrm .eq. 1) then
        if (yamec .eq. 1) then
            do 9 i = 1, 6
                r(adenme+ndim+i-1)=r(adenme+ndim+i-1) +congem(&
                adcome-1+i)
  9         continue
            do 10 i = 1, 6
                r(adenme+ndim-1+i)=r(adenme+ndim-1+i)+congem(adcome+6+i-1)
 10         continue
!
            if (yap1 .eq. 1) then
                do 11 i = 1, ndim
                    r(adenme+i-1)=r(adenme+i-1)-pesa(i)*congem(adcp11)
 11             continue
            endif
        endif
    endif
! ======================================================================
    if (fnoevo) then
! ======================================================================
! --- TERMES DEPENDANT DE DT DANS FORC_NODA POUR STAT_NON_LINE ---------
! ======================================================================
        if (yap1 .eq. 1) then
            do 112 i = 1, ndim
                r(addep1+i)=r(addep1+i)+dt*congem(adcp11+i)
112         continue
        endif
    endif
! ======================================================================
end subroutine
