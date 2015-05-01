subroutine regcge(dimdef, dimcon, regula, ndim, defgep,&
                  sigp, r)
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- BUT : MISE A JOUR DU CHAMP DE CONTRAINTES GENERALISEES -----------
! ======================================================================
    implicit      none
    integer :: dimdef, dimcon, regula(6), ndim
    real(kind=8) :: sigp(ndim*ndim*ndim), defgep(dimdef), r(dimcon)
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, adder1, adder2, adder3, adcor1, adcor2, adcor3
    integer :: dimde1, dimde2, dimde3
! ======================================================================
! --- DEFINITION DES DONNEES INITIALES ---------------------------------
! ======================================================================
    adder1 = regula(1)
    adder2 = regula(2)
    adder3 = regula(3)
    adcor1 = regula(4)
    adcor2 = regula(5)
    adcor3 = regula(6)
    dimde1 = adder2 - adder1
    dimde2 = adder3 - adder2
    dimde3 = dimdef - adder3 + 1
    do 10 i = 1, dimde1
        r(adcor1-1+i) = defgep(adder3-1+i)
10  end do
    do 20 i = 1, dimde2
        r(adcor2-1+i) = sigp(i)
20  end do
    do 30 i = 1, dimde3
        r(adcor3-1+i) = -defgep(adder1-1+i)
30  end do
! ======================================================================
end subroutine
