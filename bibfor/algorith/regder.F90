subroutine regder(dimdef, dimcon, ndim, regula, dsde2g,&
                  drde)
! ======================================================================
    implicit none
    integer :: dimdef, dimcon, ndim, regula(6)
    real(kind=8) :: dsde2g(ndim*ndim*ndim, ndim*ndim*ndim)
    real(kind=8) :: drde(dimcon, dimdef)
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
! --- BUT : MISE A JOUR DE L OPERATEUR TANGENT POUR LA PARTIE ----------
! ---       SECOND GRADIENT AU POINT D INTEGRATION ---------------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, adder1, adder2, adder3, adcor1, adcor2, adcor3
    integer :: dimde1, dimde2, dimde3
! ======================================================================
    adder1 = regula(1)
    adder2 = regula(2)
    adder3 = regula(3)
    adcor1 = regula(4)
    adcor2 = regula(5)
    adcor3 = regula(6)
    dimde1 = adder2-adder1
    dimde2 = adder3-adder2
    dimde3 = dimdef-adder3+1
! ======================================================================
    do 10 i = 1, dimdef
        do 20 j = 1, dimcon
            drde(j,i)=0.0d0
20      continue
10  end do
! ======================================================================
    do 30 i = 1, dimde1
        drde(adcor1-1+i,adder3-1+i)=drde(adcor1-1+i,adder3-1+i)+1.0d0
30  end do
    do 40 i = 1, dimde2
        do 50 j = 1, dimde2
            drde(adcor2-1+j,adder2-1+i)=drde(adcor2-1+j,adder2-1+i)+&
            dsde2g(j,i)
50      continue
40  end do
    do 60 i = 1, dimde3
        drde(adcor3-1+i,adder1-1+i)=drde(adcor3-1+i,adder1-1+i)-1.0d0
60  end do
! ======================================================================
end subroutine
