subroutine dilder(interp, dimdef, dimcon, ndim, regula,&
                  rpena, dsde2g, drde)
! ======================================================================
    implicit     none
    integer :: dimdef, dimcon, ndim, regula(6)
    real(kind=8) :: dsde2g(ndim, ndim), drde(dimcon, dimdef), rpena
    character(len=2) :: interp
! ======================================================================
! person_in_charge: romeo.fernandes at edf.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- BUT : MISE A JOUR DE L OPERATEUR TANGENT POUR LA PARTIE ----------
! ---       SECOND GRADIENT A MIDCRO DILATATION AU POINT D INTEGRATION -
! ======================================================================
    integer :: i, j, adder1, adder2, adder3, adcor1, adcor2, adcor3
! ======================================================================
    adder1=regula(1)
    adder2=regula(2)
    adder3=regula(3)
    adcor1=regula(4)
    adcor2=regula(5)
    adcor3=regula(6)
! ======================================================================
    do 10 i = 1, dimdef
        do 20 j = 1, dimcon
            drde(j,i)=0.0d0
20      continue
10  end do
! ======================================================================
    drde(adcor1,adder1)=drde(adcor1,adder1)+rpena
    do 30 i = 1, ndim
        do 40 j = 1, ndim
            drde(adcor2-1+j,adder2-1+i)=drde(adcor2-1+j,adder2-1+i)+&
            dsde2g(j,i)
40      continue
30  end do
    if (interp .ne. 'SL') then
        drde(adcor1,adder3)=drde(adcor1,adder3)+1.0d0
        drde(adcor3,adder1)=drde(adcor3,adder1)-1.0d0
    endif
! ======================================================================
end subroutine
