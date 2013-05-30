subroutine lcmhsr(necoul, necris, nbsys, nbcoef, coefh,&
                  nsg, hsr)
    implicit none
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
! person_in_charge: jean-michel.proix at edf.fr
!     ----------------------------------------------------------------
!     MONOCRISTAL : CALCUL DE LA MATRICE D'INTERACTION HSR
!     ----------------------------------------------------------------
!     IN  NOMFAM :  NOM DE LA FAMILLE DE GLISSEMENT
!         NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT
!         NBCOEF  :  NOMBRE DE COEFFICIENTS
!         COEFH  :  COEFFICIENTS H1 A H6
!     OUT HSR    :  MATRICE D'INTERACTION
!     ----------------------------------------------------------------
    include 'asterfort/lcicma.h'
    include 'asterfort/lcmhdd.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    integer :: nbcoef, nbsys, ir, is, i, j, nsg
    real(kind=8) :: coefh(6), hsr(nsg, nsg), h
    real(kind=8) :: a1(3, 3), a2(3, 3), a3(3, 3), a4(3, 3)
    character(len=16) :: necris, necoul
!     ----------------------------------------------------------------
!
!
    if (nbcoef .eq. 1) then
        h=coefh(1)
!  MATRICE D INTERACTION (NBSYS*NBSYS): 1 SUR LA DIAGONALE, H AILLEURS
        do 507 is = 1, nbsys
            do 508 ir = 1, nbsys
                hsr(is,ir) = h
508          continue
507      continue
        do 509 is = 1, nbsys
            hsr(is,is) = 1.d0
509      continue
!
    else if (necris(1:9).eq.'MONO_DD_C') then
!
!  MATRICE D INTERACTION (12*12): 5 COEFFICIENTS DD_CFC
!  DEFINITION SELON G.MONET
        call lcmhdd(necoul, necris, nbsys, nbcoef, coefh,&
                    nsg, hsr)
!
    else if (nbcoef.eq.4) then
!
!  MATRICE D INTERACTION (24*24): 4 COEFFICIENTS (BCC24)
!
        if (nbsys .ne. 24) call u2mess('F', 'COMPOR1_23')
!
!
        call r8inir(3*3, coefh(1), a1, 1)
        call r8inir(3*3, coefh(2), a2, 1)
        call r8inir(3*3, coefh(3), a3, 1)
        call r8inir(3*3, coefh(4), a4, 1)
!
!         DEFINITION DE LA MATRICE D INTERACTION BCC24
!
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    16, 1)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 1)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 1)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    13, 4)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 4)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 4)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    13, 7)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    16, 7)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 7)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    13, 10)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    16, 10)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 10)
!
!
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    1, 1)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    4, 4)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    7, 7)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    10, 10)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    13, 1)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    16, 4)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 7)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 10)
!
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    16, 13)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 13)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 13)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 16)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 16)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 19)
!
        call lcicma(a4, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    13, 13)
        call lcicma(a4, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    16, 16)
        call lcicma(a4, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    19, 19)
        call lcicma(a4, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    22, 22)
!
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    4, 1)
        hsr(4,1)=coefh(2)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    7, 1)
        hsr(7,2)=coefh(2)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    10, 1)
        hsr(10,3)=coefh(2)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    7, 4)
        hsr(8,6)=coefh(2)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    10, 4)
        hsr(12,5)=coefh(2)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hsr, 30, 30,&
                    10, 7)
        hsr(11,9)=coefh(2)
!
    else if (nbcoef.eq.6) then
!
!  MATRICE D INTERACTION (12*12): 6 COEFFICIENTS CF ZMAT
!
        if (nbsys .ne. 12) call u2mess('F', 'COMPOR1_24')
!
        do 10 i = 1, 12
            hsr(i,i)=coefh(1)
10      continue
        hsr(2,1)=coefh(2)
        hsr(3,1)=coefh(2)
        hsr(3,2)=coefh(2)
        hsr(4,1)=coefh(4)
        hsr(4,2)=coefh(5)
        hsr(4,3)=coefh(5)
        hsr(5,1)=coefh(5)
        hsr(5,2)=coefh(3)
        hsr(5,3)=coefh(6)
        hsr(5,4)=coefh(2)
        hsr(6,1)=coefh(5)
        hsr(6,2)=coefh(6)
        hsr(6,3)=coefh(3)
        hsr(6,4)=coefh(2)
        hsr(6,5)=coefh(2)
        hsr(7,1)=coefh(5)
        hsr(7,2)=coefh(4)
        hsr(7,3)=coefh(5)
        hsr(7,4)=coefh(6)
        hsr(7,5)=coefh(3)
        hsr(7,6)=coefh(5)
        hsr(8,1)=coefh(6)
        hsr(8,2)=coefh(5)
        hsr(8,3)=coefh(3)
        hsr(8,4)=coefh(5)
        hsr(8,5)=coefh(5)
        hsr(8,6)=coefh(4)
        hsr(8,7)=coefh(2)
        hsr(9,1)=coefh(3)
        hsr(9,2)=coefh(5)
        hsr(9,3)=coefh(6)
        hsr(9,4)=coefh(3)
        hsr(9,5)=coefh(6)
        hsr(9,6)=coefh(5)
        hsr(9,7)=coefh(2)
        hsr(9,8)=coefh(2)
        hsr(10,1)=coefh(5)
        hsr(10,2)=coefh(5)
        hsr(10,3)=coefh(4)
        hsr(10,4)=coefh(6)
        hsr(10,5)=coefh(5)
        hsr(10,6)=coefh(3)
        hsr(10,7)=coefh(6)
        hsr(10,8)=coefh(3)
        hsr(10,9)=coefh(5)
        hsr(11,1)=coefh(3)
        hsr(11,2)=coefh(6)
        hsr(11,3)=coefh(5)
        hsr(11,4)=coefh(3)
        hsr(11,5)=coefh(5)
        hsr(11,6)=coefh(6)
        hsr(11,7)=coefh(5)
        hsr(11,8)=coefh(5)
        hsr(11,9)=coefh(4)
        hsr(11,10)=coefh(2)
        hsr(12,1)=coefh(6)
        hsr(12,2)=coefh(3)
        hsr(12,3)=coefh(5)
        hsr(12,4)=coefh(5)
        hsr(12,5)=coefh(4)
        hsr(12,6)=coefh(5)
        hsr(12,7)=coefh(3)
        hsr(12,8)=coefh(6)
        hsr(12,9)=coefh(5)
        hsr(12,10)=coefh(2)
        hsr(12,11)=coefh(2)
!
    else
        call u2mess('F', 'COMPOR1_25')
    endif
!
    do 1 i = 1, nbsys
        do 1 j = 1, i
            hsr(j,i)=hsr(i,j)
 1      continue
!
end subroutine
