subroutine lcmhdd(necoul, necris, nbsys, nbcoef, coefh,&
                  nsg, hsr)
    implicit none
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
! person_in_charge: jean-michel.proix at edf.fr
!     ----------------------------------------------------------------
!     MONOCRISTAL : CALCUL DE LA MATRICE D'INTERACTION HSR POUR DD_CFC
!     ----------------------------------------------------------------
!     IN  NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT
!         NBCOEF  :  NOMBRE DE COEFFICIENTS
!         COEFH  :  COEFFICIENTS H1 A H6
!     OUT HSR    :  MATRICE D'INTERACTION
!     ----------------------------------------------------------------
#include "asterfort/lcicma.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mess.h"
    integer :: nbcoef, nbsys, i, j, nn(12), idbg, nsg
    real(kind=8) :: coefh(6), hsr(nsg, nsg), hgm(12, 12)
    real(kind=8) :: a1(3, 3), a2(3, 3), a3(3, 3), a4(3, 3), a0(3, 3), a5(3, 3)
    real(kind=8) :: a6(3, 3)
    real(kind=8) :: aetoil, acolin, agliss, alomer, ahirth, c0, c1, c2, c3, c4
    real(kind=8) :: c5
    character(len=16) :: necris, necoul
    data nn/7,9,8,2,1,3,12,11,10,5,4,6/
!     ----------------------------------------------------------------
    idbg=0
    if (necris(1:11) .eq. 'MONO_DD_CFC') then
!
!  MATRICE D INTERACTION (12*12): 5 COEFFICIENTS DD_CFC
!  DEFINITION SELON G.MONET
        if (nbsys .ne. 12) call u2mess('F', 'COMPOR1_24')
        aetoil=coefh(1)
        acolin=coefh(2)
        agliss=coefh(3)
        alomer=coefh(4)
        ahirth=coefh(5)
!
        call r8inir(3*3, aetoil, a0, 1)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 1)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 4)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 7)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 10)
!
        a1(1,1)=acolin
        a1(1,2)=agliss
        a1(1,3)=agliss
        a1(2,1)=agliss
        a1(2,2)=ahirth
        a1(2,3)=alomer
        a1(3,1)=agliss
        a1(3,2)=alomer
        a1(3,3)=ahirth
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 1)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 4)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 7)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 10)
!
        a2(1,1)=ahirth
        a2(1,2)=agliss
        a2(1,3)=alomer
        a2(2,1)=agliss
        a2(2,2)=acolin
        a2(2,3)=agliss
        a2(3,1)=alomer
        a2(3,2)=agliss
        a2(3,3)=ahirth
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 1)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 7)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 4)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 10)
!
        a3(1,1)=ahirth
        a3(1,2)=alomer
        a3(1,3)=agliss
        a3(2,1)=alomer
        a3(2,2)=ahirth
        a3(2,3)=agliss
        a3(3,1)=agliss
        a3(3,2)=agliss
        a3(3,3)=acolin
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 4)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 7)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 1)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 10)
!
        do 10 i = 1, 12
            do 10 j = 1, 12
                hsr(nn(i),nn(j))=hgm(i,j)
10          continue
!
    else if (necris(1:10).eq.'MONO_DD_CC') then
!
!
!  MATRICE D INTERACTION (12*12): 5 COEFFICIENTS DD_CFC
!  DEFINITION SELON G.MONET
        if (nbsys .ne. 12) call u2mess('F', 'COMPOR1_24')
        c0=coefh(1)
        c1=coefh(2)
        c2=coefh(3)
        c3=coefh(4)
        c4=coefh(5)
        c5=coefh(6)
!
        call r8inir(3*3, c1, a0, 1)
        do 12 i = 1, 3
            a0(i,i)=c0
12      continue
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 1)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 4)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 7)
        call lcicma(a0, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 10)
!
        a1(1,1)=c4
        a1(1,2)=c3
        a1(1,3)=c2
        a1(2,1)=c3
        a1(2,2)=c5
        a1(2,3)=c3
        a1(3,1)=c2
        a1(3,2)=c3
        a1(3,3)=c4
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 7)
        call lcicma(a1, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 10)
!
        a2(1,1)=c4
        a2(1,2)=c2
        a2(1,3)=c3
        a2(2,1)=c2
        a2(2,2)=c4
        a2(2,3)=c3
        a2(3,1)=c3
        a2(3,2)=c3
        a2(3,3)=c5
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 1)
        call lcicma(a2, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 10)
!
        a3(1,1)=c5
        a3(1,2)=c3
        a3(1,3)=c3
        a3(2,1)=c3
        a3(2,2)=c4
        a3(2,3)=c2
        a3(3,1)=c3
        a3(3,2)=c2
        a3(3,3)=c4
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 1)
        call lcicma(a3, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 7)
!
!
        a4(1,1)=c2
        a4(1,2)=c3
        a4(1,3)=c4
        a4(2,1)=c3
        a4(2,2)=c5
        a4(2,3)=c3
        a4(3,1)=c4
        a4(3,2)=c3
        a4(3,3)=c2
        call lcicma(a4, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    1, 4)
        call lcicma(a4, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 1)
!
        a5(1,1)=c3
        a5(1,2)=c3
        a5(1,3)=c5
        a5(2,1)=c2
        a5(2,2)=c4
        a5(2,3)=c3
        a5(3,1)=c4
        a5(3,2)=c2
        a5(3,3)=c3
        call lcicma(a5, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    10, 4)
        call lcicma(a5, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 10)
!
        a6(1,1)=c3
        a6(1,2)=c2
        a6(1,3)=c4
        a6(2,1)=c3
        a6(2,2)=c4
        a6(2,3)=c2
        a6(3,1)=c5
        a6(3,2)=c3
        a6(3,3)=c3
        call lcicma(a6, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    7, 4)
        call lcicma(a6, 3, 3, 3, 3,&
                    1, 1, hgm, 12, 12,&
                    4, 7)
!
        do 11 i = 1, 12
            do 11 j = 1, 12
                hsr(i,j)=hgm(i,j)
11          continue
        if (idbg .eq. 1) then
            write(6,*) 'MATRICE D INTERACTION POUR',necris
            do 13 i = 1, 12
                write(6,'(12(1X,E11.4))') (hgm(i,j),j=1,12)
13          continue
        endif
!
    endif
!
!
end subroutine
