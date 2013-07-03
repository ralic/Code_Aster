subroutine dil2gr(imate, compor, ndim, regula, dimdef,&
                  defgep, sigp, dsde2g)
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
! --- BUT : CALCUL DE LA LOI DE COMPORTEMENT ELASTIQUE POUR LA PARTIE --
! ---       SECOND GRADIENT --------------------------------------------
! ======================================================================
    implicit      none
#include "asterfort/rcvalb.h"
#include "asterfort/u2mesk.h"
    integer :: imate, ndim, dimdef, regula(6)
    real(kind=8) :: sigp(ndim), dsde2g(ndim, ndim), defgep(dimdef)
    character(len=16) :: compor(*)
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, adder2
    real(kind=8) :: val(5)
    integer :: icodre(5), kpg, spt
    character(len=8) :: ncra(5), fami, poum
! ======================================================================
! --- DEFINITION DES DONNEES INITIALES ---------------------------------
! ======================================================================
    data ncra  / 'A1','A2','A3','A4','A5' /
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    if (compor(1) .eq. 'ELAS') then
        do 10 i = 1, ndim
            do 20 j = 1, ndim
                dsde2g(j,i)=0.0d0
20          continue
10      continue
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', 'ELAS_2NDG', 0, ' ', 0.0d0,&
                    1, ncra(1), val(1), icodre(1), 1)
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', 'ELAS_2NDG', 0, ' ', 0.0d0,&
                    1, ncra(3), val(3), icodre(3), 1)
        do 30 i = 1, ndim
            dsde2g(i,i)=(1+ndim)*(val(1)-val(3))
30      continue
!
        adder2 = regula(2)
        do 40 i = 1, ndim
            sigp(i)=0.0d0
            do 50 j = 1, ndim
                sigp(i)=sigp(i)+dsde2g(i,j)*defgep(adder2-1+j)
50          continue
40      continue
    else
        call u2mesk('F', 'ALGORITH4_50', 1, compor(1))
    endif
! ======================================================================
end subroutine
