subroutine dhrc_jacob(eps, vint, b, c, bp1,&
                      cp1, bp2, cp2, as1, bs1,&
                      cs1, as2, bs2, cs2, indi,&
                      neta1, neta2, cstseu, jacob)
!
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
!
#include "asterfort/dhrc_calc_n.h"
#include "asterfort/matini.h"
    integer :: indi(6)
    real(kind=8) :: vint(7), eps(8)
    real(kind=8) :: b(6, 2, 2), c(2, 2, 2)
    real(kind=8) :: bp1(6, 2), cp1(2, 2), as1(6, 6), bs1(6, 2), cs1(2, 2)
    real(kind=8) :: bp2(6, 2), cp2(2, 2), as2(6, 6), bs2(6, 2), cs2(2, 2)
    real(kind=8) :: neta1(2), neta2(2), cstseu(2)
!
    real(kind=8) :: jacob(6, 6), jacobt(6, 6)
!
! ----------------------------------------------------------------------
!
!      CALCUL DE LA MATRICE JACOBIENNE DES SEUILS
!      APPELE PAR "LCGLCC"
!
! IN:
!       EPS     : TENSEUR DE DEFORMATIONS
!       B       : TENSEUR ASSOCIE AUX DEFORMATIONS PLASTIQUES
!       C       : TENSEUR DE RAIDEUR D'ÉCROUISSAGE PLASTIQUE
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!
! OUT:
!       JACOB   : MATRICE JACOBIENNE DES SEUILS ACTIVÉS
!       JACOBT   : MATRICE JACOBIENNE DES SEUILS (TOUS)
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l
!
! ----------------------------------------------------------------------
! --  CALCUL DE LA JACOBIENNE TOTALE JACOBT
! ----------------------------------------------------------------------
!
!
    call dhrc_calc_n(eps, vint, b, c, neta1,&
                     neta2)
!
    call matini(6, 6, 0.0d0, jacobt)
!
    do k = 1, 6
        do i = 1, 6
            jacobt(1,1)=jacobt(1,1)- eps(k)* as1(k,i)* eps(i)*0.5d0
            jacobt(2,2)=jacobt(2,2)- eps(k)* as2(k,i)* eps(i)*0.5d0
!
            if (i .lt. 3) then
                jacobt(1,1)=jacobt(1,1)- eps(k)* bs1(k,i)* vint(i+2)*0.5d0
                jacobt(2,2)=jacobt(2,2)- eps(k)* bs2(k,i)* vint(i+4)*0.5d0
                if (k .lt. 3) then
                    jacobt(1,1)=jacobt(1,1)- vint(k+2)* cs1(k,i)*vint(i+2)*0.5d0
                    jacobt(2,2)=jacobt(2,2)- vint(k+4)* cs2(k,i)*vint(i+4)*0.5d0
                endif
            endif
!
        end do
!
        jacobt(1,3)=jacobt(1,3)- eps(k)*bp1(k,1)*0.5d0
        jacobt(1,4)=jacobt(1,4)- eps(k)*bp1(k,2)*0.5d0
!
        jacobt(2,5)=jacobt(2,5)- eps(k)*bp2(k,1)*0.5d0
        jacobt(2,6)=jacobt(2,6)- eps(k)*bp2(k,2)*0.5d0
!
        jacobt(3,1)=jacobt(3,1)- eps(k)*bp1(k,1)*0.5d0
!
        jacobt(4,1)=jacobt(4,1)- eps(k)*bp1(k,2)*0.5d0
!
        jacobt(5,2)=jacobt(5,2)- eps(k)*bp2(k,1)*0.5d0
!
        jacobt(6,2)=jacobt(6,2)- eps(k)*bp2(k,2)*0.5d0
!
        if (k .lt. 3) then
            jacobt(1,3)=jacobt(1,3)- vint(k+2)*cp1(k,1)
            jacobt(1,4)=jacobt(1,4)- vint(k+2)*cp1(k,2)
!
            jacobt(2,5)=jacobt(2,5)- vint(k+4)*cp2(k,1)
            jacobt(2,6)=jacobt(2,6)- vint(k+4)*cp2(k,2)
!
            jacobt(3,1)=jacobt(3,1)- vint(k+2)*cp1(k,1)
!
            jacobt(4,1)=jacobt(4,1)- vint(k+2)*cp1(k,2)
!
            jacobt(5,2)=jacobt(5,2)- vint(k+4)*cp2(k,1)
!
            jacobt(6,2)=jacobt(6,2)- vint(k+4)*cp2(k,2)
        endif
!
    end do
!
    jacobt(1,2)=0.0d0
    jacobt(1,5)=0.0d0
    jacobt(1,6)=0.0d0
!
    jacobt(2,1)=0.0d0
    jacobt(2,3)=0.0d0
    jacobt(2,4)=0.0d0
!
    jacobt(3,2)=0.0d0
    jacobt(3,3)=jacobt(3,3)- c(1,1,1)
    jacobt(3,4)=jacobt(3,4)- c(2,1,1)
    jacobt(3,5)=0.0d0
    jacobt(3,6)=0.0d0
!
    jacobt(4,2)=0.0d0
    jacobt(4,3)=jacobt(4,3)- c(1,2,1)
    jacobt(4,4)=jacobt(4,4)- c(2,2,1)
    jacobt(4,5)=0.0d0
    jacobt(4,6)=0.0d0
!
    jacobt(5,1)=0.0d0
    jacobt(5,3)=0.0d0
    jacobt(5,4)=0.0d0
    jacobt(5,5)=jacobt(5,5)- c(1,1,2)
    jacobt(5,6)=jacobt(5,6)- c(2,1,2)
!
    jacobt(6,1)=0.0d0
    jacobt(6,3)=0.0d0
    jacobt(6,4)=0.0d0
    jacobt(6,5)=jacobt(6,5)- c(1,2,2)
    jacobt(6,6)=jacobt(6,6)- c(2,2,2)
!
!
    do k = 1, 6
        jacobt(1,k)=jacobt(1,k)/cstseu(1)
        jacobt(2,k)=jacobt(2,k)/cstseu(1)
        jacobt(3,k)=jacobt(3,k)*2.0d0*neta1(1)/(cstseu(2)**2.0d0)
        jacobt(4,k)=jacobt(4,k)*2.0d0*neta1(2)/(cstseu(2)**2.0d0)
        jacobt(5,k)=jacobt(5,k)*2.0d0*neta2(1)/(cstseu(2)**2.0d0)
        jacobt(6,k)=jacobt(6,k)*2.0d0*neta2(2)/(cstseu(2)**2.0d0)
    end do
!
! ----------------------------------------------------------------------
! --  CALCUL DE LA JACOBIENNE ACTIVEE JACOB
! ----------------------------------------------------------------------
!
    l=0
    do k = 1, 6
        if (k .eq. indi(k)) then
            l=l+1
            j=0
            do i = 1, 6
                if (i .eq. indi(i)) then
                    j=j+1
                    jacob(l,j)=jacobt(k,i)
                endif
            end do
        endif
    end do
!
end subroutine
