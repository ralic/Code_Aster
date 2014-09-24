subroutine dp_3d(xj2d, xi1, cohe1, delta1, beta1,&
                 h1, cohemin, fdp1, cohe2, delta2,&
                 beta2, h2)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!       calcul des invariants d' un tenseur de contrainte      
!=====================================================================
    implicit none
    real(kind=8) :: xj2d
    real(kind=8) :: xi1
    real(kind=8) :: cohe1
    real(kind=8) :: delta1
    real(kind=8) :: beta1
    real(kind=8) :: h1
    real(kind=8) :: cohemin
    real(kind=8) :: fdp1
    real(kind=8) :: delta2
    real(kind=8) :: beta2
    real(kind=8) :: cohe2
    real(kind=8) :: h2, cohe3
!
!       test positivite de la cohesion residuelle        
    cohe3=cohe1-delta1*xi1/3.d0
    if (cohe3 .lt. cohemin) then
!        au dessous on passe en vonmises avec un cohesion residulle faible        
        cohe2=cohemin
        cohe3=cohe2
        delta2=0.d0
!        on impose alors un ecoulement sans dilatance
        beta2=0.d0 
!        on met alors l ecrouissage a zero s il est negatif
        if (h1 .lt. 0.d0) then
            h2=0.d0
        else
            h2=h1
        end if 
    else
        cohe2=cohe1
        delta2=delta1
        beta2=beta1
        h2=h1
    end if
    fdp1=sqrt(xj2d)-cohe3  
end subroutine
