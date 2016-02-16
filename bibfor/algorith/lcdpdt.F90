function lcdpdt(a,b)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/assert.h"

    real(kind=8),intent(in) :: a(:),b(:)
    real(kind=8)            :: lcdpdt(size(a),size(a))
! --------------------------------------------------------------------------------------------------
!   Calcule l'application lineaire (i.e. la matrice)  X --> A*X*B + B*X*A en notations symétriques
! --------------------------------------------------------------------------------------------------
!   in a: tenseur A (1:4 ou 1:6)
!   in b: tenseur B (1:4 ou 1:6)
!   ret : tenseur resultat d'ordre 4 (1:4,1:4) ou (1:6,1:6)
! --------------------------------------------------------------------------------------------------
    real(kind=8), parameter:: unsrac = 1/sqrt(2.d0)
! --------------------------------------------------------------------------------------------------

    ASSERT(size(a).eq.size(b))
    ASSERT(size(a).eq.4 .or. size(a).eq.6)


    if (size(a).eq.6) then

        lcdpdt(1,1) = a(1)*b(1)*2
        lcdpdt(1,2) = a(4)*b(4)
        lcdpdt(1,3) = a(5)*b(5)

        lcdpdt(2,1) = a(4)*b(4)
        lcdpdt(2,2) = a(2)*b(2)*2
        lcdpdt(2,3) = a(6)*b(6)

        lcdpdt(3,1) = a(5)*b(5)
        lcdpdt(3,2) = a(6)*b(6)
        lcdpdt(3,3) = a(3)*b(3)*2

        lcdpdt(1,4) =  a(1)*b(4) + a(4)*b(1)
        lcdpdt(1,5) =  a(1)*b(5) + a(5)*b(1)
        lcdpdt(1,6) = (a(4)*b(5) + a(5)*b(4))*unsrac

        lcdpdt(2,4) =  a(2)*b(4) + a(4)*b(2)  
        lcdpdt(2,5) = (a(4)*b(6) + a(6)*b(4))*unsrac
        lcdpdt(2,6) =  a(2)*b(6) + a(6)*b(2)

        lcdpdt(3,4) = (a(5)*b(6) + a(6)*b(5))*unsrac
        lcdpdt(3,5) =  a(5)*b(3) + a(3)*b(5)
        lcdpdt(3,6) =  a(6)*b(3) + a(3)*b(6)

        lcdpdt(4,1) =  a(1)*b(4) + a(4)*b(1)
        lcdpdt(4,2) =  a(4)*b(2) + a(2)*b(4)
        lcdpdt(4,3) = (a(5)*b(6) + a(6)*b(5))*unsrac

        lcdpdt(5,1) =  a(1)*b(5) + a(5)*b(1)
        lcdpdt(5,2) = (a(4)*b(6) + a(6)*b(4))*unsrac
        lcdpdt(5,3) =  a(5)*b(3) + a(3)*b(5)

        lcdpdt(6,1) = (a(4)*b(5) + a(5)*b(4))*unsrac
        lcdpdt(6,2) =  a(6)*b(2) + a(2)*b(6)
        lcdpdt(6,3) =  a(6)*b(3) + a(3)*b(6)

        lcdpdt(4,4) = a(1)*b(2) + a(4)*b(4) + a(2)*b(1)
        lcdpdt(4,5) = (a(1)*b(6) + a(6)*b(1))*unsrac + (a(5)*b(4) + a(4)*b(5))*0.5d0
        lcdpdt(4,6) = (a(5)*b(2) + a(2)*b(5))*unsrac + (a(4)*b(6) + a(6)*b(4))*0.5d0 

        lcdpdt(5,4) = (a(1)*b(6) + a(6)*b(1))*unsrac + (a(4)*b(5)+a(5)*b(4))*0.5d0
        lcdpdt(5,5) = a(1)*b(3) + a(3)*b(1) + a(5)*b(5)
        lcdpdt(5,6) = (a(4)*b(3) + a(3)*b(4))*unsrac + (a(5)*b(6) + a(6)*b(5))*0.5d0 

        lcdpdt(6,4) = (a(2)*b(5) + a(5)*b(2))*unsrac + (a(4)*b(6)+a(6)*b(4))*0.5d0
        lcdpdt(6,5) = (a(3)*b(4) + a(4)*b(3))*unsrac + (a(6)*b(5) + a(5)*b(6))*0.5d0
        lcdpdt(6,6) = a(2)*b(3) + a(3)*b(2) + a(6)*b(6) 


    else if (size(a).eq.4) then

        lcdpdt(1,1) = a(1)*b(1)*2
        lcdpdt(1,2) = a(4)*b(4)
        lcdpdt(1,3) = 0

        lcdpdt(2,1) = a(4)*b(4)
        lcdpdt(2,2) = a(2)*b(2)*2
        lcdpdt(2,3) = 0

        lcdpdt(3,1) = 0
        lcdpdt(3,2) = 0
        lcdpdt(3,3) = a(3)*b(3)*2

        lcdpdt(1,4) =  a(1)*b(4) + a(4)*b(1)
        lcdpdt(2,4) =  a(2)*b(4) + a(4)*b(2)  
        lcdpdt(3,4) =  0

        lcdpdt(4,1) =  a(1)*b(4) + a(4)*b(1)
        lcdpdt(4,2) =  a(4)*b(2) + a(2)*b(4)
        lcdpdt(4,3) =  0

        lcdpdt(4,4) = a(1)*b(2) + a(4)*b(4) + a(2)*b(1)

    end if

end function lcdpdt
