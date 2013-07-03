function ioriv3(num, noeud, vect, coor)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
    implicit none
!
!   IORIV3  --  ORIENTATION D'UNE MAILLE PAR RAPPORT A UN VECTEUR
!
!   ARGUMENT     E/S  TYPE      ROLE
!   NUM          IN/OUT  K*     NUMEROTATION DE LA MAILLE
!
!   CODE RETOUR IORIV3 : 0 SI LA MAILLE NE CONTIENT PAS LE NOEUD
!                       -1 OU 1 SINON (SELON QU'IL AIT OU NON
!                                      FALLU REORIENTER)
#include "asterfort/u2mess.h"
    integer :: num(2), i, ioriv3, k, l, n1, n2, noeud
    real(kind=8) :: scal, x1, x2, y1, y2, z1, z2
!-----------------------------------------------------------------------
    real(kind=8) :: vect(3), coor(3, *), x, y, z
    x(i)=coor(1,i)
    y(i)=coor(2,i)
    z(i)=coor(3,i)
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!     BOUCLE SUR LES SOMMETS
    ioriv3=0
    do 10 i = 1, 2
        if (num(i) .eq. noeud) then
            n1=num(1)
            n2=num(2)
            x1=x(n1)
            y1=y(n1)
            z1=z(n1)
            x2=x(n2)
            y2=y(n2)
            z2=z(n2)
            scal=(x2-x1)*vect(1)+(y2-y1)*vect(2)+(z2-z1)*vect(3)
            if (scal .gt. 0) then
                ioriv3= 1
            else if (scal.lt.0) then
                ioriv3=-1
            else
                call u2mess('F', 'MODELISA4_83')
            endif
        endif
10  end do
    if (ioriv3 .lt. 0) then
!       ON PERMUTE LES SOMMETS
        k=num(1)
        l=num(2)
        num(1)=l
        num(2)=k
    endif
!
end function
