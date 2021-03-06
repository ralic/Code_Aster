function ioriv1(num, noeud, vect, coor)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
    implicit none
#include "asterfort/utmess.h"
!
!     IORIV1  --  ORIENTATION D'UNE MAILLE PAR RAPPORT A UN VECTEUR
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NUM          IN/OUT  K*     NUMEROTATION DE LA MAILLE
!
!   CODE RETOUR IORIV1 : 0 SI LA MAILLE NE CONTIENT PAS LE NOEUD
!                       -1 OU 1 SINON (SELON QU'IL AIT OU NON
!                                      FALLU REORIENTER)
    integer :: num(2)
    real(kind=8) :: vect(2), coor(3, *)
    integer :: i, ioriv1, k, l, n1, n2, noeud
    real(kind=8) :: scal, x1, x2, xn, y1, y2, yn
#define x(i) coor(1,i)
#define y(i) coor(2,i)
!
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!     BOUCLE SUR LES SOMMETS
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    ioriv1=0
    do 10 i = 1, 2
        if (num(i) .eq. noeud) then
            n1=num(1)
            n2=num(2)
            x1=x(n1)
            y1=y(n1)
            x2=x(n2)
            y2=y(n2)
!     VECTEUR NORMAL AU SEGMENT
            xn=y2-y1
            yn=x1-x2
            scal=xn*vect(1)+yn*vect(2)
            if (scal .gt. 0) then
                ioriv1= 1
            else if (scal.lt.0) then
                ioriv1=-1
            else
                call utmess('F', 'MODELISA4_76')
            endif
        endif
10  end do
    if (ioriv1 .lt. 0) then
!       ON PERMUTE LES SOMMETS
        k=num(1)
        l=num(2)
        num(1)=l
        num(2)=k
    endif
!
end function
