function iorim2(num1, n1, num2, n2, reorie)
    implicit none
#include "asterf_types.h"
    integer :: iorim2, n1, n2, num1(n1), num2(n2)
    aster_logical :: reorie
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
!     IORIM2  --  ORIENTATION D'UNE MAILLE PAR RAPPORT A UNE VOISINE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NUM1            IN    K*     NUMEROTATION DE LA MAILLE 1
!    NUM2          IN/OUT  K*     NUMEROTATION DE LA MAILLE 2
!    N1              IN    K*     NOMBRE DE NOEUDS DE LA MAILLE 1
!    N2              IN    K*     NOMBRE DE NOEUDS DE LA MAILLE 2
!
!   CODE RETOUR IORIM2 : 0 SI LES MAILLES NE SONT PAS CONTIGUES
!                       -1 OU 1 SINON (SELON QU'IL AIT OU NON
!                                      FALLU REORIENTER)
!
!     DONNEES POUR TRIA3,TRIA6,TRIA7,QUAD4,QUAD8,QUAD9
!     NOMBRE DE SOMMETS EN FONCTION DU NOMBRE DE NOEUDS DE L'ELEMENT
    integer :: nso(9), nso1, nso2, i1, j1, i2, j2, i, k, l
    data nso /0,0,3,4,0,3,3,4,4/
!
#define egal(i1,j1,i2,j2) (num1(i1).eq.num2(i2)).and. \
    (num1(j1).eq.num2(j2))
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    nso1 = nso(n1)
    nso2 = nso(n2)
!     BOUCLES SUR LES ARETES
    do 10 i1 = 1, nso1
        j1 = i1 + 1
        if (j1 .gt. nso1) j1 = 1
        do 10 i2 = 1, nso2
            j2 = i2 + 1
            if (j2 .gt. nso2) j2 = 1
            if (egal(i1,j1,i2,j2)) then
                iorim2 = -1
                goto 100
            endif
            if (egal(i1,j1,j2,i2)) then
                iorim2 = 1
                goto 100
            endif
 10     continue
    iorim2 = 0
100 continue
!
! --- ON PERMUTE LES SOMMETS
    if (reorie .and. iorim2 .lt. 0) then
        k = num2(2)
        l = num2(nso2)
        num2(2) = l
        num2(nso2) = k
!        ON PERMUTE LES NOEUDS INTERMEDIAIRES
        if (n2 .ne. nso2) then
            do 200 i = 1, nso2/2
                k = num2(nso2+i)
                l = num2(2*nso2+1-i)
                num2(nso2+i) = l
                num2(2*nso2+1-i) = k
200         continue
        endif
    endif
!
end function
