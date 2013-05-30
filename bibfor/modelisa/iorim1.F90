function iorim1(num1, num2, reorie)
    implicit none
    integer :: iorim1, num1(2), num2(2)
    logical :: reorie
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
!     IORIM1  --  ORIENTATION D'UNE MAILLE PAR RAPPORT A UNE VOISINE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NUM1            IN    K*     NUMEROTATION DE LA MAILLE 1
!    NUM2          IN/OUT  K*     NUMEROTATION DE LA MAILLE 2
!
!   CODE RETOUR IORIM1 : 0 SI LES MAILLES NE SONT PAS CONTIGUES
!                       -1 OU 1 SINON (SELON QU'IL AIT OU NON
!                                      FALLU REORIENTER)
!
    integer :: i1, j1, k, l
    logical :: egal
    egal(i1,j1) = num1(i1).eq.num2(j1)
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- BOUCLES SUR LES SOMMETS
    do 10 i1 = 1, 2
        j1 = 3-i1
        if (egal(i1,i1)) then
            iorim1 = -1
            goto 100
        endif
        if (egal(i1,j1)) then
            iorim1 = 1
            goto 100
        endif
10  end do
    iorim1 = 0
100  continue
!
! --- ON PERMUTE LES SOMMETS
    if (reorie .and. iorim1 .lt. 0) then
        k = num2(1)
        l = num2(2)
        num2(1) = l
        num2(2) = k
    endif
!
end function
