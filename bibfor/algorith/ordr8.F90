subroutine ordr8(tab, nb, iord)
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
!***********************************************************************
!    P. RICHARD     DATE //
!-----------------------------------------------------------------------
!  BUT:  TROUVER L'ORDRE CROISSANT D'UNE TABLE DE VALEUR R8
    implicit none
!     PAS DE MODIFICATION DE L'ORDRE D'ENTREE MAIS DETERMINATION DE
!     POINTEUR D'ORDRE
!
!-----------------------------------------------------------------------
!
! TAB      /I/: TABLEAU A ORDONNER
! NB       /I/: TAILLAE DU TABLEAU A ORDONNER
! IORD     /O/: TABLE DES POINTEURS D'ORDRE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: tab(nb)
    integer :: iord(nb)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iormin, itemp, j, nb
    real(kind=8) :: vmin
!-----------------------------------------------------------------------
    do 10 i = 1, nb
        iord(i)=i
10  end do
!
    do 20 i = 1, nb-1
        vmin=tab(iord(i))
        iormin=i
        do 30 j = i+1, nb
            if (tab(iord(j)) .lt. vmin) then
                vmin=tab(iord(j))
                iormin=j
            endif
30      continue
        itemp=iord(i)
        iord(i)=iord(iormin)
        iord(iormin)=itemp
20  end do
!
end subroutine
