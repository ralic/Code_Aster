subroutine utremt(mot, liste, nbval, place)
    implicit none
    character(len=*) :: mot, liste(*)
    integer :: nbval, place
!
!     ------------------------------------------------------------------
!     RECHERCHE UN MOT DONNE DANS UN LISTE (TABLEAU) DE MOTS
!     ------------------------------------------------------------------
! IN  MOT      : CH*(*) : LE MOT CHERCHE DANS LA LISTE
! IN  LISTE(*) : CH*(*) : LISTE DE MOT PROPOSE
! IN  NBVAL    : IS     : NOMBRE DE MOT DANS LA LISTE
! OUT PLACE    : IS     : PLACE DU MOT DANS LA LISTE
!                         = 0  SI LE MOT EST ABSENT DE LA LISTE
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    place = 0
    do 10 i = 1, nbval
        if (mot .eq. liste(i)) then
            place = i
            goto 9999
        endif
10  end do
9999  continue
end subroutine
