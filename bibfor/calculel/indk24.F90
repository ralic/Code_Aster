function indk24(lk24, k24z, rang, nbk24)
    implicit none
    integer :: indk24
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! INSPI INDIK8
!     ARGUMENTS:
!     ----------
    integer :: nbk24, rang
    character(len=*) :: k24z, lk24(*)
    character(len=24) :: k24, lk24z
! ----------------------------------------------------------------------
!     ENTREES:
!     LK24 : LISTE DE K24 OU ON DOIT CHERCHER LE MOT K24
!     K24 : MOT A CHERCHER
!     NBK24: NOMBRE DE MOTS DE LK24
!     RANG : ON CHERCHE LE RANG-IEME MOT K24 DANS LA LISTE.
!
!     SORTIES:
!     INDK24 : POSITION DU MOT CHERCHE DANS LA LISTE.
!           SI LE MOT EST ABSENT: INDK24=0
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j
! DEB-------------------------------------------------------------------
!
    k24 = k24z
    j = 0
    do 100 i = 1, nbk24
        lk24z = lk24(i)
        if (lk24z .eq. k24) then
            j = j + 1
            if (j .eq. rang) goto 110
        endif
100  end do
    indk24 = 0
    goto 120
110  continue
    indk24 = i
120  continue
end function
