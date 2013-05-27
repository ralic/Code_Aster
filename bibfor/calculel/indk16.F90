function indk16(lk16, k16z, rang, nbk16)
    implicit none
    integer :: indk16
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
    integer :: nbk16, rang
    character(len=*) :: k16z, lk16(*)
    character(len=16) :: k16, lk16z
! ----------------------------------------------------------------------
!     ENTREES:
!     LK16 : LISTE DE K16 OU ON DOIT CHERCHER LE MOT K16
!     K16 : MOT A CHERCHER
!     NBK16: NOMBRE DE MOTS DE LK16
!     RANG : ON CHERCHE LE RANG-IEME MOT K16 DANS LA LISTE.
!
!     SORTIES:
!     INDK16 : POSITION DU MOT CHERCHE DANS LA LISTE.
!           SI LE MOT EST ABSENT: INDK16=0
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j
! DEB-------------------------------------------------------------------
!
    k16 = k16z
    j = 0
    do 100 i = 1, nbk16
        lk16z = lk16(i)
        if (lk16z .eq. k16) then
            j = j + 1
            if (j .eq. rang) goto 110
        endif
100  end do
    indk16 = 0
    goto 120
110  continue
    indk16 = i
120  continue
end function
