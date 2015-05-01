function indk32(lk32, k32z, rang, nbk32)
    implicit none
    integer :: indk32
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! INSPI INDIK24
!     ARGUMENTS:
!     ----------
    integer :: nbk32, rang
    character(len=*) :: k32z, lk32(*)
    character(len=32) :: k32, lk32z
! ----------------------------------------------------------------------
!     ENTREES:
!     LK32 : LISTE DE K32 OU ON DOIT CHERCHER LE MOT K32
!     K32 : MOT A CHERCHER
!     NBK32: NOMBRE DE MOTS DE LK32
!     RANG : ON CHERCHE LE RANG-IEME MOT K32 DANS LA LISTE.
!
!     SORTIES:
!     INDK32 : POSITION DU MOT CHERCHE DANS LA LISTE.
!           SI LE MOT EST ABSENT: INDK32=0
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j
! DEB-------------------------------------------------------------------
!
    k32 = k32z
    j = 0
    do 10 i = 1, nbk32
        lk32z = lk32(i)
        if (lk32z .eq. k32) then
            j = j + 1
            if (j .eq. rang) goto 20
        endif
10  end do
    indk32 = 0
    goto 30
20  continue
    indk32 = i
30  continue
end function
