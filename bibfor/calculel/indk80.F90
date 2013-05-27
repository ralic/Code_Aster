function indk80(lk80, k80z, rang, nbk80)
    implicit none
    integer :: indk80
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: nbk80, rang
    character(len=*) :: k80z, lk80(*)
    character(len=80) :: k80, lk80z
! ----------------------------------------------------------------------
!     ENTREES:
!     LK80 : LISTE DE K80 OU ON DOIT CHERCHER LE MOT K80
!     K80 : MOT A CHERCHER
!     NBK80: NOMBRE DE MOTS DE LK80
!     RANG : ON CHERCHE LE RANG-IEME MOT K80 DANS LA LISTE.
!
!     SORTIES:
!     INDK80 : POSITION DU MOT CHERCHE DANS LA LISTE.
!           SI LE MOT EST ABSENT: INDK80=0
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j
! DEB-------------------------------------------------------------------
!
    k80 = k80z
    j = 0
    do 10 i = 1, nbk80
        lk80z = lk80(i)
        if (lk80z .eq. k80) then
            j = j + 1
            if (j .eq. rang) goto 20
        endif
10  end do
    indk80 = 0
    goto 30
20  continue
    indk80 = i
30  continue
end function
