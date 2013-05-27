function indiis(lis, is, rang, nbis)
    implicit none
    integer :: indiis
!
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
!     ARGUMENTS:
!     ----------
    integer :: nbis, rang
    integer :: is, lis(*)
! ----------------------------------------------------------------------
!     ENTREES:
!     LIS : LISTE DE IS OU ON DOIT CHERCHER L'ENTIER IS
!     IS  : ENTIER A CHERCHER
!     NBIS: NOMBRE D'ENTIERS DE LA LISTE
!     RANG: ON CHERCHE LE RANG-IEME ENTIER IS DANS LA LISTE.
!
!     SORTIES:
!     INDIIS : POSITION DE L'ENTIER DANS LA LISTE
!           SI L'ENTIER EST ABSENT: INDIIS=0
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    j = 0
    do 100 i = 1, nbis
        if (lis(i) .eq. is) then
            j = j + 1
            if (j .eq. rang) goto 110
        endif
100  end do
    indiis = 0
    goto 120
110  continue
    indiis = i
120  continue
end function
