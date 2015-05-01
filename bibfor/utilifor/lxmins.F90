subroutine lxmins(chaine)
! aslint: disable=
    implicit none
    character(len=*) :: chaine
!
!     ------------------------------------------------------------------
!      TRANSFORME LES MAJUSCULES EN MINUSCULES
!     ------------------------------------------------------------------
! VAR CHAINE CHAINE A TRANSFORMER
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         -
!     ROUTINE(S) FORTRAN     :
!         CHAR    ICHAR   LEN
!     ------------------------------------------------------------------
!
    integer :: mxchar
!-----------------------------------------------------------------------
    integer :: i, ilong
!-----------------------------------------------------------------------
    parameter ( mxchar=255 )
    character(len=1) :: class(0:mxchar)
    character(len=26) :: minus, major
!
    integer :: long, first
    save         class, first
!     ------------------------------------------------------------------
    data first/0/
    data minus/'abcdefghijklmnopqrstuvwxyz'/
    data major/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
!     ------------------------------------------------------------------
!
    if (first .eq. 0) then
!
!        INITIALISATION DES TABLES DE CONVERSION
!
        first = 1
        do 10 i = 0, mxchar
            class(i) = char(i)
10      continue
!
        do 20 i = 1, 26
            class(ichar(major(i:i))) = class(ichar(minus(i:i)))
20      continue
    endif
!
    long = len(chaine)
    do 100 ilong = 1, long
        chaine(ilong:ilong) = class(ichar(chaine(ilong:ilong)))
100  end do
!     ------------------------------------------------------------------
end subroutine
