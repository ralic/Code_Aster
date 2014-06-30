function meiden(scal, ncmp, i1, i3, nec,&
                i2, i4)
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
    implicit none
    logical(kind=1) :: meiden
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
    character(len=4) :: scal
    integer :: ncmp, i1, i3, nec, i2, i4
! ----------------------------------------------------------------------
!     ENTREES:
!        SCAL : R, I , C, K8, K16, K24
!        NCMP : NOMBRE DE COMPOSANTES DES GRANDEURS
!          I1 : ADRESSE DANS ZR OU ZI ... DU DEBUT DE LA 1ERE GRANDEUR
!          I3 : ADRESSE DANS ZR OU ZI ... DU DEBUT DE LA 2EME GRANDEUR
!        NEC  : NOMBRE D'ENTIERS CODES
!          I2 : ADRESSE DANS ZI DU DEBUT DU DG DE LA 1ERE GRANDEUR
!          I4 : ADRESSE DANS ZI DU DEBUT DU DG DE LA 2EME GRANDEUR
!
!     SORTIES:
!     MEIDEN : VRAI SI LES 2 GRANDEURS SONT IDENTIQUES.
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iec
!-----------------------------------------------------------------------
    meiden = .false.
!
!     -- ON TESTE D'ABORD L'EGALITE DES DESCIPTEUR GRANDEUR:
    do 1,iec = 1,nec
    if (zi(i2+iec) .ne. zi(i4+iec)) goto 9999
    1 end do
!
!     -- ON TESTE ENSUITE LES VALEURS:
    if (scal(1:1) .eq. 'I') then
        do 2,i = 1,ncmp
        if (zi(i1+i) .ne. zi(i3+i)) goto 9999
 2      continue
    else if (scal(1:1).eq.'R') then
        do 3,i = 1,ncmp
        if (zr(i1+i) .ne. zr(i3+i)) goto 9999
 3      continue
    else if (scal(1:1).eq.'C') then
        do 4,i = 1,ncmp
        if (zc(i1+i) .ne. zc(i3+i)) goto 9999
 4      continue
    else if (scal(1:3).eq.'K8 ') then
        do 5,i = 1,ncmp
        if (zk8(i1+i) .ne. zk8(i3+i)) goto 9999
 5      continue
    else if (scal(1:3).eq.'K16') then
        do 6,i = 1,ncmp
        if (zk16(i1+i) .ne. zk16(i3+i)) goto 9999
 6      continue
    else if (scal(1:3).eq.'K24') then
        do 7,i = 1,ncmp
        if (zk24(i1+i) .ne. zk24(i3+i)) goto 9999
 7      continue
    else
        ASSERT(.false.)
    endif
    meiden = .true.
9999  continue
end function
