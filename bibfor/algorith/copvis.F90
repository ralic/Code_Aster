subroutine copvis(nb, ivec1, ivec2)
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
!
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  COPIER UN VECTEUR D'ENTIER DANS UN AUTRE EN LE MULTIPLIANT
!   PAR UN FACTEUR
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! NB       /I/: NOMBRE D'ENTIER A COPIER
! IVEC1    /I/: VECTEUR A COPIER
! IVEC2    /O/: VECTEUR RESULTAT
!
!-----------------------------------------------------------------------
!
!      CHARACTER*1
!      CHARACTER*8
!      CHARACTER*9
!      CHARACTER*16
!      CHARACTER*24
!      REAL*8
    integer :: ivec1(nb), ivec2(nb)
    integer :: i, nb
!-----------------------------------------------------------------------
!
    if (nb .eq. 0) goto 9999
!
    do 10 i = 1, nb
        ivec2(i)=ivec1(i)
10  end do
!
!
9999  continue
end subroutine
