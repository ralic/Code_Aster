subroutine rcvale_wrap (nommaz, phenom, nbpar, nompar, valpar,&
                        nbres, nomres, valres, icodre, iarret)
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
#include "asterfort/rcvale.h"   
!  
    integer, intent(in) :: nbpar, nbres
    character(len=*), intent(in) :: phenom
    integer, intent(in) :: iarret
    character(len=*), intent(in) :: nommaz
    integer, intent(out) :: icodre(nbres)
    character(len=8), intent(in) :: nompar(nbpar)
    character(len=16), intent(in) :: nomres(nbres)
    real(kind=8), intent(in) :: valpar(nbpar)
    real(kind=8), intent(out) :: valres(nbres)
! ----------------------------------------------------------------------
!     WRAPPER FOR rcvale CALLED IN  C from python
!     CHANGE nomres ARRAY WHITH FIXED LENGTH
!

    call rcvale(nommaz, phenom, nbpar, nompar, valpar, nbres, nomres, valres, icodre, iarret)
!
end subroutine
