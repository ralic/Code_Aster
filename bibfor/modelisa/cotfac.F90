subroutine cotfac(xyz, n1, n2, n3, n4,&
                  xpt, iret)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION :
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: n1, n2, n3, n4, iret
    real(kind=8) :: xyz(3, *), xpt(*)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: v12(3), v23(3), v24(3), v25(3), vno(3)
    real(kind=8) :: ra, rb, rr
    real(kind=8) :: eps
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    eps=1.d-5
    v12(1)=xyz(1,n2)-xyz(1,n1)
    v12(2)=xyz(2,n2)-xyz(2,n1)
    v12(3)=xyz(3,n2)-xyz(3,n1)
    v23(1)=xyz(1,n3)-xyz(1,n2)
    v23(2)=xyz(2,n3)-xyz(2,n2)
    v23(3)=xyz(3,n3)-xyz(3,n2)
    vno(1) = v23(2) * v12(3) - v23(3) * v12(2)
    vno(2) = v23(3) * v12(1) - v23(1) * v12(3)
    vno(3) = v23(1) * v12(2) - v23(2) * v12(1)
!
    v24(1)=xyz(1,n4)-xyz(1,n2)
    v24(2)=xyz(2,n4)-xyz(2,n2)
    v24(3)=xyz(3,n4)-xyz(3,n2)
!
    v25(1)=xpt(1)-xyz(1,n2)
    v25(2)=xpt(2)-xyz(2,n2)
    v25(3)=xpt(3)-xyz(3,n2)
!
    ra=vno(1)*v24(1)+vno(2)*v24(2)+vno(3)*v24(3)
    rb=vno(1)*v25(1)+vno(2)*v25(2)+vno(3)*v25(3)
    rr=rb/ra
!
    if (rr .gt. eps) then
        iret=1
    else if (rr.gt.-1.d0*eps) then
        iret=0
    else
        iret=-1
    endif
!
end subroutine
