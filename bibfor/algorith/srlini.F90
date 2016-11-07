subroutine srlini(sigf,nr,yd,dy)

!
! ===================================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ===================================================================================

!!!
!!! MODELE LKR : ROUTINE INITIALISANT DY
!!!

! ===================================================================================
! IN  : SIGF(6)    : PREDICTION ELASTIQUE DES CONTRAINTES (LCELAS)
!     : NR = NDT+3 : DIMENSION VECTEUR INCONNUES
!     : YD(NDT+3)  : VALEUR DES INCONNUES A T
! OUT : DY(NDT+3)  : SOLUTION ESSAI  = ( DSIG DLAMBDA DXIP DXIVP )
! ===================================================================================
    
    implicit   none
    
    !!!
    !!! Variables globales
    !!!
    
    integer :: nr
    real(kind=8) :: sigf(6),yd(nr),dy(nr)
    
    !!!
    !!! Variables locales
    !!!
    
    integer :: ndi,ndt,i
    common /tdim/ ndt,ndi
    
    !!!
    !!! Initialisation de dy
    !!!
    
    do i=1,ndt
        dy(i)=0.d0
    end do
    
    do i=1,ndt
        dy(i)=sigf(i)-yd(i)
    end do

end subroutine
