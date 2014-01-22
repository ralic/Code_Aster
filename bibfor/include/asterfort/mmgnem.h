!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine mmgnem(ndim  ,nnm   ,nne,mprt1n,mprt2n, &
                  wpg   , &
          ffe,dffm  ,jacobi,coefac,jeu   , &
          dlagrc,kappa ,vech1 ,vech2 ,h     , &
          matrem)
    
        integer :: ndim
        integer :: nnm, nne
        

        
        real(kind=8) :: wpg
        real(kind=8) :: ffe(9)
        real(kind=8) :: dffm(2, 9)
        real(kind=8) :: jacobi
        real(kind=8) :: coefac        
        real(kind=8) :: jeu
        real(kind=8) :: dlagrc
    
        real(kind=8) :: mprt1n(3, 3)
        real(kind=8) :: mprt2n(3, 3)

    real(kind=8) :: mprt22(3, 3)
        
    real(kind=8) ::  kappa(2, 2)
    real(kind=8) ::  h(2,2)     
    
    real(kind=8) :: vech1(3)
    real(kind=8) :: vech2(3)
        
        real(kind=8) :: matrem(27, 27) 
    end subroutine mmgnem
end interface
