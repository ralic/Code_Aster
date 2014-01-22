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
    subroutine mmtgeo(phasep,ndim  ,nne   ,nnm   ,mprt1n, &
              mprt2n,mprojn,mprt11,mprt21,mprt22, &
          wpg   ,ffe   ,ffm   ,dffm  ,jacobi, &
          coefac,jeu   ,dlagrc,kappa ,vech1 , &
          vech2 ,h        ,hah  , &
          matree,matrmm,matrem, matrme)
          
        character(len=9) :: phasep
    
        integer :: ndim
        integer :: nne
        integer :: nnm
        
    real(kind=8) :: mprojn(3, 3)
        
        real(kind=8) :: wpg
        real(kind=8) :: ffe(9)
        real(kind=8) :: ffm(9)
        real(kind=8) :: dffm(2, 9)
        real(kind=8) :: jacobi
        real(kind=8) :: coefac        
        real(kind=8) :: jeu
        real(kind=8) :: dlagrc
    
        real(kind=8) :: mprt1n(3, 3)
        real(kind=8) :: mprt2n(3, 3)
        real(kind=8) :: mprt11(3, 3)
        real(kind=8) :: mprt21(3, 3)
    real(kind=8) :: mprt22(3, 3)
        
    real(kind=8) ::  kappa(2, 2)
    real(kind=8) ::  h(2,2)  
    real(kind=8) ::  hah(2,2)    
    
    real(kind=8) :: vech1(3)
    real(kind=8) :: vech2(3)
        
    real(kind=8) :: matree(27, 27)
        real(kind=8) :: matrmm(27, 27)
        real(kind=8) :: matrem(27, 27)
        real(kind=8) :: matrme(27, 27)
    end subroutine mmtgeo
end interface
