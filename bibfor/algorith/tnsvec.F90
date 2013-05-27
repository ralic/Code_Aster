subroutine tnsvec(choix, ndim, mat, vec, r)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/assert.h'
    integer :: ndim, i, choix
    real(kind=8) :: mat(3, 3), vec(2*ndim), r
!     FONCTION TRANSFORMANT UNE MATRICE SYMETRIQUE (TENSEUR) 3,3
!     EN VECTEUR 6 (OU 4 EN 2D) ET MULTIPLIANT LES TERMES NON DIAGONAUX
!     PAR UN REEL R ET INVERSEMENT.
!     IN : CHOIX =3 OU 6 (DIMENSION DE L'OBJET EN ENTREE)
!     Si CHOIX=3
!        IN : MAT
!        OUT : VEC
!     SI CHOIX=6
!        IN : VEC
!        OUT : MAT
!
    if (choix .eq. 3) then
!
!        TRANSFORMATION MATRICE EN VECTEUR
        do 300 i = 1, 3
            vec(i)=mat(i,i)
300      continue
        vec(4)=mat(1,2)*r
        if (ndim .eq. 3) then
            vec(5)=mat(1,3)*r
            vec(6)=mat(2,3)*r
        endif
!
!
    else if (choix.eq.6) then
!
!        TRANSFORMATION VECTEUR EN MATRICE
        do 600 i = 1, 3
            mat(i,i)=vec(i)
600      continue
        mat(1,2)=vec(4)*r
        mat(2,1)=vec(4)*r
        if (ndim .eq. 2) then
            mat(1,3)=0.d0
            mat(2,3)=0.d0
            mat(3,1)=0.d0
            mat(3,2)=0.d0
        else
            mat(1,3)=vec(5)*r
            mat(3,1)=vec(5)*r
            mat(2,3)=vec(6)*r
            mat(3,2)=vec(6)*r
        endif
    else
        call assert(choix.eq.3)
    endif
!
end subroutine
