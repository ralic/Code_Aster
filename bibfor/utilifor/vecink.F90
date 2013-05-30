subroutine vecink(n, s, x)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----------------------------------------------------------------
!     INITIALISATION DU VECTEUR DE CHAR   X = S
!     IN  S      :  CHAINE DE CARACTERES POUR INITIALISER
!     IN  N      :  DIMENSION DE X
!     OUT X      :  VECTEUR DE CHAR RESULTAT
!     POUR TOUS LES TYPES DE DONNEES VOIR AUSSI VECINI, VECINT, VECINK
!     ET VECINC.
!     ----------------------------------------------------------------
    integer :: n, i
    character(len=*) :: x(n), s
    do 1 i = 1, n
        x(i)=s
 1  end do
end subroutine
