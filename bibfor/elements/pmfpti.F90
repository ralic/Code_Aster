subroutine pmfpti(num, xl, xi, wi, b,&
                  g)
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
! -----------------------------------------------------------
! ---  POSITION ET POIDS DES POINTS DE GAUSS + MATRICE B
!         DE L'ELEMENT POUTRE EULER (HERMITE)
! --- IN : LONGUEUR DE L'ELEMENT XL
! --- IN : NUMERO DU POINT DE GAUSS
! --- OUT : XI POSITION DU POINT
!           WI POIDS DE CE POINT
!           B MATRICE B (4 VALEURS DIFFERENTES NON NULLES)
! -----------------------------------------------------------
    integer :: num
    real(kind=8) :: xl, xi, wi, b(4), g
    real(kind=8) :: un, deux, quatre, six, douze
    parameter (un=1.d0,deux=2.d0,quatre=4.d0,six=6.d0,douze=12.d0)
    real(kind=8) :: xg(2), wg(2), xp(2)
    data xg /-.57735026918963d0,.57735026918963d0/
    data wg /1.d0 , 1.d0/
    data xp /0.d0 , 1.d0/
!
    if (num .gt. 0) then
!       NUM=1 OU 2 : POINTS DE GAUSS
        xi=(un + xg(num))/ deux
!       LE JACOBIEN L/2 EST MIS DIRECTEMENT DANS LE POIDS
        wi=wg(num)*xl/deux
    else
!       NUM=-1 OU -2 : NOEUDS
        xi=xp(-num)
    endif
!     -- ON NE STOCKE PAS LES 0. DE LA MATRICE B,
!        ON NE CALCULE PAS LES OPPOSES
    b(1)=un/xl
    b(2)=(-six+douze*xi)/xl/xl
    b(3)=(-quatre+six*xi)/xl
    b(4)=(-deux+six*xi)/xl
!
    g=(-deux*xi+un)*quatre/xl
!
end subroutine
