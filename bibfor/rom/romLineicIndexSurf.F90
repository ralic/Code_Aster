subroutine romLineicIndexSurf(nb1 , tab11, tab12,&
                              nb2 , tab21, tab22,&
                              tab3, epsi)
!
implicit none
!
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, intent(in) :: nb1
    real(kind=8), intent(in) :: tab11(nb1)
    real(kind=8), intent(in) :: tab12(nb1)
    integer, intent(in) :: nb2
    real(kind=8), intent(in) :: tab21(nb2)
    real(kind=8), intent(in) :: tab22(nb2)
    integer, intent(out) :: tab3(nb1)
    real(kind=8), intent(in) :: epsi
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Utilities
!
! For lineic base
!
! --------------------------------------------------------------------------------------------------
! 
! A PARTIR DE DEUX LISTES DE COORDONNEES 3D ET DEUX LISTES DE COORDONNEES 2D
! TROUVER LA POSITION DE DEUX LISTES 3D DANS 2D
! LE RESULTAT EST UNE LISTE D'ENTIER.
!
! TAB11, TAB12      /R/: TABLEAUX 3D
! NB1               /I/: TAILLAE DU TABLEAU 3D
! TAB21, TAB22      /R/: TABLEAUX 2D
! NB2               /I/: TAILLE DU TABLEAU 2D
! TAB3              /I/: TABLEAU RESULTAT
! EPS               /R/: VALEUR DE TOLERENCE
!
! --------------------------------------------------------------------------------------------------
! 
    integer :: i, j
!
! --------------------------------------------------------------------------------------------------
! 
    do i = 1, nb1
        do j = 1, nb2
            if (abs(tab21(j)-tab11(i)).lt.epsi .and. abs(tab22(j)-tab12(i)).lt.epsi) then
                tab3(i) = j
                exit
            endif
        enddo
    enddo
        
end subroutine
