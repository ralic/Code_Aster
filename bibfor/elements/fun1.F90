subroutine fun1(area, a1, a2, n)
    implicit none
    integer :: n
    real(kind=8) :: area, a1, a2
!     ------------------------------------------------------------------
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
!         CALCUL DE L'AIRE OU DE LA CONSTANTE DE TORSION EQUIVALENTE
!    D'UNE POUTRE DROITE A SECTION VARIABLE SOUS L'HYPOTHESE DE VARIA-
!    TION LINEAIRE DES COORDONNEES
!     ------------------------------------------------------------------
!                        LISTE DES ARGUMENTS
!    TYPE  !   NOM  !  TABLEAU  !             SIGNIFICATION
!    -------------------------------------------------------------------
! IN  R8   ! A1     !     -     ! VALEUR INITIALE
! IN  R8   ! A2     !     -     ! VALEUR FINALE
! IN  IS   ! N      !     -     ! ORDRE DU POLYNOME
! OUT  R8  ! AREA   !     -     ! VALEUR EQUIVALENTE
!     ------------------------------------------------------------------
!
    real(kind=8) :: xm, xm1, xm2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (a1 .eq. a2) then
        area = a1
    else
        if (n .lt. 2) then
            area = (a2-a1) / (log(a2)-log(a1))
        else if (n .eq.2) then
!           VARIATION HOMOTHETIQUE.
            area = sqrt (a1*a2)
        else if (n .eq.3) then
            xm = 2.d0/3.d0
            xm1 = a1 ** xm
            xm2 = a2 ** xm
            area = 2 * (xm1*a2 - a1*xm2 ) / (xm2-xm1)
        else
            xm = 1.d0 / n
            area = (n-1)*((a2**xm)-a1**xm)
            xm = xm-1.d0
            area=area / ((a1**xm)-a2**xm)
        endif
    endif
end subroutine
