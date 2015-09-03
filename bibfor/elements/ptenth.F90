subroutine ptenth(ul, xl, f, n, mat,  enerth)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!     CALCUL DE L'ENERGIE DE DEFORMATION THERMIQUE POUR LES
!          ELEMENTS POUTRE (POU_D_T, POU_D_E)
!
! --------------------------------------------------------------------------------------------------
!
! IN  UL     :  DEPLACEMENTS DES NOEUDS DE LA POUTRE
! IN  XL     :  LONGUEUR DE LA POUTRE
! IN  F      :  DILATATION DU MATERIAU
! IN  N      :  DIMENSION DE LA MATRICE MAT
! IN  MAT    :  MATRICE DE RAIDEUR
! IN  ITYPE  :  TYPE DE LA SECTION
! OUT ENERTH :  ENERGIE DE DEFORMATION THERMIQUE
!
! --------------------------------------------------------------------------------------------------
!
    implicit  none
!
    integer ::  n
    real(kind=8) :: ul(12), f, mat(n,n), enerth
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ii, jj
    real(kind=8) :: ugt(12), flt(12), xl, flm(12)
!
! --------------------------------------------------------------------------------------------------
!
    enerth = 0.d0
    ugt(:) = 0.d0
    flt(:) = 0.d0
    flm(:) = 0.d0
!
    if (f .ne. 0.d0) then
        ugt(1) = -f*xl
        ugt(7) = -ugt(1)
!       calcul des forces induites par les déformations thermiques
        do ii = 1, 6
            do jj = 1, 6
                flt(ii)   = flt(ii)   - mat(ii,jj)    *ugt(jj)
                flt(ii+6) = flt(ii+6) - mat(ii+6,jj+6)*ugt(jj+6)
                flm(ii)   = flm(ii)   - mat(ii,jj)    *ul(jj)
                flm(ii+6) = flm(ii+6) - mat(ii+6,jj+6)*ul(jj+6)
            enddo
        enddo
!       energie de deformation induite par les deformations thermiques
        do  ii = 1, 12
            enerth = enerth + (0.5d0*ugt(ii)*flt(ii)-ugt(ii)*flm(ii))
        enddo
    endif
!
end subroutine
