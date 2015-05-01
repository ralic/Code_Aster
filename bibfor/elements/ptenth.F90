subroutine ptenth(ul, xl, f, n, mat,&
                  itype, enerth)
! aslint: disable=
    implicit  none
#include "jeveux.h"
#include "asterfort/jevech.h"
    integer :: itype, n
    real(kind=8) :: ul(12), f, mat(n, n), enerth
!     ------------------------------------------------------------------
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
!     CALCUL DE L'ENERGIE DE DEFORMATION THERMIQUE POUR LES
!          ELEMENTS POUTRE (POU_D_T, POU_D_E, POU_C_T)
!     ------------------------------------------------------------------
! IN  UL     :  DEPLACEMENTS DES NOEUDS DE LA POUTRE
! IN  XL     :  LONGUEUR DE LA POUTRE
! IN  F      :  DILATATION DU MATERIAU
! IN  N      :  DIMENSION DE LA MATRICE MAT
! IN  MAT    :  MATRICE DE RAIDEUR
! IN  ITYPE  :  TYPE DE LA SECTION
! OUT ENERTH :  ENERGIE DE DEFORMATION THERMIQUE
!     ------------------------------------------------------------------
    integer :: i, j, lrcou
    real(kind=8) :: ugt(12), flt(12), along, angs2, deux, rad, xl, zero, flm(12)
!     ------------------------------------------------------------------
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.d0
    deux = 2.d0
!
    enerth = zero
    do 10 i = 1, 12
        ugt(i) = zero
        flt(i) = zero
        flm(i) = zero
10  end do
!
! --- CARACTERISTIQUES DE POUTRES COURBES :
!     -----------------------------------
    if (itype .eq. 10) then
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angs2 = asin( xl / ( deux * rad ) )
        xl = rad * angs2 * deux
    endif
!
!
    if (f .ne. zero) then
        if (itype .ne. 10) then
            ugt(1) = -f*xl
            ugt(7) = -ugt(1)
        else
            along = deux*rad*f*sin(angs2)
            ugt(1) = -along*cos(angs2)
            ugt(2) = along*sin(angs2)
            ugt(7) = -ugt(1)
            ugt(8) = ugt(2)
        endif
!
! ---   CALCUL DES FORCES INDUITES PAR LES DEFORMATIONS THERMIQUES :
!       ----------------------------------------------------------
        do 20 i = 1, 6
            do 30 j = 1, 6
                flt(i) = flt(i) - mat(i,j) *ugt(j)
                flt(i+6) = flt(i+6) - mat(i+6,j+6)*ugt(j+6)
                flm(i) = flm(i) - mat(i,j) *ul(j)
                flm(i+6) = flm(i+6) - mat(i+6,j+6)*ul(j+6)
30          continue
20      continue
!
! ---   ENERGIE DE DEFORMATION INDUITE PAR LES DEFORMATIONS THERMIQUES :
!       --------------------------------------------------------------
        do 40 i = 1, 12
            enerth = enerth + (0.5d0*ugt(i)*flt(i)-ugt(i)*flm(i))
40      continue
!
    endif
!
!
end subroutine
