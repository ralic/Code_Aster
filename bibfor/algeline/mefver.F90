subroutine mefver(ndim, som, xint, yint, rint)
! aslint: disable=
    implicit none
!
#include "asterc/r8pi.h"
#include "asterfort/utmess.h"
    integer :: ndim(14)
    real(kind=8) :: som(9), xint(*), yint(*), rint(*)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     VERIFICATION DE L'ORDRE ET DE LA BONNE DISPOSITION DES SOMMETS DE
!     L ENCEINTE RECTANGULAIRE
!     VERIFICATION DE L INCLUSION DES FAISCEAUX DANS L ENCEINTE
!     CIRCULAIRE
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NDIM   : TABLEAU DES DIMENSIONS
! IN  : SOM    : COORDONNEES DES SOMMETS DE L'ENCEINTE RECTANGULAIRE
!                OU XEXT,YEXT,REXT
! IN  : XINT   : COORDONNEES 'X' DES CENTRES DES CYLINDRES DANS
!                LE REPERE AXIAL
! IN  : YINT   : COORDONNEES 'Y' DES CENTRES DES CYLINDRES DANS
!                LE REPERE AXIAL
! IN  : RINT   : RAYONS DES CYLINDRES
! ----------------------------------------------------------------------
    integer :: ind(3)
    real(kind=8) :: xsom(4), ysom(4), ux(4), uy(4), norm, a1, a(4)
    real(kind=8) :: vect(4), long(4)
    character(len=3) :: note
!     ------------------------------------------------------------------
!
! --- LECTURE DES DIMENSIONS
!-----------------------------------------------------------------------
    integer :: i, iencei, j, nbcyl
    real(kind=8) :: diff, epsit, pi, pis2, proj, rext
    real(kind=8) :: xext, yext
!-----------------------------------------------------------------------
    nbcyl = ndim(3)
    iencei = ndim(6)
!
!
    pi = r8pi()
    pis2 = pi / 2.d0
    epsit = 1.d-5
!
    if (iencei .eq. 2) then
        do 10 i = 1, 4
            xsom(i) = som(2*i-1)
            ysom(i) = som(2*i)
10      continue
!
!
! ---    MISE EN ORDRE DES SOMMETS DE L ENCEINTE
!
        ux(1) = xsom(2) - xsom(1)
        uy(1) = ysom(2) - ysom(1)
        ux(2) = xsom(3) - xsom(1)
        uy(2) = ysom(3) - ysom(1)
        ux(3) = xsom(4) - xsom(1)
        uy(3) = ysom(4) - ysom(1)
!
        do 20 i = 2, 3
            norm = (ux(i)*ux(i)+uy(i)*uy(i)) * (ux(1)*ux(1)+uy(1)*uy( 1))
            norm = sqrt(norm)
            if (norm .eq. 0.d0) then
                call utmess('F', 'ALGELINE_88')
            endif
            a(i-1) = acos((ux(i)*ux(1)+uy(i)*uy(1)) / norm)
            a1 = asin((ux(1)*uy(i)-uy(1)*ux(i)) / norm)
            if (a1 .lt. 0.d0) a(i-1) = 2*pi - a(i-1)
20      continue
!
        if (a(1) .lt. a(2) .and. a(2) .lt. pi) then
            ind(1) = 2
            ind(2) = 3
            ind(3) = 4
        else if (a(1).gt.a(2) .and.a(1).lt.pi) then
            ind(1) = 2
            ind(2) = 4
            ind(3) = 3
        else if (a(1).lt.pis2.and.a(2).gt.pi) then
            ind(1) = 4
            ind(2) = 2
            ind(3) = 3
        else if (a(2).lt.pis2.and.a(1).gt.pi) then
            ind(1) = 3
            ind(2) = 2
            ind(3) = 4
        else if (a(1).lt.a(2).and.a(1).gt.pi) then
            ind(1) = 3
            ind(2) = 4
            ind(3) = 2
        else if (a(1).gt.a(2).and.a(2).gt.pi) then
            ind(1) = 4
            ind(2) = 3
            ind(3) = 2
        else
            call utmess('F', 'ALGELINE_89')
        endif
!
        do 30 i = 1, 3
            som(2*(i+1)-1) = xsom(ind(i))
            som(2*(i+1)) = ysom(ind(i))
30      continue
!
! ---    ON VERIFIE QUE LES QUATRES SOMMETS FORMENT BIEN UN RECTANGLE
        do 40 i = 1, 4
            xsom(i) = som(2*i-1)
            ysom(i) = som(2*i)
40      continue
!
        ux(1) = xsom(2) - xsom(1)
        uy(1) = ysom(2) - ysom(1)
        ux(2) = xsom(4) - xsom(1)
        uy(2) = ysom(4) - ysom(1)
        ux(3) = xsom(4) - xsom(3)
        uy(3) = ysom(4) - ysom(3)
        ux(4) = xsom(3) - xsom(2)
        uy(4) = ysom(3) - ysom(2)
!
        do 60 i = 1, 2
            vect(i) = ux(i)*uy(i+2)-uy(i)*ux(i+2)
60      continue
!
        norm = (ux(2)*ux(2)+uy(2)*uy(2)) * (ux(1)*ux(1)+uy(1)*uy(1))
        norm = sqrt(norm)
        if (norm .eq. 0.d0) then
            call utmess('F', 'ALGELINE_88')
        endif
        a(1) = acos((ux(2)*ux(1)+uy(2)*uy(1)) / norm)
        if ((abs(a(1)-pis2)+abs(vect(1))+abs(vect(2))) .gt. epsit) then
            call utmess('F', 'ALGELINE_89')
        endif
!
!
! ---    VERIFICATION DE L INCLUSION DES CYLINDRES DANS L ENCEINTE
! ---    RECTANGULAIRE
!
! ---    NORMALISATION DES VECTEURS U(1) ET U(2)
        do 70 i = 1, 2
            long(i) = sqrt(ux(i)*ux(i)+uy(i)*uy(i))
            ux(i) = ux(i) / long(i)
            uy(i) = uy(i) / long(i)
70      continue
!
        do 90 i = 1, nbcyl
            do 80 j = 1, 2
                proj = ux(j)*(xint(i)-xsom(1)) + uy(j)*(yint(i)-ysom( 1))
                if ((proj-rint(i)) .lt. 0.d0 .or. (proj+rint(i)) .gt. long(j)) then
                    write(note(1:3),'(I3.3)') i
                    call utmess('F', 'ALGELINE_90', sk=note)
!
                endif
80          continue
90      continue
!
! ---    VERIFICATION DE L INCLUSION DES CYLINDRES DANS L ENCEINTE
! ---    CIRCULAIRE
!
    else if (iencei.eq.1) then
        xext = som(1)
        yext = som(2)
        rext = som(3)
        do 100 i = 1, nbcyl
            diff = sqrt((xext-xint(i))**2 + (yext-yint(i))**2)
            if ((diff+rint(i)) .gt. rext) then
                write(note(1:3),'(I3.3)') i
                call utmess('F', 'ALGELINE_81', sk=note)
            endif
100      continue
    endif
!
!
end subroutine
