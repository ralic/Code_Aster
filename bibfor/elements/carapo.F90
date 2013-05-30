subroutine carapo(sect, geom, orien, xl, pgl,&
                  itype, a, xiy, xiz, xjx,&
                  alfay, alfaz, ey, ez, a2,&
                  xiy2, xiz2, xjx2, alfay2, alfaz2)
!
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
!
    implicit none
    include 'asterfort/matrot.h'
    include 'asterfort/u2mess.h'
    integer :: itype
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx, ez, ey
    real(kind=8) :: a2, xiy2, xiz2, alfay2, alfaz2, xjx2, xl
    real(kind=8) :: pgl(3, 3), sect(*), geom(6), orien(3)
!
!     RECUPERATION DES CARACTERISTIQUES GEOMETRIQUES POUR
!     LES ELEMENTS DE POUTRE 'MECA_POU_D_E/D_T'
!        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!
! IN  SECT   : R8 : VECTEUR CONTENANT LES CARACTERISTIQUES
! IN  GEOM   : R8 : COORDONNES DES NOEUDS
! IN  ORIEN  : R8 : ORIENTATION 3 ANGLE NAUTIQUES
! OUT XL     : R8 : LONGUEUR DE L'ELEMENT
! OUT PGL    : R8 : MATRICE (3,3) DE ROTATION
! OUT ITYPE  : IS : TYPE DE VARIATION DE SECTION
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    itype = nint(sect(23))
!     --- SECTION INITIALE ---
    a = sect(1)
    xiy = sect(2)
    xiz = sect(3)
    alfay = sect(4)
    alfaz = sect(5)
    xjx = sect(8)
!     --- SECTION FINALE ---
    a2 = sect(12)
    xiy2 = sect(13)
    xiz2 = sect(14)
    alfay2 = sect(15)
    alfaz2 = sect(16)
    ey = -(sect(6)+sect(17))/2.d0
    ez = -(sect(7)+sect(18))/2.d0
    xjx2 = sect(19)
!
    xl = sqrt( (geom(4)-geom(1))**2 + (geom(5)-geom(2))**2 + (geom(6)-geom(3))**2)
    if (xl .eq. 0.d0) then
        call u2mess('F', 'ELEMENTS_17')
    endif
!
    call matrot(orien, pgl)
end subroutine
