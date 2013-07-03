subroutine gdinor(norm, lobj2, iadnum, coorn, in2)
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
    implicit none
!
!     ------------------------------------------------------------------
!
!      FONCTION REALISEE:
!
!      CALCUL DE LA DIRECTION DU CHAMP THETA DANS LE CAS OU LA NORMALE
!      AU PLAN DES LEVRES FIGURE DANS LA SD FOND_FISS (OPTION NORMALE
!      DE DEFI_FOND_FISS)
!
!     ------------------------------------------------------------------
!
! ENTREE:
!        NORM   : NORMALE AU PLAN DES LEVRES DE LA FISSURE
!        LOBJ2  : NOMBRE DE NOEUDS DE GAMM0
!        ZI(IADNUM): NUMEROS DES NOEUDS DU FOND DE FISSURE
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DU MAILLAGE
!
! SORTIE:
!        ZR(IN2): NORMALE UNITAIRE EN TOUT NOEUD DE GAMMA0
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: norm, coorn
!
    real(kind=8) :: dir1x, dir1y, dir1z
    real(kind=8) :: dir2x, dir2y, dir2z
    real(kind=8) :: dir11x, dir11y, dir11z
    real(kind=8) :: dirmox, dirmoy, dirmoz
    real(kind=8) :: norme, nx, ny, nz
!
!
!
!-----------------------------------------------------------------------
    integer :: i, iacoor, iadnum, ianorm, in2, lobj2, num1
    integer :: num2
    real(kind=8) :: x1, x2, x21, y1, y2, y21, z1
    real(kind=8) :: z2, z21
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(norm, 'L', ianorm)
    nx = zr(ianorm)
    ny = zr(ianorm+1)
    nz = zr(ianorm+2)
!
    call jeveuo(coorn, 'L', iacoor)
    dir1x = 0.d0
    dir1y = 0.d0
    dir1z = 0.d0
!
!    BOUCLE SUR LES NOEUDS ORDONNES DU FOND DE FISSURE
!
    do 1 i = 1, lobj2-1
        dir2x = dir1x
        dir2y = dir1y
        dir2z = dir1z
        num1 = zi(iadnum+i-1)
        num2 = zi(iadnum+i)
        x1 = zr(iacoor+(num1-1)*3 )
        y1 = zr(iacoor+(num1-1)*3+1)
        z1 = zr(iacoor+(num1-1)*3+2)
        x2 = zr(iacoor+(num2-1)*3 )
        y2 = zr(iacoor+(num2-1)*3+1)
        z2 = zr(iacoor+(num2-1)*3+2)
        x21 = x2-x1
        y21 = y2-y1
        z21 = z2-z1
!
!    CALCUL DU PRODUIT VECTORIEL : U VECT N , OU U EST LE VECTEUR ARETE
!
        dir1x = y21*nz - z21*ny
        dir1y = z21*nx - x21*nz
        dir1z = x21*ny - y21*nx
        if (i .ne. 1) then
!
!    MOYENNAGE DES 2 DIRECTIONS POUR UN NOEUD APPARTENANT A 2 ARETES
!
            dirmox = (dir1x+dir2x)/2.d0
            dirmoy = (dir1y+dir2y)/2.d0
            dirmoz = (dir1z+dir2z)/2.d0
        else
            dirmox = dir1x
            dirmoy = dir1y
            dirmoz = dir1z
            dir11x = dir1x
            dir11y = dir1y
            dir11z = dir1z
        endif
        norme = sqrt(dirmox*dirmox+dirmoy*dirmoy+dirmoz*dirmoz)
        zr(in2+3*(i-1) ) = dirmox/norme
        zr(in2+3*(i-1)+1) = dirmoy/norme
        zr(in2+3*(i-1)+2) = dirmoz/norme
 1  end do
!
!    TRAITEMENT DU DERNIER NOEUD
!    PAS DE MOYENNE SI DERNIER NOEUD DIFFERENT DU PREMIER
!    SINON COURBE FERMEE : MOYENNE
!
    if (zi(iadnum+lobj2-1) .eq. zi(iadnum+1-1)) then
        dirmox = (dir1x+dir11x)/2.d0
        dirmoy = (dir1y+dir11y)/2.d0
        dirmoz = (dir1z+dir11z)/2.d0
        norme = sqrt(dirmox*dirmox+dirmoy*dirmoy+dirmoz*dirmoz)
        zr(in2+3*(lobj2-1) ) = dirmox/norme
        zr(in2+3*(lobj2-1)+1) = dirmoy/norme
        zr(in2+3*(lobj2-1)+2) = dirmoz/norme
        zr(in2+3*(1-1) ) = dirmox/norme
        zr(in2+3*(1-1) +1) = dirmoy/norme
        zr(in2+3*(1-1) +2) = dirmoz/norme
    else
        norme = sqrt(dir1x*dir1x+dir1y*dir1y+dir1z*dir1z)
        zr(in2+3*(lobj2-1) ) = dir1x/norme
        zr(in2+3*(lobj2-1)+1) = dir1y/norme
        zr(in2+3*(lobj2-1)+2) = dir1z/norme
    endif
!
    call jedema()
end subroutine
