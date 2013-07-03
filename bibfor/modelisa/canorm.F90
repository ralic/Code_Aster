subroutine canorm(coor, normal, ndim, ityp, inorm)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/u2mess.h"
    integer :: ndim, ityp, inorm
    real(kind=8) :: coor(*), normal(3)
! ======================================================================
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
!     BUT : CALCUL DE LA NORMALE A UNE MAILLE  EN UN NOEUD
!     AVEC OU SANS NORMALISATION DE CE VECTEUR
!
! IN  COOR    R8 : TABLEAU DES COORDONNEES DES NBNO NOEUDS DE LA MAILLE
!                  DE DIMENSION (3*NBNO)
! IN  NDIM    I  : DIMENSION DU MODELE (2 SI COORD_2D OU 3 SI COORD_3D)
! IN  ITYP    I  : TYPE DE LA MAILLE
! IN  INORM   I  : INDICATEUR DE NORMALISATION
!                  INORM = 0 PAS DE NORMALISATION
!                  INORM = 1 NORMALISATION
! OUT NORMALE R8 : NORMALE CALCULEE
!
! ROUTINES APPELLEES :
!     NORMEV     PROVEC
!     JENUNO     JEXNUM
!
!
!
    real(kind=8) :: xx(3), yy(3), norme, surf, vect(3)
    character(len=8) :: nomtm
!
! DEBUT ----------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: j
!-----------------------------------------------------------------------
    call jenuno(jexnum('&CATA.TM.NBNO', ityp), nomtm)
    if (nomtm(1:3) .eq. 'SEG') then
        if (ndim .eq. 3) then
            normal(1) = coor(4) - coor(1)
            normal(2) = coor(5) - coor(2)
            normal(3) = coor(6) - coor(3)
            if (inorm .eq. 1) then
                call normev(normal, norme)
            else
                call u2mess('F', 'MODELISA3_20')
            endif
!          ELSE
!            NORMAL(1) = COOR(5) - COOR(2)
!            NORMAL(2) = COOR(1) - COOR(4)
!            NORMAL(3) = 0.0D0
!            IF (INORM.EQ.1) THEN
!              CALL NORMEV(NORMAL,NORME)
!            ENDIF
!
!          ENDIF
!
        else
            normal(1) = coor(5) - coor(2)
            normal(2) = coor(1) - coor(4)
            normal(3) = 0.0d0
            if (inorm .eq. 1) then
                call normev(normal, norme)
            endif
!
        endif
!
    else if (nomtm(1:4).eq.'TRIA') then
        if (ndim .eq. 2) then
            call u2mess('F', 'MODELISA3_21')
!
        else
            do 10 j = 1, 3
                xx(j) = coor(3+j) - coor(j)
                yy(j) = coor(6+j) - coor(3+j)
10          continue
            call provec(xx, yy, normal)
            do 20 j = 1, 3
                normal(j) = normal(j)/2.0d0
20          continue
            if (inorm .eq. 1) then
                call normev(normal, norme)
            endif
!
        endif
!
    else if (nomtm(1:4).eq.'QUAD') then
        if (ndim .eq. 2) then
            call u2mess('F', 'MODELISA3_22')
!
        else
!
!     PRODUIT VECTORIEL (N1N3) * (N2N4) POUR CALCULER LE VECTEUR NORMAL
!
            do 30 j = 1, 3
                xx(j) = coor(6+j) - coor(j)
                yy(j) = coor(9+j) - coor(3+j)
30          continue
            call provec(xx, yy, normal)
            call normev(normal, norme)
            if (inorm .eq. 0) then
!
!     ON CALCULE UNE APPROXIMATION DE LA SURFACE
!     DANS L'ORDRE
!     (N1N2) * (N1N3)
!     (N1N3) * (N1N4)
!     (N2N3) * (N2N4)
!     (N2N4) * (N2N1)
!
                surf = 0.0d0
                do 40 j = 1, 3
                    xx(j) = coor(3+j) - coor(j)
                    yy(j) = coor(6+j) - coor(j)
40              continue
                call provec(xx, yy, vect)
                call normev(vect, norme)
                surf = surf + norme
                do 50 j = 1, 3
                    xx(j) = coor(6+j) - coor(j)
                    yy(j) = coor(9+j) - coor(j)
50              continue
                call provec(xx, yy, vect)
                call normev(vect, norme)
                surf = surf + norme
                do 60 j = 1, 3
                    xx(j) = coor(6+j) - coor(3+j)
                    yy(j) = coor(9+j) - coor(3+j)
60              continue
                call provec(xx, yy, vect)
                call normev(vect, norme)
                surf = surf + norme
                do 70 j = 1, 3
                    xx(j) = coor(9+j) - coor(3+j)
                    yy(j) = coor(j) - coor(3+j)
70              continue
                call provec(xx, yy, vect)
                call normev(vect, norme)
                surf = surf + norme
                surf = surf/4.0d0
                do 80 j = 1, 3
                    normal(j) = normal(j)*surf
80              continue
            endif
!
        endif
!
    endif
!
!
! FIN ------------------------------------------------------------------
end subroutine
