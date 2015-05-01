subroutine mmmjeu(ndim  ,jeusup,norm  ,geome ,geomm , &
                  ddeple,ddeplm,mprojt,jeu   ,djeu  , &
                  djeut ,iresog)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/matini.h"
#include "asterfort/assert.h"
    integer :: ndim
    integer :: iresog
    real(kind=8) :: jeusup
    real(kind=8) :: norm(3)
    real(kind=8) :: geomm(3), geome(3)
    real(kind=8) :: ddeple(3), ddeplm(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: jeu, djeu(3), djeut(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES JEUX
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  JEUSUP : JEU SUPPLEMENTAIRE PAR DIST_ESCL/DIST_MAIT
! IN  NORM   : VALEUR DE LA NORMALE
! IN  GEOME  : COORDONNEES ACTUALISEES DU POINT DE CONTACT
! IN  GEOMM  : COORDONNEES ACTUALISEES DU PROJETE DU POINT DE CONTACT
! IN  DDEPLE : INCREMENT DEPDEL DU DEPL. DU POINT DE CONTACT
! IN  DDEPLM : INCREMENT DEPDEL DU DEPL. DU PROJETE DU POINT DE CONTACT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! OUT JEU    : JEU NORMAL ACTUALISE
! OUT DJEU   : INCREMENT DEPDEL DU JEU
! OUT DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
!
! ----------------------------------------------------------------------
!
    integer :: idim, i, j
!
! ----------------------------------------------------------------------
!
   
    do idim = 1, 3
        djeu(idim) = 0.d0
        djeut(idim) = 0.d0
    end do
    jeu = 0.d0
!
! --- CALCUL DE L'INCREMENT DE JEU
!
    do idim = 1, 3
        djeu(idim) = ddeple(idim) - ddeplm(idim)
    end do
!
! ---- CALCUL DU JEU TOTAL :
! ---- LE JEU EST CALCULE A PARTIR DE (MMREAC+MMDEPM) SUIVANT
! ---- POINT FIXE  : (MAILLAGE+DEPMOI          =GEOM_)+DDEPL_ 
! ---- NEWTON GENE : (MAILLAGE+DEPMOI+PPE*DDPL_=GEOM_)
! ---- GEOM_ --> MMREAC, DDEPL_ --> MMDEPM 
!
    jeu = jeusup
    do idim = 1, ndim
        jeu = jeu + ( geome(idim)+(1-iresog)*ddeple(idim) &
                - geomm(idim)-(1-iresog)*ddeplm( idim))*norm(idim)
    end do
!
! --- PROJECTION DE L'INCREMENT DE JEU SUR LE PLAN TANGENT
!
    do i = 1, ndim
        do j = 1, ndim
            djeut(i) = mprojt(i,j)*djeu(j)+djeut(i)
        end do
    end do


end subroutine
