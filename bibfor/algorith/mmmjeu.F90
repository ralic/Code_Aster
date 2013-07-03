subroutine mmmjeu(ndim, jeusup, norm, geome, geomm,&
                  ddeple, ddeplm, mprojt, jeu, djeu,&
                  djeut, iresog, tau1, tau2, gene11,&
                  gene21)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: ndim
    real(kind=8) :: jeusup
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: geomm(3), geome(3)
    real(kind=8) :: ddeple(3), ddeplm(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: jeu, djeu(3), djeut(3)
    integer :: iresog
    real(kind=8) :: gene11(3, 3), gene21(3, 3)
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
! IN  IRESOG : ALGO. DE RESOLUTION POUR LA GEOMETRIE
!              0 - POINT FIXE
!              1 - NEWTON
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT GENE11 : MATRICE
! OUT GENE21 : MATRICE
!
! ----------------------------------------------------------------------
!
    integer :: idim, i, j
!
! ----------------------------------------------------------------------
!
    do 1 idim = 1, 3
        djeu(idim) = 0.d0
        djeut(idim) = 0.d0
 1  end do
    jeu = 0.d0
!
! --- CALCUL DE L'INCREMENT DE JEU
!
    do 5 idim = 1, 3
        djeu(idim) = ddeple(idim) - ddeplm(idim)
 5  end do
!
! --- CALCUL DU JEU TOTAL
!
    jeu = jeusup
    do 10 idim = 1, ndim
        jeu = jeu + ( geome(idim)+ddeple(idim)- geomm(idim)-ddeplm( idim))*norm(idim)
10  end do
!
! --- PROJECTION DE L'INCREMENT DE JEU SUR LE PLAN TANGENT
!
    do 20 i = 1, ndim
        do 25 j = 1, ndim
            djeut(i) = mprojt(i,j)*djeu(j)+djeut(i)
25      continue
20  end do
!
! --- MATRICE GENE11 ET GENE21
!
    if (iresog .eq. 1) then
        if ((1.d0-jeu**2) .ne. 0.0d0) then
            do 24 i = 1, ndim
                do 26 j = 1, ndim
                    gene11(i,j) = gene11(i,j)*jeu/(1.d0-jeu**2)+ tau1(i)*tau1(j)/(1.d0-jeu**2)
!
                    gene21(i,j) = gene21(i,j)*jeu/(1.d0-jeu**2)+ tau2(i)*tau1(j)/(1.d0-jeu**2)
26              continue
24          continue
        else
            call matini(3, 3, 0.d0, gene11)
            call matini(3, 3, 0.d0, gene21)
        endif
    else
        call matini(3, 3, 0.d0, gene11)
        call matini(3, 3, 0.d0, gene21)
    endif
!
end subroutine
