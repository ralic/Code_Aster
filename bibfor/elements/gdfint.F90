subroutine gdfint(kp, nno, ajacob, pjacob, en,&
                  enprim, x0pg, pn, pm, fint)
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
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LA
!           CONTRIBUTION DU POINT DE GAUSS NUMERO KP AUX FORCES INTERNES
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           NNO       : NOMBRE DE NOEUDS DE L'ELEMENT
!           AJACOB    : JACOBIEN
!           PJACOB    : POIDS * JACOBIEN
!           EN        : FONCTIONS DE FORME
!           ENPRIM    : DERIVEES DES FONCTIONS DE FORME
!           X0PG      : DERIVEES DES COORDONNEES PAR RAP. A L'ABS. CURV.
!           PN        : RESULTANTE DES FORCES AU PT DE GAUSS EN AX.GENE.
!           PM        : MOMENT RESULTANT AU PT DE GAUSS EN AXES GENERAUX
!
!     OUT : FINT      : FORCES INT. (CUMUL DES CONTRIB. DES PTS DE GAUS)
! ------------------------------------------------------------------
    implicit none
#include "asterfort/gdmb.h"
#include "asterfort/promat.h"
#include "asterfort/transp.h"
    real(kind=8) :: en(3, 2), enprim(3, 2), x0pg(3), pn(3), pm(3), fint(6, 3)
    real(kind=8) :: b(6, 6), bt(6, 6), vect(6), fors(6)
    integer :: i, k, kp, ne, nno
    real(kind=8) :: ajacob, pjacob
!-----------------------------------------------------------------------
!
    do 11 ne = 1, nno
        call gdmb(ne, kp, ajacob, en, enprim,&
                  x0pg, b)
        call transp(b, 6, 6, 6, bt,&
                    6)
        do 1 i = 1, 3
            vect(i) = pn(i)
            vect(3+i) = pm(i)
 1      continue
        call promat(bt, 6, 6, 6, vect,&
                    6, 6, 1, fors)
        do 5 k = 1, 6
            fint(k,ne) = fint(k,ne) + pjacob*fors(k)
 5      continue
11  end do
end subroutine
