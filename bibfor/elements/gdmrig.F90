subroutine gdmrig(kp, nno, ajacob, pjacob, en,&
                  enprim, x0pg, rot0, rotk, granc,&
                  pn, pm, rigi)
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
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LA
!           CONTRIBUTION DU POINT DE GAUSS KP A LA MATRICE DE RIGIDITE.
!           CETTE MATRICE EST DISSYMETRIQUE, PLEINE ET RANGEE LIGNE PAR
!           LIGNE.
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           NNO       : NOMBRE DE NOEUDS
!           AJACOB    : JACOBIEN
!           PJACOB    : POIDS * JACOBIEN
!           EN        : FONCTIONS DE FORME
!           ENPRIM    : DERIVEES DES FONCTIONS DE FORME
!           X0PG      : DERIVEES DES COORDONNEES PAR RAP. A L'ABS. CURV.
!           ROT0      : MATRICE DE ROTATION DES AXES PRINCIPAUX D'INERT.
!                       AU POINT DE GAUSS DANS LA POSITION DE REFERENCE,
!                       PAR RAPPORT AUX AXES GENERAUX
!           ROTK      : MATRICE DE ROTATION
!           GRANC     : MATRICE DIAGONALE DE COMPORTEMENT ELASTIQUE
!           PN        : RESULTANTE DES FORCES AU PT DE GAUSS EN AX.GENE.
!           PM        : MOMENT RESULTANT AU PT DE GAUSS EN AXES GENERAUX
!
!     OUT : RIGI      : MATRICE DE RIGIDITE (CUMUL DES CONTRIBUTIONS DES
!                       POINTS DE GAUSS)
! ------------------------------------------------------------------
    implicit none
#include "asterfort/cumuma.h"
#include "asterfort/extrma.h"
#include "asterfort/gdmb.h"
#include "asterfort/gdmd.h"
#include "asterfort/gdmups.h"
#include "asterfort/promat.h"
#include "asterfort/stokma.h"
#include "asterfort/transp.h"
    real(kind=8) :: en(3, 2), enprim(3, 2), x0pg(3), rot0(3, 3), rotk(3, 3)
    real(kind=8) :: granc(6), pn(3), pm(3), rigi(18, 18), d(9, 9)
    real(kind=8) :: stokaj(9, 6, 6), pi(6, 6), cpit(6, 6), picpit(6, 6)
    real(kind=8) :: picpbj(6, 6), bi(6, 6), bj(6, 6), bibj(6, 6), bit(6, 6)
    real(kind=8) :: upsi(9, 6), upsj(9, 6), upiupj(6, 6), upsit(6, 9)
    real(kind=8) :: dupsj(9, 6), rotabs(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i, j, kp, ne, nno
    real(kind=8) :: ajacob, pjacob, zero
!-----------------------------------------------------------------------
    zero = 0.d0
!
    do 7 j = 1, 6
        do 6 i = 1, 6
            pi(i,j) = zero
            cpit(i,j) = zero
 6      end do
 7  end do
!
    call promat(rotk, 3, 3, 3, rot0,&
                3, 3, 3, rotabs)
!
    do 9 j = 1, 3
        do 8 i = 1, 3
            pi(i,j) = rotabs(i,j)
            pi(3+i,3+j) = pi(i,j)
            cpit(i,j) = granc(i) * rotabs(j,i)
            cpit(3+i,3+j) = granc(3+i) * rotabs(j,i)
 8      end do
 9  end do
!
    call promat(pi, 6, 6, 6, cpit,&
                6, 6, 6, picpit)
!
    call gdmd(x0pg, pn, pm, d)
!
    do 21 ne = 1, nno
        call gdmb(ne, kp, ajacob, en, enprim,&
                  x0pg, bi)
        call stokma(bi, 6, 6, ne, stokaj)
        call gdmups(ne, kp, ajacob, en, enprim,&
                    upsi)
        call stokma(upsi, 9, 6, nno+ne, stokaj)
21  end do
    do 41 j = 1, nno
        call extrma(stokaj, 6, 6, j, bj)
        call promat(picpit, 6, 6, 6, bj,&
                    6, 6, 6, picpbj)
        call extrma(stokaj, 9, 6, nno+j, upsj)
        call promat(d, 9, 9, 9, upsj,&
                    9, 9, 6, dupsj)
        do 31 i = 1, nno
            call extrma(stokaj, 6, 6, i, bi)
            call transp(bi, 6, 6, 6, bit,&
                        6)
            call promat(bit, 6, 6, 6, picpbj,&
                        6, 6, 6, bibj)
            call cumuma(i, j, bibj, pjacob, rigi)
!
            call extrma(stokaj, 9, 6, nno+i, upsi)
            call transp(upsi, 9, 9, 6, upsit,&
                        6)
            call promat(upsit, 6, 6, 9, dupsj,&
                        9, 9, 6, upiupj)
            call cumuma(i, j, upiupj, pjacob, rigi)
!*** FIN DE I
31      end do
!*** FIN DE J
41  end do
end subroutine
