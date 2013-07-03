subroutine rogllo(nb1, nb2, vrg, blam, ctor,&
                  knn)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! ......................................................................
!     FONCTION  :  ROTATION DES BLOCS 3 3 DE LA MATRICE DE RIGIDITE
!                  DU REPERE GLOBAL AU REPERE LOCAL
!                  COQUE_3D
!
!           (                                  )         (    U    )
!           ( ( M_TRANSLATION ) (  COUPLAGE )  )         (    V    )
!           (                                  )         (    W    )
! ( VRG ) = (                                  )   ( U )=(         )
!       I   (                                  )         ( THETA_X )
!           ( (   COUPLAGE    ) ( M_ROTATION ) )         ( THETA_Y )
!           (                                  )         ( THETA_Z )
!                                                I                   I
!   ON TOURNE  ( M_ROTATION ) SEULEMENT
!
! ......................................................................
!
#include "asterc/r8prem.h"
#include "asterfort/btkb.h"
    integer :: in
    integer :: ii, jj
    integer :: i, j
    integer :: nb1, nb2
    integer :: irig
!
!---- DECLARATIONS RIGIDITE GEOMETRIQUE
!
    real(kind=8) :: vrg ( 2601 )
    real(kind=8) :: blam ( 9 , 3 , 3 )
    real(kind=8) :: rigrl ( 3 , 3 )
    real(kind=8) :: rigrg ( 3 , 3 )
    real(kind=8) :: bid33 ( 3 , 3 )
    real(kind=8) :: ctor, knn, xmin
!
    real(kind=8) :: barl ( 3 , 3 )
!
! DEB
!
!---- A CHAQUE ITERATION
!
    xmin = 1.d0 / r8prem ( )
!
!---- EN CHAQUE NOEUD
!
    do 401 in = 1, nb2
!
!------- ON RECUPERE BARLAMBDA
!
        do 411 jj = 1, 3
            do 422 ii = 1, 3
!
                barl ( ii , jj ) = blam ( in , ii , jj )
!
422          continue
411      continue
!
!-------    ON CONSTRUIT RIGRG
!
        if (in .le. nb1) then
!
!--------------    NOEUDS DE SERENDIP
            do 431 jj = 1, 3
                do 441 ii = 1, 3
                    j = 6 * ( in - 1 ) + jj + 3
                    i = 6 * ( in - 1 ) + ii + 3
                    irig = ( 6 * nb1 + 3 ) * ( j - 1 ) + i
                    rigrg ( ii , jj ) = vrg ( irig )
441              continue
431          continue
!
        else
!
!--------------    SUPERNOEUD
            do 451 jj = 1, 3
                do 461 ii = 1, 3
                    j = 6 * nb1 + jj
                    i = 6 * nb1 + ii
                    irig = ( 6 * nb1 + 3 ) * ( j - 1 ) + i
                    rigrg ( ii , jj ) = vrg ( irig )
461              continue
451          continue
!
        endif
!
!-------    ROTATION DE RIGRG : LOCALES --> GLOBALES
!
!           RIGRL =  ( LAMBDA0 )   * SIGMT * ( LAMBDA0 ) T
!
!
        call btkb(3, 3, 3, rigrg, barl,&
                  bid33, rigrl)
!
!-------    ON COMPARE LES DEUX PREMIERS TERMES DIAGONAUX DE RIGRL
!
        if (abs(rigrl ( 1 , 1 )) .lt. xmin) then
            xmin = abs(rigrl ( 1 , 1 ))
        endif
        if (abs(rigrl ( 2 , 2 )) .lt. xmin) then
            xmin = abs(rigrl ( 2 , 2 ))
        endif
!
401  continue
!
    knn = ctor * xmin
!
! FIN
!
end subroutine
