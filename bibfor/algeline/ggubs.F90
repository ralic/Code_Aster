subroutine ggubs(dseed, nr, r)
    implicit none
    integer :: nr
    real(kind=8) :: dseed, r(nr)
!     ------------------------------------------------------------------
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
!     GENERATEUR DE NOMBRES (PSEUDO-)ALEATOIRES UNIFORMEMENT REPARTIS
!     ENTRE (0,1)                                     (CF GGUBS DE IMSL)
!     ------------------------------------------------------------------
!     CETTE VERSION NE FONCTIONNE QUE SUR DES MACHINES CODANT REELS OU
!     ENTIERS SUR AU MOINS 32 BITS.
!     ------------------------------------------------------------------
! VAR DSEED  - EN ENTREE UNE VALEUR ENTRE (1.E0, 2147483647.E0).
!            - EN SORTIE UNE AUTRE VALEUR POUR LE PROCHAIN TIRAGE.
! IN  NR     - NOMBRE DE VALEUR A TIRER
! OUT R      - TABLEAU CONTENANT LES VALEURS TIREES
!     ------------------------------------------------------------------
!             D2P31M=(2**31)-1,    D2P31 =(2**31)
    real(kind=8) :: d2p31m, d2p31
!-----------------------------------------------------------------------
    integer :: i, idseed
!-----------------------------------------------------------------------
    data    d2p31m/2147483647.d0/, d2p31/2147483648.d0/
!     ------------------------------------------------------------------
    do 10 i = 1, nr
!        DSEED  = AMOD(16807.*DSEED,D2P31M)
        idseed = int(16807.d0*dseed/d2p31m)
        dseed = 16807.d0*dseed-idseed*d2p31m
        r(i) = dseed / d2p31
10  end do
end subroutine
