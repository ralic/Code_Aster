subroutine renuu1(coin, longi, ordo, longo, nbco,&
                  newn)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
    integer :: longi, longo, coin(*), ordo(*), nbco(*), newn(*)
! ----------------------------------------------------------------------
!     BUT:     ORDONNER LA LISTE DE NOEUDS COIN(*) DE LONGUEUR LONGI
!           DANS LA LISTE ORDO(*) :
!              UN NOEUD I EST PLACE AVANT UN NOEUD J SI SON NOMBRE DE
!           CONNECTIVITE EST INFERIEUR A CELUI DE J.
!
!              LES NOEUDS DEJA RENUMEROTES NE SONT PAS TRAITES
!           (CE SONT LES NOEUDS DONT LE .NEWN EST DEJA >0)
!           LA LISTE DES NOEUDS ORDONNES PEUT DONC ETRE DE LONGUEUR
!           MOINDRE QUE COIN : LONGO <= LONGI)
!
!     IN : LONGI : LONGUEUR UTILE DE COIN
!          COIN(*) : LISTE DES NOEUDS A ORDONNER.
!          NBCO(I) : NOMBRE DE NOEUDS CONNECTES AU NOEUD I.
!          NEWN(I) : NOUVEAU NUMERO DU NOEUD I (0 SI PAS RENUMEROTE).
!
!     OUT: LONGO : LONGUEUR UTILE DE ORDO
!          ORDO(*) : LISTE DES NOEUDS REORDONNES.
!
! ----------------------------------------------------------------------
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j, k
!-----------------------------------------------------------------------
    longo=0
    do 1, i=1,longi
    if (newn(coin(i)) .gt. 0) goto 1
    longo=longo+1
!
!        -- ON PLACE LE 1ER EN 1ER :
    if (longo .eq. 1) then
        ordo(1)= coin(i)
        goto 1
    endif
!
    do 2, j=1,longo-1
!           -- ON INSERE COIN(I) :
    if (nbco(coin(i)) .le. nbco(ordo(j))) then
        do 3, k= longo-1,j,-1
        ordo(k+1)= ordo(k)
 3      continue
        ordo(j)= coin(i)
        goto 1
    endif
 2  continue
    ordo(longo)= coin(i)
    1 end do
!
end subroutine
