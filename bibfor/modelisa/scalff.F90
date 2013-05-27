subroutine scalff(nbfonc, nbp, disc, vale, a)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
!     CALCUL DES PRODUITS SCALAIRES ENTRE LES FONCTIONS DE FORME
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : NBFONC : NOMBRE DE FONCTIONS
! IN  : NBP    : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS
! IN  : DISC   : DISCRETISATION SUR LAQUELLE SONT CALCULEES LES
!                INTEGRALES DONNANT LES PRODUITS SCALAIRES  - DIM : NBP
! IN  : VALE   : TABLEAU DES VALEURS DES FONCTIONS  - DIM : (NBP,NBFONC)
! OUT : A      : MATRICE DES PRODUITS SCALAIRES  - DIM : (NBFONC,NBFONC)
!
!
    include 'jeveux.h'
    integer :: nbfonc, nbp
    real(kind=8) :: disc(nbp), vale(nbp, nbfonc), a(nbfonc, nbfonc)
    integer :: ifo1, ifo2, ip
    real(kind=8) :: dx, y1, y2, yy
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!-----1.CALCUL DES TERMES DU TRIANGLE INFERIEUR
!
    do 10 ifo2 = 1, nbfonc
!
        do 11 ifo1 = ifo2, nbfonc
!
            a(ifo1,ifo2) = 0.d0
            do 12 ip = 1, nbp-1
                dx = disc(ip+1) - disc(ip)
                y1 = vale(ip,ifo1)*vale(ip,ifo2)
                y2 = vale(ip+1,ifo1)*vale(ip+1,ifo2)
                yy = y1 + y2
                a(ifo1,ifo2) = a(ifo1,ifo2) + yy * dx
12          continue
            a(ifo1,ifo2) = a(ifo1,ifo2)/2.d0
!
11      continue
!
10  end do
!
!-----2.DEDUCTION DES TERMES DU TRIANGLE SUPERIEUR PAR SYMETRIE
!
    if (nbfonc .gt. 1) then
!
        do 20 ifo2 = 2, nbfonc
            do 21 ifo1 = 1, ifo2-1
                a(ifo1,ifo2) = a(ifo2,ifo1)
21          continue
20      continue
!
    endif
!
end subroutine
