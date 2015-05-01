subroutine scaldf(nbfonc, nbp, nbmr, disc, vale,&
                  defm, b)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CALCUL DES PRODUITS SCALAIRES ENTRE LES DEFORMEES MODALES ET LES
!     FONCTIONS DE FORME ASSOCIEES A L'EXCITATION
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : NBFONC : NOMBRE DE FONCTIONS DE FORME ASSOCIEES A L'EXCITATION
! IN  : NBP    : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS DE
!                FORME SUR L'INTERVALLE 0,2L
! IN  : NBMR   : NOMBRE DE MODES PRIS EN COMPTE
! IN  : DISC   : DISCRETISATION SUR LAQUELLE SONT CALCULEES LES
!                INTEGRALES DONNANT LES PRODUITS SCALAIRES
!                DIMENSION NBP
! IN  : VALE   : TABLEAU DES VALEURS DES FONCTIONS DE FORME (NBP,NBFONC)
! IN  : DEFM   : TABLEAU DES VALEURS DES DEFORMEES MODALES (NBP,NBMR)
! OUT : B      : MATRICE DES PRODUITS SCALAIRES (NBFONC,NBMR)
!
!
#include "jeveux.h"
    integer :: nbfonc, nbp, nbmr
    real(kind=8) :: disc(nbp), vale(nbp, nbfonc), defm(nbp, nbmr)
    real(kind=8) :: b(nbfonc, nbmr)
    integer :: ifo, imr, ip
    real(kind=8) :: dx, y1, y2, yy
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
    do 10 imr = 1, nbmr
        do 20 ifo = 1, nbfonc
            b(ifo,imr) = 0.d0
            do 30 ip = 1, nbp-1
                dx = disc(ip+1) - disc(ip)
                y1 = vale(ip,ifo)*defm(ip,imr)
                y2 = vale(ip+1,ifo)*defm(ip+1,imr)
                yy = y1 + y2
                b(ifo,imr) = b(ifo,imr) + yy * dx
30          continue
            b(ifo,imr) = b(ifo,imr)/2.d0
20      continue
10  end do
end subroutine
