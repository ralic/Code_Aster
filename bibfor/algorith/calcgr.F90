subroutine calcgr(igau, nbsig, nbvari, vip, nu,&
                  epsfl)
    implicit   none
    integer :: nbsig, igau, nbvari
    real(kind=8) :: nu, epsfl(nbsig), vip(*)
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! ======================================================================
!   BUT : CALCUL DES DEFORMATIONS DE FLUAGE DE GRANGER EN
!         UN POINT DE GAUSS
!         ROUTINE APPELEE POUR LE POST-TRAITEMENT
!----------------------------------------------------------------------
    integer :: i, k
    real(kind=8) :: epstmp(6)
!
!
    do 100 k = 1, nbsig
        epstmp(k) = vip((igau-1)*nbvari + 8*nbsig+k)
        do 90 i = 1, 8
            epstmp(k) = epstmp(k) - vip((igau-1)*nbvari+ (i-1)*nbsig+ k)
90      continue
100  continue
!
!
    epsfl(1) = epstmp(1) - nu* (epstmp(2) + epstmp( 3))
    epsfl(2) = epstmp(2) - nu* (epstmp(1) + epstmp( 3))
    epsfl(3) = epstmp(3) - nu* (epstmp(1) + epstmp( 2))
    do 110 i = 4, nbsig
        epsfl(i) = (1.d0 + nu) *epstmp(i)
110  continue
!
!
end subroutine
