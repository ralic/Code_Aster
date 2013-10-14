subroutine vdesga(kwgt, nb1, nb2, depl, btild,&
                  indith, alpha, tempga, epsiln, sigma,&
                  vectt)
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
    implicit none
!
!
#include "jeveux.h"
!
#include "asterfort/jevech.h"
#include "asterfort/matrc.h"
    real(kind=8) :: depl(*), btild(5, 42), matc(5, 5), tempga(*)
    real(kind=8) :: vectt(3, 3)
    real(kind=8) :: epsi(5), epstot(5), sigm(5), epsiln(6, *), sigma(6, *)
    real(kind=8) :: kappa
!
!-----------------------------------------------------------------------
    integer :: i, indith, jcara, k, kwgt
    integer :: nb1, nb2
    real(kind=8) :: alpha, deux
!-----------------------------------------------------------------------
    deux = 2.0d0
!
    do i = 1, 5
        epsi(i)=0.d0
        do k = 1, 5*nb1+2
            epsi(i)=epsi(i)+btild(i,k)*depl(k)
        end do
    end do
!
    epsiln(1,kwgt)=epsi(1)
    epsiln(2,kwgt)=epsi(2)
    epsiln(3,kwgt)=0.d0
    epsiln(4,kwgt)=epsi(3)/deux
    epsiln(5,kwgt)=epsi(4)/deux
    epsiln(6,kwgt)=epsi(5)/deux
!
    call jevech('PCACOQU', 'L', jcara)
    kappa = zr(jcara+3)
!
    call matrc(nb2, kappa, matc, vectt)
!
    if (indith .eq. -1) then
!
!     PAS DE CONTRAINTES THERMIQUES
!
        do i = 1, 5
            sigm(i)=0.d0
            do k = 1, 5
                sigm(i)=sigm(i)+matc(i,k)*epsi(k)
            end do
        end do
!
    else if (indith.eq.0) then
!
!     AVEC CONTRAINTES THERMIQUES
!
!     POUR CELA ON PASSE PAR LES DEFORMATIONS TOTALES
!     AU LIEU DE FAIRE ARIMETHIQUEMENT  SIGMA(ELAS) + SIGMA(THER)
!     CECI EN RAISON DE L'HETEROGENEITE ENTRE DEFORMATION ELASTIQUE
!     ET THERMIQUE (L'UN PEUT ETRE LINEAIE, L'AUTRE QUADRATQUE EN KSI3
!     DEFORMATIONS TOTALES = DEFOR(ELAS) - DEFOR(THER)
!     PUIS CONTRAINTES VRAIES = H * DEFORMATIONS TOTALES
!
        epstot(1)=epsi(1)-alpha*tempga(kwgt)
        epstot(2)=epsi(2)-alpha*tempga(kwgt)
        epstot(3)=epsi(3)
        epstot(4)=epsi(4)
        epstot(5)=epsi(5)
!
        do i = 1, 5
            sigm(i)=0.d0
            do k = 1, 5
                sigm(i)=sigm(i)+matc(i,k)*epstot(k)
            end do
        end do
!
    endif
!
    sigma(1,kwgt)=sigm(1)
    sigma(2,kwgt)=sigm(2)
    sigma(3,kwgt)=0.d0
    sigma(4,kwgt)=sigm(3)
    sigma(5,kwgt)=sigm(4)
    sigma(6,kwgt)=sigm(5)
!
!
end subroutine
