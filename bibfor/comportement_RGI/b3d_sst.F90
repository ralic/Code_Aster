subroutine b3d_sst(ss6, istep, vsige33, vsige33t, sigit3)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     actualisation des contraintes seuils pour  l endo de traction
!     declarations externes
!=====================================================================
    implicit none
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
#include "asterfort/indice1.h"
#include "asterfort/x33x6.h"

    real(kind=8) :: ss6(6)
    integer :: istep
    real(kind=8) :: vsige33(3, 3), vsige33t(3, 3)
!     variables locales
    real(kind=8) :: ss33(3, 3), ss133(3, 3)
    integer :: i, k, l
    real(kind=8) :: deltas, sigeq
    real(kind=8) :: sigit3(3)
!     la prise en compte des covs est deja traite avant b3d_sst, on ne t
!     rangement des seuils d'endommagements du pas precedent en tableau
    call x6x33 (ss6,ss33)
!     expression des contraintes seuils dans la base principales des con
!     passage ds la base des contraintes principales pour actualisation
    call b3d_chrep(ss133, ss33, vsige33)
    do i = 1, 3
        sigeq=sigit3(i)
        if (sigeq .gt. ss133(i,i)) then
!            increment de contrainte normal (toujours positif)
            deltas=sigeq-ss133(i,i)
!            actualisation de la composante normale de la contrainte seu
            ss133(i,i)=sigeq
!            increment de rotation par diminution de la contrainte de ci
!            dans la base principale actuelle des contraintes effectives
            call indice1(i,k,l)
            if (ss133(i,k) .le. 0.d0) then
                ss133(i,k)=min(ss133(i,k)+deltas,0.d0)
            else
                ss133(i,k)=max(ss133(i,k)-deltas,0.d0)
                end if
                if (ss133(i,l) .le. 0.d0) then
                    ss133(i,l)=min(ss133(i,l)+deltas,0.d0)
                else
                    ss133(i,l)=max(ss133(i,l)-deltas,0.d0)
                    end if
!            symetrisation du tenseur
!            ss133(i,l)=0.d0
!            ss133(i,k)=0.d0
                    ss133(l,i)=ss133(i,l)
                    ss133(k,i)=ss133(i,k)
                    end if
                    end do
!     retour des contraintes seuils dans la base fixe
                    call b3d_chrep(ss33, ss133, vsige33t)
!     retour en vecteur 6 des contraintes seuils
      call x33x6(ss33,ss6)
end subroutine
