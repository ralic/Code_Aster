subroutine b3d_sdif(ss6, young0, rt, epic, erreur,&
                    dt3, st3, vss33, vss33t, rapp3)
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
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!     endo diffus de la forme d=s/(r+s)
!=====================================================================
!     declarations externes
    implicit none
#include "asterfort/b3d_valp33.h"
#include "asterfort/x6x33.h"
#include "asterfort/transpos1.h"
        real(kind=8) :: ss6(6)
        real(kind=8) :: young0
        real(kind=8) :: rt
        real(kind=8) :: epic
        integer :: erreur
        real(kind=8) :: dt3(3)
        real(kind=8) :: st3(3)
        real(kind=8) :: vss33(3, 3)
        real(kind=8) :: vss33t(3, 3)
        real(kind=8) :: rapp3(3)
    real(kind=8) ::  ss33(3, 3), ss3(3),rt0
    integer :: i,j,k
!
!     diagonalisation contraintes seuils actuelles et valeurs
!     propres par la methode de jacobi
      call x6x33(ss6,ss33)
    call b3d_valp33(ss33, ss3, vss33)
!     creation de la matrice de passage inverse
      call transpos1(vss33t,vss33,3)
!
!     resistance effective au pic de traction
    rt0=young0*epic
!     endommagement du au franchissement du pic
!     independant de la taille de l element car endo diffus
!     (ecrouissage post pic nul)
    do i = 1, 3
        if (ss3(i) .gt. 0.d0) then
            dt3(i)=ss3(i)/(rt0+ss3(i))
!         dt3(i)=0.d0
!         print*,'Ds b3d_sdiffd diff(',i,')=',dt3(i)
        else
            dt3(i)=0.d0
            end if
!       resistance residuelle localisee apres endommagement diffus
            rapp3(i)=rt*(1.d0-dt3(i))
            end do
!     verif de la condition de croissance des endos inutile car
!     endo local ne depend pas de la taille des elements
!     calcul des indice de fissuration
            do i = 1, 3
                if (dt3(i) .lt. 1.d0) then
                    st3(i)=1.d0/(1.d0-dt3(i))
                else
                    print*,'dt3==1 ds b3d_sdif',dt3(i)
                    erreur=1
                    end if
                    end do
end subroutine
