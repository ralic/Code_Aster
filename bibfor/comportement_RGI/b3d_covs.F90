subroutine b3d_covs(covs, vsige33, vsige33t, siget3, sigec3,&
                    sigi6, sigit3, sigic3)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!
!     prise en compte des autocontraintes induites par les  inclusions
!===================================================================
    implicit none
!
#include "asterfort/x33x6.h"
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
    integer :: i, j
    real(kind=8) :: sige6(6), covs, sigi6(6), vsige33(3, 3)
    real(kind=8) :: vsige33t(3, 3), siget3(3), sigec3(3)
!
    real(kind=8) :: l66(6, 6), lm166(6, 6)
    real(kind=8) :: sigic3(3), sigit3(3)
    real(kind=8) :: sigi133(3, 3), sigi33(3, 3), deltas
!
    if (covs .ne. 0.) then
!      cas ou cov n est pas nul : calcul des contraintes modifiees
!      par les autocontraintes
!      prise en compte des covs anisotropes / J.Armengaud juin 2013
!      initialisation de la matrice l66 a modififier si necessaire
!      L est defini en BASE FIXE (I.E. BASE DU MAILLAGE)
        do i = 1, 6
            do j = 1, 6
                if (i .ne. j) then
                    l66(i,j)=0.d0
                else
                    l66(i,j)=1.d0
                end if
                if (i .le. 3) then
                    if (j .le. 3) then
                        if (i .ne. j) then
                            l66(i,j)=-1.d0
                        end if
                    end if
                end if
            end do
        end do
!      on introduit ici les modification eventuelles de la matrice
!      de localisation des contraintes moyenne sur les inclusions
!      l66(i,j)=...
!
!      initialisation de la matrice covs*(L-I), (L-I) contient
!      la variation moyenne de la contrainte au bord de l'inclusion
        do i = 1, 6
            do j = 1, 6
                if (i .ne. j) then
                    lm166(i,j)=covs*l66(i,j)
                else
                    lm166(i,i)=covs*(l66(i,i)-1.d0)
                end if
            end do
        end do
!
!      calcul des contraintes totales au bord des inclusions
!
!      reconstruction du tenseur des contraintes effectives
        do i = 1, 6
            if (i .le. 3) then
                sige6(i)=siget3(i)+sigec3(i)
            else
                sige6(6)=0.d0
            end if
        end do
!
!      on se place en base fixe ca L est en base fixe
        call x6x33(sige6, sigi33)
        call b3d_chrep(sigi133, sigi33, vsige33)
        call x33x6(sigi133, sige6)
        do i = 1, 6
            sigi6(i)=sige6(i)
            do j = 1, 6
                sigi6(i)=sigi6(i)+lm166(i,j)*sige6(j)
            end do
        end do
!
!      passage des contraintes induites dans la base principale
!      des contraintes principales
        call x6x33(sigi6, sigi33)
        call b3d_chrep(sigi133, sigi33, vsige33t)
        call x33x6(sigi133, sigi6)
        do i = 1, 3
!       on choisit systementiqquement la partie defavorable de l'autocontrainte resultante
!       de façon a ne jamais etre inferieur a la contrainte moyenne
            deltas=abs(sigi6(i)-(siget3(i)+sigec3(i)))
            sigi6(i)=(siget3(i)+sigec3(i))+deltas
!       partie positive
            sigit3(i)=0.5d0*(sigi6(i)+abs(sigi6(i)))
!       partie complementaire
            sigic3(i)=sigi6(i)-sigit3(i)
        end do
    else
!      cas ou covs est nul
        do i = 1, 6
            if (i .le. 3) then
                sigit3(i)=siget3(i)
                sigic3(i)=sigec3(i)
                sigi6(i)=sigit3(i)+sigic3(i)
            else
                sigi6(i)=0.d0
            end if
        end do
    end if
end subroutine
