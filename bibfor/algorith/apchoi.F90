subroutine apchoi(dist, distm, posmai, posmam, tau1,&
                  tau1m, tau2, tau2m, ksi1, ksi1m,&
                  ksi2, ksi2m, iproj, iprojm, vect,&
                  vectm)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "asterc/r8prem.h"
#include "asterfort/infdbg.h"
#include "blas/dcopy.h"
    integer :: iproj, iprojm
    real(kind=8) :: dist, distm
    integer :: posmai, posmam
    real(kind=8) :: tau1(3), tau1m(3)
    real(kind=8) :: tau2(3), tau2m(3)
    real(kind=8) :: vect(3), vectm(3)
    real(kind=8) :: ksi1, ksi1m
    real(kind=8) :: ksi2, ksi2m
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (ALGO)
!
! CHOIX DE LA MAILLE LA PLUS PROCHE
!
! ----------------------------------------------------------------------
!
!
! IN  DIST   : DISTANCE COURANTE
! I/O DISTM  : DISTANCE MINIMALE
! IN  POSMAI : POSITION DE LA MAILLE MAITRE APPARIEE
! I/O POSMAM : POSITION DE LA MAILLE MAITRE CORRESPONDANT A LA
!               DISTANCE MINIMALE
! IN  IPROJ  : VAUT 0 SI POINT PROJETE DANS L'ELEMENT
!                   1 SI POINT PROJETE DANS LA ZONE DEFINIE PAR TOLEOU
!                   2 SI POINT PROJETE EN DEHORS (EXCLUS)
! I/O IPROJM : TYPE DE PROJECTION CORRESPONDANT AU DIST MINIMUM
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! I/O TAU1M  : PREMIER VECTEUR TANGENT CORRESPONDANT A LA DISTANCE
!              MINIMALE
! I/O TAU2M  : SECOND VECTEUR TANGENT CORRESPONDANT A LA DISTANCE
!              MINIMALE
! IN  KSI1   : COORD. PARAM. 1 DE LA PROJECTION SUR MAILLE MAITRE
! IN  KSI2   : COORD. PARAM. 2 DE LA PROJECTION SUR MAILLE MAITRE
! I/O KSI1M  : COORD. PARAM. 1 DE LA PROJECTION SUR MAILLE MAITRE
!               CORRESPONDANT A LA DISTANCE MINIMALE
! I/O KSI2M  : COORD. PARAM. 2 DE LA PROJECTION SUR MAILLE MAITRE
!               CORRESPONDANT A LA DISTANCE MINIMALE
! IN  VECT   : VECTEUR E->M COURANT
! OUT VECTM  : VECTEUR E->M (CORRESPONDANT AU JEU MINIMUM)
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8) :: ecan
!
! ----------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ECART ANCIEN/NOUVEAU
!
    if ((distm.eq.0.d0) .or. (dist.eq.distm)) then
        ecan = 0.d0
    else
        ecan = abs((dist - distm)/distm)
    endif
!
!
!
! PREMIERE PROJECTION
    if (iprojm .eq. -1) then
        distm = dist
        posmam = posmai
        iprojm = iproj
        call dcopy(3, tau1, 1, tau1m, 1)
        call dcopy(3, tau2, 1, tau2m, 1)
        call dcopy(3, vect, 1, vectm, 1)
        ksi1m = ksi1
        ksi2m = ksi2
    else
! PROJECTION SUIVANTES
        if (iprojm .ne. 0) then
! ON N'A PAS ENCORE PROJETE DANS UN ELEMENT
            if (iproj .eq. 0) then
! UN POINT SE PROJETE DANS UN ELEMENT
                distm = dist
                posmam = posmai
                iprojm = iproj
                call dcopy(3, tau1, 1, tau1m, 1)
                call dcopy(3, tau2, 1, tau2m, 1)
                call dcopy(3, vect, 1, vectm, 1)
                ksi1m = ksi1
                ksi2m = ksi2
            else
                if (dist .lt. distm) then
                    distm = dist
                    posmam = posmai
                    iprojm = iproj
                    call dcopy(3, tau1, 1, tau1m, 1)
                    call dcopy(3, tau2, 1, tau2m, 1)
                    call dcopy(3, vect, 1, vectm, 1)
                    ksi1m = ksi1
                    ksi2m = ksi2
                endif
            endif
        else
! ON A DEJA PROJETE DANS UN ELEMENT
            if (iproj .eq. 0) then
                if (dist .lt. distm .and. ecan .gt. r8prem()) then
! ON SELECTIONNE LE PROJETE MINIMISANT LA DISTANCE
                    distm = dist
                    posmam = posmai
                    iprojm = iproj
                    call dcopy(3, tau1, 1, tau1m, 1)
                    call dcopy(3, tau2, 1, tau2m, 1)
                    call dcopy(3, vect, 1, vectm, 1)
                    ksi1m = ksi1
                    ksi2m = ksi2
!
                endif
            endif
        endif
    endif
!
end subroutine
