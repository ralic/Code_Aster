subroutine zbroot(mem, rhonew, echec)
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
    implicit none
#include "asterf_types.h"
#include "asterc/r8prem.h"
    real(kind=8) :: mem(2, *), rhonew
    aster_logical :: echec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
!
! RESOLUTION F(X) = 0 : ITERATION COURANTE
!
! ----------------------------------------------------------------------
!
!
!  IN  MEM   : COUPLES (X,F) ANTERIEURS
!  OUT ECHEC : .TRUE. SI LA RECHERCHE DE RACINE A ECHOUE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: rhoneg, rhopos
    real(kind=8) :: parmul, fneg, fpos
    integer :: dimcpl, nbcpl
    aster_logical :: bpos, lopti
    common /zbpar/ rhoneg,rhopos,&
     &               parmul,fneg  ,fpos  ,&
     &               dimcpl,nbcpl ,bpos  ,lopti
!
    real(kind=8) :: x0, x1, f0, f1, p0, p1
!
! ----------------------------------------------------------------------
!
    echec = .false.
!
    if (nbcpl .ge. 2) then
        x1 = mem(1,1)
        x0 = mem(1,2)
        f1 = mem(2,1)
        f0 = mem(2,2)
        if (abs(f1) .ge. abs(f0)) then
! -- EN CAS DE NON PERTINENCE DES ITERES : DICHOTOMIE
            rhonew = 0.5d0 * (rhoneg + rhopos)
            goto 9999
        else
! -- INTERPOLATION LINEAIRE
            if (abs(x1-x0) .lt. r8prem()) then
                echec = .true.
                goto 9999
            endif
            p1 = (f1-f0)/(x1-x0)
            p0 = f0 - p1*x0
!
            if (abs(p1) .le. abs(f0)/(rhopos+x0)) then
                rhonew = 0.5d0 * (rhoneg + rhopos)
            else
                rhonew = -p0/p1
            endif
        endif
!      DO 5000 I = 1,NBCPL
!        WRITE (6,5010) I,MEM(1,I),MEM(2,I)
! 5000 CONTINUE
! 5010 FORMAT('RL DBG : ',I4,2X,G22.15,2X,G22.15)
!
    endif
!
9999 continue
!
!
end subroutine
