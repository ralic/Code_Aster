subroutine xhlan5(ino, idepl, idepm, idep0, lact,&
                  ndim, pla, lamb, nvec, champ, job, dpf)
    implicit none
#include "asterfort/vecini.h"
#include "asterfort/assert.h"
    integer :: ino
#include "jeveux.h"
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Routine utilitaire de calcul d'un champ
!
! IN INO    : NUMERO DU NOEUD
! IN IDEPL  : ADRESSE INCREMENT PRECEDANT
! IN IDEPM  : ADRESSE DEPLACEMENT INSTANT -
! IN IDEP0  : ADRESSE INCREMENT ACTUEL
! IN LACT   : DDL DE LAGRANGE ACTIF OU NON
! IN NDIM   : DIMENSION DU MODELE
! IN PLA    : PLACE DES DDLS DE LAGRANGE
! OUT LAMB  : CHAMP DEMANDE
! IN NVEC   : NOMBRE D ADRESSES DEPLACEMENT: 1 OU 2
! IN CHAMP  : NOM DU CHAMP: LAMBDA, MU OU W
    integer :: idepl, idepm, idep0
    integer :: j, lact(16), ndim, nli
    integer :: pla(27), pli, nvec, indcha
    real(kind=8) :: lamb(3), dpf
    character(len=8) :: champ, job
!
! --- RÉACTION CONTACT = SOMME DES FF(I).LAMBDA(I) POUR I=1,NNOL
! --- RÉACTION FROTT = SOMME DES FF(I).(LAMB1(I).TAU1+LAMB2(I).TAU2)
! --- (DEPDEL+DEPMOI)
    if(champ.eq.'LAMBDA') then
        indcha = 0
    else if(champ(1:1).eq.'W') then
        indcha = 1
    else if(champ.eq.'MU') then
        indcha = 2
    else
        ASSERT(.false.)
    endif
    call vecini(3, 0.d0, lamb)
    pli=pla(ino)
    nli=lact(ino)
!
    if (nli .eq. 0) goto 1
    do j = 1, ndim
       if (champ(2:2).ne.'M') then
          lamb(j) = lamb(j) + zr(idepl-1+pli+2+indcha*ndim+j)
       endif
       if (champ(2:2).eq.'M'.and.nvec.eq.1) then
          lamb(j) = lamb(j) + zr(idepm-1+pli+2+indcha*ndim+j)
       endif
       if (nvec .ge. 2) then
          lamb(j) = lamb(j) + zr(idepm-1+pli+2+indcha*ndim+j)
       endif
       if (nvec .ge. 3) then
          lamb(j) = lamb(j) + zr(idep0-1+pli+2+indcha*ndim+j)
       endif
    end do
 1  continue
    dpf = zr(idepl-1+pli)
    if (job.eq.'ACTU_VI') then
       dpf = dpf - zr(idepm-1+pli)
    endif
end subroutine
