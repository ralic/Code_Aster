subroutine hujcvx(mod, nmat, materf, vinf, deps,&
                  sigd, sigf, seuil, iret)
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
! person_in_charge: alexandre.foucault at edf.fr
    implicit none
!   ------------------------------------------------------------------
!   DEFINITION DU DOMAINE POTENTIEL DES MECANISMES ACTIFS
!   IN  MOD    :  MODELISATION
!       NMAT   :  DIMENSION TABLEAU DONNEES MATERIAU
!       MATERF :  COEFFICIENTS MATERIAU A T+DT
!       VINF   :  VARIABLES INTERNES  A T+DT
!       DEPS   :  INCREMENT DE DEFORMATION
!       SIGD   :  CONTRAINTE A T
!       SIGF   :  CONTRAINTE A T+DT  (ELAS)
!
!   OUT VINF   :  VARIABLES INTERNES MODIFIEES PAR LES NOUVEAUX
!                 MECANISMES
!       SEUIL  :  POSITIF SI PLASTICITE A PRENDRE EN COMPTE
!       IRET   :  CODE RETOUR
!   ------------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/hujpot.h"
    integer :: iret, nmat
    real(kind=8) :: materf(nmat, 2), vinf(*), deps(6), sigd(6), sigf(6)
    real(kind=8) :: seuil
    character(len=8) :: mod
!
    integer :: i
    aster_logical :: rdctps
    character(len=7) :: etatf
    real(kind=8) :: un, bid66(6, 6), zero, somme, matert(22, 2)
!
    parameter     ( zero = 0.d0 )
    parameter     ( un   = 1.d0 )
! ======================================================================
! --- INTIALISATION ETATF
    etatf = 'ELASTIC'
!
! --- CONTROLE DE LA NORME DE DEFORMATION
    somme = zero
    do 10 i = 1, 6
        somme = somme + abs(deps(i))
 10 end do
    if (somme .lt. r8prem()) goto 999
!
    do 20 i = 1, 22
        matert(i,1) = materf(i,1)
        matert(i,2) = materf(i,2)
 20 end do
!
! --- DEFINITION DU DOMAINE POTENTIEL DES MECANISMES ACTIFS
    call hujpot(mod, matert, vinf, deps, sigd,&
                sigf, etatf, rdctps, iret, .true._1)
!
! --- SI ETATF = 'ELASTIC' --> SEUIL < 0
!
999 continue
    if(etatf.eq.'ELASTIC')seuil = - un
!
!
end subroutine
