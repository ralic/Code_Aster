subroutine initia(neq, lgrot, indro, chamro, chamin)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
    logical(kind=1) :: lgrot
    integer :: neq, indro(*)
    real(kind=8) :: chamro(*), chamin(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - UTILITAIRE)
!
! INITIALISE UN CHAM_NO EN TENANT COMPTE DES GRANDES ROTATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  NEQ    : LONGUEUR DES CHAM_NO
! IN  LGROT  : TRUE  S'IL Y A DES DDL DE GRDE ROTATION
!                       FALSE SINON
! IN  INDRO  : VECTEUR DONNANT LE TYPE DES DDL:
!                 0: TRANSLATION OU PETITE ROTATION
!                 1: GRANDE ROTATION
! IN  CHAMRO  : CHAM_NO DONNE
! OUT CHAMIN  : CHAM_NO INITIALISE
!
!    SI LGROT=FALSE:  ZERO
!    SI LGROT=TRUE :  ZERO POUR LES DDL DE TRANSLATION OU DE
!                      PETITE ROTATION
!                      LA VALEUR DE MME RANG EXTRAITE DE CHAMRO
!                      POUR LES DDL DE GRANDE ROTATION
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: zero
    integer :: i
!
! ----------------------------------------------------------------------
!
    zero = 0.d0
    if (.not.lgrot) then
        do 10 i = 1, neq
            chamin(i) = zero
10      continue
    else
        do 20 i = 1, neq
            if (indro(i) .eq. 0) then
                chamin(i) = zero
            else if (indro(i).eq.1) then
                chamin(i) = chamro(i)
            else
                ASSERT(.false.)
            endif
20      continue
    endif
end subroutine
