subroutine nmgrot(iran, deldet, theta, chamaj)
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
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/proqua.h"
#include "asterfort/quavro.h"
#include "asterfort/vroqua.h"
    real(kind=8) :: theta(3), deldet(3)
    integer :: iran(3)
    real(kind=8) :: chamaj(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - UTILITAIRE - DYNAMIQUE)
!
! MISE A JOUR DES DEPLACEMENTS EN GRANDES ROTATIONS
! POUR POU_D_GD
!
! ----------------------------------------------------------------------
!
!
! IN  POUGD  : VARIABLE CHAPEAU POUR POUTRES EN GRANDES ROTATIONS
! IN  THETA  : VALEUR DE LA ROTATION PRECEDENTE
! IN  IRAN   : NUMEROS ABSOLUS D'EQUATION DES DDL DE ROTATION DANS LES
!                 CHAM_NO
! OUT CHAMAJ : CHAMP MISE A JOUR
!
!
!
!
    integer :: ic
    real(kind=8) :: quapro(4), quarot(4), delqua(4)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!
! --- QUATERNION DE LA ROTATION PRECEDENTE
!
    call vroqua(theta, quarot)
!
! --- QUATERNION DE L'INCREMENT DE ROTATION
!
    call vroqua(deldet, delqua)
!
! --- CALCUL DE LA NOUVELLE ROTATION
!
    call proqua(delqua, quarot, quapro)
    call quavro(quapro, theta)
!
! --- MISE A JOUR DE LA ROTATION
!
    do 15 ic = 1, 3
        chamaj(iran(ic)) = theta (ic)
15  end do
!
!
    call jedema()
end subroutine
