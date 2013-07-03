subroutine nmcrcv(sdcrit)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sdcrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! OUT SDCRIT : SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!                (1) NOMBRE ITERATIONS NEWTON
!                (2) NOMBRE ITERATIONS RECHERCHE LINEAIRE
!                (3) RESI_GLOB_RELA
!                (4) RESI_GLOB_MAXI
!                (5) PARAMETRE DE PILOTAGE ETA
!                (6) CHARGEMENT EXTERIEUR
!                (9) RESI_COMP_RELA
!
! ----------------------------------------------------------------------
!
    integer :: jcrr, jcrk
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION
!
    call wkvect(sdcrit(1:19)//'.CRTR', 'V V R8', 9, jcrr)
    call wkvect(sdcrit(1:19)//'.CRDE', 'V V K16', 9, jcrk)
    zk16(jcrk+1-1) = 'ITER_GLOB'
    zk16(jcrk+2-1) = 'ITER_LINE'
    zk16(jcrk+3-1) = 'RESI_GLOB_RELA'
    zk16(jcrk+4-1) = 'RESI_GLOB'
    zk16(jcrk+5-1) = 'ETA_PILOTAGE'
    zk16(jcrk+6-1) = 'CHAR_MINI'
    zk16(jcrk+7-1) = 'RESI_GLOB_MOINS'
    zk16(jcrk+8-1) = 'RESI_REFE'
    zk16(jcrk+9-1) = 'RESI_COMP'
!
    call jedema()
!
end subroutine
