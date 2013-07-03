function comcou(iarg)
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
! ----------------------------------------------------------------------
!
! ROUTINE PERMETTANT DE RECUPERE SOIT LE COMMUNICATEUR ORIGINEL (IRET=0)
! SOIT LE COMMUNICATEUR COURANT (IRET=1).
! ILS SONT STOCKES DANS L'OBJET JEVEUX SUR LA BASE GLOBALE:
! 'COMMUNICATEUR_MPI.REFE'.
! EN CAS DE PB, CALL ASSERT(.FALSE.) (SAUF LA PREMIERE FOIS OU ON CREE
! L'OBJET)
! ----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: comcou
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpiexe.h"
    integer :: jco, iarg
    character(len=24) :: k24bid
!
    call jemarq()
    comcou=-9999
    k24bid='COMMUNICATEUR_MPI.REFE'
    call jeveuo(k24bid, 'L', jco)
    comcou = int(zi(jco+iarg), 4)
    call jedema()
!
end function
