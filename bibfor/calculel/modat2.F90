function modat2(iopt, ite, nompar)
    implicit none
    integer :: modat2
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
!
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jelira.h"
    integer :: iopt, ite
    character(len=8) :: nompar
! ----------------------------------------------------------------------
! BUT : TROUVER LE NUMERO DU MODE LOCAL ASSOCIE A UN PARAMETRE
!       D'UNE OPTION POUR UN TYPE_ELEM DONNE.
!
! ARGUMENTS :
!  IOPT    IN    I    : NUMERO DE L'OPTION DE CALCUL
!  ITE     IN    I    : NUMERO  DU TYPE_ELEM
!  NOMPAR  IN    K8   : NOM DU PARAMETRE POUR L'OPTION
!  MODAT2  OUT   I    : NUMERO DU MODE_LOCAL TROUVE DANS LE
!                       CATALOGUE DU TYPE_ELEM
!                       = 0 SI LE TYPE_ELEM NE CONNAIT PAS
!                       L'OPTION OU NE CONNAIT PAS LE PARAMETRE.
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: iopte, lgco, n1
    integer :: nucalc, nbpar, k, joptmo, joptno
    integer, pointer :: nbligcol(:) => null()
    integer, pointer :: optte(:) => null()
! ----------------------------------------------------------------------

    modat2 = 0

    call jeveuo('&CATA.TE.OPTTE', 'L', vi=optte)
    call jeveuo('&CATA.TE.NBLIGCOL', 'L', vi=nbligcol)
    lgco = nbligcol(1)
    iopte = optte((ite-1)*lgco+iopt)


    if (iopte .eq. 0) goto 20

    call jeveuo(jexnum('&CATA.TE.OPTMOD', iopte), 'L', joptmo)
    nucalc = zi(joptmo-1+1)
    if (nucalc .le. 0) goto 20

    call jeveuo(jexnum('&CATA.TE.OPTNOM', iopte), 'L', joptno)
    nbpar = zi(joptmo-1+2) + zi(joptmo-1+3)
    do k = 1,nbpar
        if (nompar .ne. zk8(joptno-1+k)) cycle
        modat2 = zi(joptmo-1+3+k)
    end do

!
20  continue
end function
