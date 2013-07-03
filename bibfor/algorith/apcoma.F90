subroutine apcoma(sdappa, noma, newgeo, numma, nnosdm,&
                  coorma)
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
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=19) :: sdappa
    character(len=8) :: noma
    character(len=19) :: newgeo
    integer :: numma, nnosdm
    real(kind=8) :: coorma(27)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! COORDONNEES D'UNE MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NOMA   : SD MAILLAGE
! IN  NEWGEO : CHAMP DE GEOMETRIE ACTUALISE
! IN  NUMMA  : NUMERO ABSOLU DE LA MAILLE DANS LE MAILLAGE
! IN  NNOSDM : NOMBRE DE NOEUDS DE LA MAILLE POUR LE CONTACT
! OUT COORMA : COORDONNEES DE LA MAILLE
!
!
!
!
    character(len=24) :: k8bid
!
    integer :: nbnmax
    parameter   (nbnmax = 9)
!
    integer :: no(nbnmax), ino
    integer :: jcoor, i, jdec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 10 i = 1, 27
        coorma(i) = 0.d0
10  end do
!
! --- NOMBRE DE NOEUDS DE LA MAILLE
!
    call assert(nnosdm.gt.0)
    call assert(nnosdm.le.nbnmax)
!
! --- NUMEROS ABSOLUS DES NOEUDS DE LA MAILLE
!
    call jeveuo(jexnum(noma//'.CONNEX', numma ), 'L', jdec)
    do 61 ino = 1, nnosdm
        no(ino) = zi(jdec+ino-1)
61  end do
!
! --- COORDONNEES DES NOEUDS DE LA MAILLE
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jcoor)
    do 70 ino = 1, nnosdm
        coorma(3*(ino-1)+1) = zr(jcoor+3*(no(ino)-1))
        coorma(3*(ino-1)+2) = zr(jcoor+3*(no(ino)-1)+1)
        coorma(3*(ino-1)+3) = zr(jcoor+3*(no(ino)-1)+2)
70  end do
!
    call jedema()
!
end subroutine
