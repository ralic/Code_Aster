subroutine op0098()
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!  P. RICHARD   DATE 09/07/91
!-----------------------------------------------------------------------
!  BUT : OPERATEUR DE DEFINITION DE LISTE INTERFACE POUR SUPERPOSITION
!        OU SYNTHESE MODALE : DEFI_INTERF_DYNA
!        LISTE_INTERFACE CLASSIQUE ( MIXTE CRAIG-BAMPTON MAC-NEAL)
!-----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/calc98.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/imbint.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres, mailla, nomgd
    character(len=19) :: numddl
    character(len=16) :: nomope, nomcon
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! --- PHASE DE VERIFICATION
!
!-----------------------------------------------------------------------
    integer :: iadref, ifm, lddesc, nbcmp
    integer :: nbec, nbid, niv, numgd
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
! --- RECUPERATION NOM ARGUMENT
!
    call getres(nomres, nomcon, nomope)
!
! --- CREATION .REFE
!
    call getvid('   ', 'NUME_DDL', iocc=1, scal=numddl, nbret=nbid)
    numddl(15:19)='.NUME'
    call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
    call wkvect(nomres//'.IDC_REFE', 'G V K24', 3, iadref)
    zk24(iadref)=mailla
    zk24(iadref+1)=numddl
    zk24(iadref+2)='              '
!
! --- CREATION DU .DESC
!
    call dismoi('NOM_GD', numddl, 'NUME_DDL', repk=nomgd)
    call dismoi('NUM_GD', nomgd, 'GRANDEUR', repi=numgd)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=nbcmp)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
!
    call wkvect(nomres//'.IDC_DESC', 'G V I', 5, lddesc)
    zi(lddesc)=1
    zi(lddesc+1)=nbec
    zi(lddesc+2)=nbcmp
    zi(lddesc+3)=numgd
    zi(lddesc+4)=0
!
! CETTE DERNIERE VALEUR SERA REACTUALISEE PAR LE NOMBRE DE DEFORMEE
! STATIQUE A CALCULER
!
! --- CAS D'UNE LISTE_INTERFACE CONNUE
!
    call calc98(nomres, mailla, numddl)
!
! --- IMPRESSION SUR FICHIER
!
    if (niv .gt. 1) call imbint(nomres, ifm)
!
    call jedema()
end subroutine
