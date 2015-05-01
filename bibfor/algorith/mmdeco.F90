subroutine mmdeco(defico, resoco)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONSTINUE - POST-TRAITEMENT)
!
! GESTION DE LA DECOUPE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    character(len=24) :: tabfin, etatct
    integer :: jtabf, jetat
    integer :: zetat, ztabf
    integer :: ntpc, ipc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    etatct = resoco(1:14)//'.ETATCT'
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    call jeveuo(etatct, 'E', jetat)
    ztabf = cfmmvd('ZTABF')
    zetat = cfmmvd('ZETAT')
!
! --- INITIALISATION
!
    ntpc = cfdisi(defico,'NTPC')
!
! --- SAUVEGARDE DE L ETAT DE CONTACT EN CAS DE REDECOUPAGE
!
    do 1000 ipc = 1, ntpc
!       STATUT DE CONTACT
        zr(jetat-1+zetat*(ipc-1)+1) = zr(jtabf+ztabf*(ipc-1)+22)
!       SEUIL DE FROTTEMENT
        zr(jetat-1+zetat*(ipc-1)+2) = zr(jtabf+ztabf*(ipc-1)+16)
!       MEMOIRE DE GLISSIERE
        zr(jetat-1+zetat*(ipc-1)+3) = zr(jtabf+ztabf*(ipc-1)+17)
1000  end do
!
    call jedema()
!
end subroutine
