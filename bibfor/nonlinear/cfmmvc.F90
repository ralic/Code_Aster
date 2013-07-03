subroutine cfmmvc(defico, jeux, loca, enti, zone,&
                  npt)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=24) :: defico
    character(len=24) :: jeux, loca, enti, zone
    integer :: npt
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE VERIF)
!
! CREATION SD PROVISOIRE POUR MODE VERIF EN POST-TRAITEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
! IN  ENTI   : NOM DE LA SD STOCKANT LES NOMS DES ENTITES APPARIEES
! IN  ZONE   : NOM DE LA SD STOCKANT LA ZONE A LAQUELLE APPARTIENT LE
!              POINT
! IN  LOCA   : NUMERO DU NOEUD POUR LE POINT DE CONTACT (-1 SI LE POINT
!              N'EST PAS UN NOEUD ! )
! OUT NPT    : NOMBRE DE POINTS EN MODE VERIF
!
!
!
!
    integer :: ntpt, ntpc
    integer :: jjeux, jloca, jenti, jzone
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- QUELQUES DIMENSIONS
!
    ntpt = cfdisi(defico,'NTPT' )
    ntpc = cfdisi(defico,'NTPC' )
    npt = ntpt-ntpc
    call assert(npt.ge.1)
!
! --- CREATION SD PROVISOIRES
!
    jeux = '&&CFMMVC.JEUX'
    loca = '&&CFMMVC.LOCA'
    enti = '&&CFMMVC.ENTI'
    zone = '&&CFMMVC.ZONE'
    call wkvect(jeux, 'V V R', npt, jjeux)
    call wkvect(loca, 'V V I', npt, jloca)
    call wkvect(enti, 'V V K16', npt*2, jenti)
    call wkvect(zone, 'V V I', npt, jzone)
!
    call jedema()
end subroutine
