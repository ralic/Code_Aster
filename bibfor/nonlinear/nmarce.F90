subroutine nmarce(sdieto, result, sdimpr, sddisc, instan,&
                  numarc, force)
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
#include "asterfort/jeveuo.h"
#include "asterfort/nmeteo.h"
    character(len=24) :: sdieto, sdimpr
    character(len=8) :: result
    character(len=19) :: sddisc
    integer :: numarc
    real(kind=8) :: instan
    logical :: force
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - ARCHIVAGE )
!
! ARCHIVAGE DES CHAMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDIMPR : SD AFFICHAGE
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  FORCE  : VRAI SI ON SOUHAITE FORCER L'ARCHIVAGE DE TOUS LES CHAMPS
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  INSTAN : INSTANT D'ARCHIVAGE
! IN  NUMARC : NUMERO D'ARCHIVAGE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: ioinfo
    integer :: jioinf
    integer :: nbcham
    integer :: icham
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    call jeveuo(ioinfo, 'L', jioinf)
    nbcham = zi(jioinf+1-1)
!
! --- BOUCLE SUR LES CHAMPS
!
    do 10 icham = 1, nbcham
        call nmeteo(result, sdimpr, sddisc, sdieto, force,&
                    numarc, instan, icham)
10  end do
!
    call jedema()
end subroutine
