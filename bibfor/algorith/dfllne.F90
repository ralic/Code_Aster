subroutine dfllne(mcfact, nechec, nerreu)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
    character(len=16) :: mcfact
    integer :: nechec, nerreu
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES ECHECS
!
! DECOMPTE DES OCCURRENCES MOT-CLEF ECHEC
!
! ----------------------------------------------------------------------
!
! IN  MCFACT : MOT-CLEF FACTEUR POUR LIRE L'ECHEC
! OUT NECHEC : NB OCCURRENCES MOT-CLEF ECHEC
! OUT NERREU : NB OCCURRENCES EVENEMENT ='ERREUR'
!
!
!
!
    integer :: ibid
    integer :: iechec
    character(len=16) :: even
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nerreu = 0
!
! --- NECHEC: NOMBRE D'OCCURRENCES DU MOT-CLE FACTEUR 'ECHEC'
!
    call getfac(mcfact, nechec)
!
! --- NOMBRE D'OCCURRENCES DES EVENEMENTS
!
    do 100 iechec = 1, nechec
        call getvtx(mcfact, 'EVENEMENT', iocc=iechec, scal=even, nbret=ibid)
        if (even .eq. 'ERREUR') nerreu = nerreu+1
100  end do
!
! --- ON NE PEUT DEFINIR QU'UNE SEULE OCCURRENCE AVEC 'ERREUR'
!
    if (nerreu .gt. 1) call u2mess('F', 'DISCRETISATION_10')
!
    call jedema()
end subroutine
