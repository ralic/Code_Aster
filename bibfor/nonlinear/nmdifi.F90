subroutine nmdifi(motfac, iocc, provli, tole, nbinst,&
                  numfin)
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
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/utacli.h"
    character(len=16) :: motfac
    character(len=19) :: provli
    real(kind=8) :: tole
    integer :: numfin, nbinst, iocc
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! DETERMINATION DU NUMERO D'ORDRE FINAL
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLEF FACTEUR
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! IN  PROVLI : NOM DE LA LISTE D'INSTANT PROVISOIRE
! IN  TOLE   : TOLERANCE POUR RECHERCHE DANS LISTE D'INSTANTS
! IN  NBINST : NOMBRE D'INSTANTS DANS LA LISTE
! OUT NUMFIN : NUMERO D'ORDRE FINAL
!
!
!
!
    integer :: n1, n2
    real(kind=8) :: inst
    integer :: jinst
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numfin = 0
!
! --- NOM SD_DISC
!
    call jeveuo(provli, 'L', jinst)
!
! --- LECTURE MOTS-CLEFS
!
    call getvis(motfac, 'NUME_INST_FIN', iocc=iocc, scal=numfin, nbret=n1)
    call getvr8(motfac, 'INST_FIN', iocc=iocc, scal=inst, nbret=n2)
!
! --- PAS D'OCCURENCE DES MOTS-CLES -> NUMERO INITIAL
!
    if (n1+n2 .eq. 0) then
        numfin = nbinst-1
!
! --- MOTS-CLES INST_FIN
!
    else if (n1 .eq. 0) then
        call utacli(inst, zr(jinst), nbinst, tole, numfin)
    endif
!
! --- VERIFICATIONS
!
    if (numfin .lt. 0 .or. numfin .gt. (nbinst-1)) then
        call u2mess('F', 'DISCRETISATION_94')
        numfin = nbinst - 1
    endif
!
    call jedema()
!
end subroutine
