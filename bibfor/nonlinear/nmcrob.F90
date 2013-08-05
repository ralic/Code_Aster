subroutine nmcrob(noma, nomo, result, numreo, sdieto,&
                  sdobse)
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
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcroi.h"
#include "asterfort/nmcrot.h"
#include "asterfort/nmextr.h"
#include "asterfort/nmobno.h"
#include "asterfort/u2mesi.h"
    character(len=8) :: result, noma, nomo
    integer :: numreo
    character(len=19) :: sdobse
    character(len=24) :: sdieto
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION SD OBSERVATION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
! IN  RESULT : NOM UTILISATEUR DU RESULTAT
! IN  NUMREO : NUMERO DE REUSE POUR LA TABLE OBSERVATION
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: ntobs, nbocc
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<    NONLINE> ... CREATION SD OBSERVATION'
    endif
!
! --- INITIALISATIONS
!
    ntobs = 0
    motfac = 'OBSERVATION'
!
! --- NOMBRE OCCURRENCES
!
    call getfac(motfac, nbocc)
    ASSERT(nbocc.le.99)
!
! --- LECTURE DES DONNEES
!
    call nmextr(noma, nomo, sdobse, sdieto, motfac,&
                nbocc, numreo, ntobs)
!
! --- CONTROLE
!
    if (ntobs .ne. 0) then
        call u2mesi('I', 'OBSERVATION_3', 1, ntobs)
    else
        goto 999
    endif
!
! --- LECTURE LISTES INSTANTS
!
    call nmcroi(sdobse, motfac, nbocc)
!
! --- NOM DES COLONNES
!
    call nmobno(sdobse, motfac, nbocc)
!
! --- CREATION DE LA TABLE D'OBSERVATION
!
    call nmcrot(result, sdobse)
!
999  continue
!
    call jedema()
!
end subroutine
