subroutine nmcrpx(motfaz, motpaz, iocc, nomsd, base)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcrpa.h"
#include "asterfort/nmcrpp.h"
#include "asterfort/wkvect.h"
    character(len=*) :: motfaz, motpaz
    character(len=1) :: base
    integer :: iocc
    character(len=19) :: nomsd
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
!
! LECTURE DES INFORMATIONS DANS CATAPY POUR LES MOTS-CLEFS
! DE TYPE SELECTION D'INSTANTS
!
! ----------------------------------------------------------------------
!
!
! CETTE ROUTINE LIT DES ARGUMENTS DE TYPE SELECTION D'INSTANTS
!  L'UTILISATEUR DONNE SES INSTANTS DE TROIS MANIERES DIFFERENTES
!
!    1/ LISTE D'INSTANTS DONNEE PAR MOT-CLEF <LIST_INST>
!         LA LISTE AYANT ETE CREEE PAR DEFI_LIST_REEL
!    2/ LISTE D'INSTANTS DONNEE PAR MOT-CLEF <LIST>
!         LA LISTE AYANT ETE CREEE PAR UNE LISTE PYTHON (LIST_R8)
!    3/ FREQUENCE DES INSTANTS DONNEE PAR MOT-CLEF <PAS_*>
!         LA LISTE AYANT ETE CREEE PAR UNE LISTE PYTHON (LIST_R8)
!
! NB: SI PAS DE LISTE NI DE FREQUENCE DONNEES, PAR DEFAUT, PAS = 1
!
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE <LIST_INST/INST>
!               SI MOTFAC= ' ' -> ON NE LIT RIEN ET ON PREND DES
!               VALEURS PAR DEFAUT
!               FREQ = 1
! IN  MOTPAS : MOT-FACTEUR POUR LIRE <PAS>
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! IN  NOMSD  : NOM DE LA STRUCTURE DE DONNEES PRODUITE
!     ON VA CREER DEUX OBJETS :
!         NOMSD(1:19)//'.INFL' -  VECTEUR DE R8 DE LONGUEUR 4
!            1 - FREQUENCE (0 SI LISTE)
!            2 - TOLERANCE RECHERCHE (<0 SI ABSOLU,
!                                     >0 SI RELATIF)
!            3 - NOMBRE D'INSTANTS DE LA LISTE (NBINST)
!            4 - VALEUR MINI. ENTRE DEUX INSTANTS
!         NOMSD(1:19)//'.LIST' - VECTEUR DE R8 DE LONGUEUR NBINST
!            LISTE DES INSTANTS
! IN  BASE   : NOM DE LA BASE POUR LA CREATION SD
!
!
!
!
    character(len=16) :: motfac, motpas
    character(len=8) :: criter
    real(kind=8) :: prec, dtmin, tole
    integer :: nbinst, n1, freq
    character(len=24) :: sdlist, sdinfl
    integer :: jinfl
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    motfac = motfaz
    motpas = motpaz
    nbinst = 0
    freq = 0
!
! --- NOM DES SDS
!
    sdlist = nomsd(1:19)//'.LIST'
    sdinfl = nomsd(1:19)//'.INFL'
!
! --- L'OPERATEUR N'UTILISE PAS LIST_INST
!
    if (motfac .eq. ' ') then
        freq = 1
        criter = 'RELATIF'
        tole = 0.d0
        nbinst = 0
        dtmin = 0.d0
        goto 99
    endif
!
! --- LECTURE PRECISION
!
    call nmcrpp(motfac, iocc, prec, criter, tole)
!
! --- LECTURE LISTE INSTANTS
!
    call nmcrpa(motfac, iocc, sdlist, base, nbinst,&
                dtmin)
!
! --- LECTURE PAS
!
    n1 = 0
    if (nbinst .eq. 0) then
        call getvis(motfac, motpas, iocc=iocc, scal=freq, nbret=n1)
        if (n1 .ne. 0) then
            ASSERT(freq.ge.0)
        endif
    endif
!
! --- AUCUN MOT-CLE : PAS  = 1
!
    if (n1+nbinst .eq. 0) then
        freq = 1
    endif
!
! --- SAUVEGARDE INFORMATIONS
!
99  continue
    call wkvect(sdinfl, base//' V R', 4, jinfl)
    zr(jinfl-1+1) = freq
    zr(jinfl-1+2) = tole
    zr(jinfl-1+3) = nbinst
    zr(jinfl-1+4) = dtmin
!
    call jedema()
!
end subroutine
