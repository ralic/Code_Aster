subroutine lrvemo(modele)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    character(len=8) :: modele
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
!-----------------------------------------------------------------------
!   BUT:ROUTINE DE LIRE RESU / LIRE_CHAMP QUI VERIFIE LA COHERENCE ENTRE
!       LE MODELE FOURNI ET LES DONNEES DU FICHIER MED
!
!   ENTREE: MODELE(K8)  = NOM DU MODELE
!-----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
!
    integer :: n1
!
    character(len=8) :: chanom, typech
    character(len=16) :: typres, pheno, valk(2), nomcmd, tych
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    call getres(chanom, typech, nomcmd)
!
!     ON VERIFIE QUE LE PHENOMENE DU MODELE FOURNI EST COHERENT AVEC
!     LA SD RESULTAT A PRODUIRE
!     =========================
    if (nomcmd(1:9) .eq. 'LIRE_RESU') then
        call getvtx(' ', 'TYPE_RESU', scal=typres, nbret=n1)
        call dismoi('PHENOMENE', modele, 'MODELE', repk=pheno)
        valk(1)=pheno
        valk(2)=typres
        if (typres(1:9) .eq. 'EVOL_THER') then
            if (pheno(1:9) .eq. 'MECANIQUE') then
                call utmess('F+', 'MED_54')
                call utmess('F', 'MED_56', nk=2, valk=valk)
            endif
        else
            if (pheno(1:9) .eq. 'THERMIQUE') then
                call utmess('F+', 'MED_54')
                call utmess('F', 'MED_56', nk=2, valk=valk)
            endif
        endif
    else if (nomcmd(1:10).eq.'LIRE_CHAMP') then
        call dismoi('PHENOMENE', modele, 'MODELE', repk=pheno)
        call getvtx(' ', 'TYPE_CHAM', scal=tych, nbret=n1)
    endif
!
    call jedema()
!
end subroutine
