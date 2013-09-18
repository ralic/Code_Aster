subroutine crcmel(nbmo1, moclef, compor, ces2, modele,&
                  ncmpma, nomcmp, nt)
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
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
!     INITIALISATION D'UNE CARTE DE COMPORTEMENT
!     RELATION='ELAS' SUR TOUT LE MAILLAGE
!
! IN   NBMO1  :  NOMBRE DE MOTS CLES FACTEURS (COMP_ELAS / COMP_INCR)
! IN MOCLEF   :  LISTE DES MOTS-CLES (COMP_INCR / COMP_ELAS)
! OUT COMPOR  :  CARTE DE COMPORTEMENT CREEE
! OUT CES2    :  CHAMELEM SIMPLE ISSU DE COMPOR, DEFINIR SUR LES
!                ELEMENTS QUI CALCULENT FULL_MECA
! IN MODELE   : LE MODELE
! IN NCMPMA   : NOMBRE DE CMP DE LA GRANDEUR COMPOR
! IN NOMCMP   : NOMS DES CMP DE LA GRANDEUR COMPOR
! OUT NT      : NOMBRE D'OCCURRENCES DE COMP_INCR / COMP_ELAS
! ---------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
    integer :: iret, nbmo1, i, nt, n1, ncmpma, ibid, irepe
    integer :: jncmp, jvalv, icmp, iexi
    character(len=16) :: moclef(2)
    character(len=8) :: nomcmp(ncmpma), noma
    character(len=19) :: compor
    character(len=24) :: modele, ligrel
    character(len=19) :: ces1, ces2, cel1
! ----------------------------------------------------------------------
!
    call jemarq()
!
!
!    -----------------------------------------------------------
!     ON INITIALISE UNE CARTE PAR DEFAUT, SUR TOUT LE MAILLAGE
!     AVEC LES CARACTERISTIQUES SUIVANTES :
!          COMP_INCR ( RELATION    : ELAS
!                      DEFORMATION : PETIT
!                      TOUT        : OUI)
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', i,&
                noma, iret)
    call alcart('V', compor, noma, 'COMPOR')
    call jeveuo(compor//'.NCMP', 'E', jncmp)
    call jeveuo(compor//'.VALV', 'E', jvalv)
    do 90 icmp = 1, ncmpma
        zk8(jncmp+icmp-1) = nomcmp(icmp)
90  end do
!
!
    zk16(jvalv-1+1) = 'ELAS'
    zk16(jvalv-1+2) = '1'
    zk16(jvalv-1+3) = 'PETIT'
    zk16(jvalv-1+4) = 'COMP_ELAS'
    zk16(jvalv-1+5) = 'ANALYTIQUE'
    zk16(jvalv-1+6) = '1'
    do 20 i = 7, ncmpma
        zk16(jvalv-1+i) = ' '
20  end do
!
    nt=0
    do 10 i = 1, nbmo1
        call getfac(moclef(i), n1)
        nt=max(nt,n1)
10  end do
    if (nt .eq. 0) then
!        UTILISE EN PARTICULIER POUR LA COMMANDE CALC_G :
!        SI AUCUN DES DEUX COMPORTEMENTS COMP_ELAS ET COMP_INCR N'EST
!        SPECIFIE PAR L'UTILISATEUR,      IRET=1
!        DANS CE CAS ON CHOISIT COMP_ELAS
        zk16(jvalv-1+4) = 'COMP_ELAS'
    endif
!
    call nocart(compor, 1, ncmpma)
!
! ======================================================================
!    ETAPE UTILE POUR LES VERIFICATIONS DE COHERENCE
!    CREATION D'UN CHAM_ELEM ASSOCIE A LA CARTE COMPO1 DEFINI SEULEMENT
!    SUR LES MAILLES SACHANT CALCULER FULL_MECA
! ======================================================================
!
    if (nt .ne. 0) then
        ligrel = modele(1:8)//'.MODELE    .LIEL'
        call jeveuo(ligrel(1:19)//'.REPE', 'L', irepe)
        ces1='&&CRCMEL.CES1'
        cel1='&&CRCMEL.CEL1'
        call carces(compor, 'ELEM', ' ', 'V', ces1,&
                    'A', ibid)
        call cescel(ces1, ligrel, 'FULL_MECA', 'PCOMPOR', 'OUI',&
                    ibid, 'V', cel1, 'A', ibid)
!         ON RECUPERE UN CHAM_ELEM DEFINI SEULEMENT SUR LES ELEMENTS
!         QUI SAVENT CALCULER FULL_MECA
!
!         -- SI LE CHAMP EST VIDE, C'EST QU'AUCUN ELEMENT DU MODELE
!            NE SAIT CALCULER FULL_MECA :
        call exisd('CHAMP', cel1, iexi)
        if (iexi .eq. 0) then
            call utmess('F', 'CALCULEL5_5')
        endif
        call celces(cel1, 'V', ces2)
        call detrsd('CHAMP', ces1)
        call detrsd('CHAMP', cel1)
    endif
!
    call jedema()
end subroutine
