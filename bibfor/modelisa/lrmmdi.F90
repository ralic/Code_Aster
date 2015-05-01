subroutine lrmmdi(fid, nomamd, typgeo, nomtyp, nnotyp,&
                  nmatyp, nbnoeu, nbmail, nbnoma, descfi,&
                  adapma)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE -  FORMAT MED - LES DIMENSIONS
!     -    -     -                  -         --
!-----------------------------------------------------------------------
!     LECTURE DU FICHIER MAILLAGE AU FORMAT MED
!               PHASE 0 : LA DESCRIPTION
!     ENTREES :
!       FID    : IDENTIFIANT DU FICHIER MED
!       NOMAMD : NOM MED DU MAILLAGE A LIRE
!       TYPGEO : TYPE MED POUR CHAQUE MAILLE
!       NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!       NNOTYP : NOMBRE DE NOEUDS POUR CHAQUE TYPE DE MAILLES
!       DESCFI : DESCRIPTION DU FICHIER
!     SORTIES:
!       NMATYP : NOMBRE DE MAILLES PAR TYPE
!       NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!       NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!       NBNOMA : NOMBRE CUMULE DE NOEUDS PAR MAILLE
!       ADAPMA : REPERAGE DU NUMERO D'ADAPTATION EVENTUEL
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/as_mmhnme.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: fid
    integer :: nbnoeu, nbmail, nbnoma
    integer :: typgeo(*), nmatyp(*), nnotyp(*)
!
    character(len=8) :: nomtyp(*)
    character(len=*) :: nomamd
    character(len=*) :: adapma
    character(len=*) :: descfi
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: ntymax
    parameter (ntymax = 69)
    integer :: edcoor
    parameter (edcoor=0)
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: typnoe
    parameter (typnoe=0)
    integer :: edconn
    parameter (edconn=1)
    integer :: edmail
    parameter (edmail=0)
    integer :: ednoda
    parameter (ednoda=0)
!
    integer :: codret
    integer :: iaux, jaux, ityp
!
    character(len=8) :: saux08
!
!
!     ------------------------------------------------------------------
    call jemarq()
!
!====
! 1. DIVERS NOMBRES
!====
!
! 1.1. ==> NOMBRE DE NOEUDS
!
    call as_mmhnme(fid, nomamd, edcoor, ednoeu, typnoe,&
                   iaux, nbnoeu, codret)
    if (codret .ne. 0) then
        call codent(codret, 'G', saux08)
        call utmess('F', 'MED_12', sk=saux08)
    endif
    if (nbnoeu .eq. 0) then
        call utmess('F', 'MED_21')
    endif
!
! 1.2. ==> NOMBRE DE MAILLES PAR TYPE ET LONGUEUR TOTALE DU TABLEAU
!          DE CONNECTIVITE NODALE
!
    nbmail = 0
    nbnoma = 0
!
    do 12 , ityp = 1 , ntymax
!
    if (typgeo(ityp) .ne. 0) then
!
        call as_mmhnme(fid, nomamd, edconn, edmail, typgeo(ityp),&
                       ednoda, nmatyp(ityp), codret)
        if (codret .ne. 0) then
            call utmess('A', 'MED_23', sk=nomtyp(ityp))
            call codent(codret, 'G', saux08)
            call utmess('F', 'MED_12', sk=saux08)
        endif
!
        nbmail = nbmail + nmatyp(ityp)
        nbnoma = nbnoma + nmatyp(ityp) * nnotyp(ityp)
!
    else
!
        nmatyp(ityp) = 0
!
    endif
!
    12 end do
!
    if (nbmail .eq. 0) then
        call utmess('F', 'MED_29')
    endif
!
!====
! 2. NUMERO D'ITERATION POUR L'ADAPTATION DE MAILLAGE
!    IL VAUT ZERO SAUF SI LE FICHIER A ETE ECRIT PAR HOMARD. ON TROUVE
!    ALORS LE NUMERO DANS LA DESCRIPTION DU FICHIER SOUS LA FORME :
!    DESCFI = 'HOMARD VN.P   NITER '
!              123456789012345678901
!====
!
    iaux = lxlgut(descfi)
    if (iaux .ge. 20) then
        if (descfi(1:6) .eq. 'HOMARD') then
            read ( descfi(17:21) , fmt='(I5)' ) jaux
        else
            jaux = 0
        endif
    else
        jaux = 0
    endif
!
    call wkvect(adapma, 'G V I', 1, iaux)
    zi(iaux) = jaux
!
!====
! 3. LA FIN
!====
!
    call jedema()
!
end subroutine
