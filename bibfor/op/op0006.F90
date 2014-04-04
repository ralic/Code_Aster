subroutine op0006()
    implicit none
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE AFFE_MATERIAU
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/afvarc.h"
#include "asterfort/assert.h"
#include "asterfort/cmtref.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/imprsd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rccomp.h"
#include "asterfort/rcmate.h"
#include "asterfort/utmess.h"
    character(len=8) :: chmat, nomail, nomode, nomai2
    character(len=16) :: nomcmd, type
    character(len=24) :: valk(3)
    integer ::  ifm, n1, niv
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    nomode = ' '
    call getres(chmat, type, nomcmd)

!   -- noms du maillage et du modele :
!   ----------------------------------
    nomode = ' '
    nomai2 = ' '
    call getvid(' ', 'MODELE', scal=nomode, nbret=n1)
    if (n1.eq.1) then
        call dismoi('NOM_MAILLA', nomode, 'MODELE', repk=nomail)
        call getvid(' ', 'MAILLAGE', scal=nomai2, nbret=n1)
        if (n1.eq.1 .and. nomail.ne.nomai2) then
            valk(1)=nomode
            valk(2)=nomai2
            valk(3)=nomail
            call utmess('F', 'MODELISA2_11', nk=3, valk=valk)
        endif
    else
        call getvid(' ', 'MAILLAGE', scal=nomail, nbret=n1)
        ASSERT(n1.eq.1)
    endif
!
!
!     1- TRAITEMENT DU MOT-CLE AFFE :
!     -----------------------------------------
    call rcmate(chmat, nomail, nomode)
!
!
!     2- TRAITEMENT DU MOT-CLE AFFE_COMPOR :
!     -----------------------------------------
    call rccomp(chmat, nomail)
!
!
!     3- TRAITEMENT DU MOT-CLE AFFE_VARC :
!     -----------------------------------------
    call afvarc(chmat, nomail, nomode)
!
!
!     4- IL FAUT RECONSTRUIRE LA CARTE .CHAMP_MAT POUR OBTENIR :
!     1 VALEUR DANS LA CARTE => 1 (OU PLUS) MATERIAU(X) + 1 TEMP_REF
!     C'EST UNE CONSEQUENCE DE RCMACO / ALFINT
!     -----------------------------------------------------------------
    call cmtref(chmat, nomail)
!
!
!     5- IMPRESSION DU CHAMP PRODUIT SI INFO=2 :
!     ------------------------------------------
    if (niv .gt. 1) then
        call imprsd('CHAMP', chmat//'.CHAMP_MAT', ifm, 'CHAM_MATER:')
    endif
!
!
    call jedema()
end subroutine
