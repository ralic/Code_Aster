subroutine obcrea(typstz, nomstr)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/obcr26.h"
#include "asterfort/obcr27.h"
#include "asterfort/obcr28.h"
#include "asterfort/obcrlo.h"
#include "asterfort/wkvect.h"
    character(len=*) :: typstz
    character(len=24) :: nomstr
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT)
!
! CREATION
!
! ----------------------------------------------------------------------
!
!
! IN  TYPSTR : TYPE DU STRUCT
! IN  NOMSTR : NOM DU STRUCT
!
! ----------------------------------------------------------------------
!
    integer :: nbpari, nbparr, nbpark, nbparo, nbparb
    character(len=24) :: sddesc, sddesk
    integer :: jsdesc, jsdesk
    integer :: nbdesc, nbdesk
    integer :: ifm, niv
    character(len=24) :: typstr
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    typstr = typstz
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION STRUCT <',nomstr,&
        ' DE TYPE <',typstr,'>'
    endif
!
! --- NOM DES OBJETS JEVEUX
!
    nbdesc = 5
    sddesc = nomstr(1:19)//'.DESC'
    nbdesk = 1
    sddesk = nomstr(1:19)//'.DESK'
!
! --- CREATION
!
    if (typstr .eq. 'LISTE_STRUCTS') then
        call obcrlo(nomstr, nbparb, nbpari, nbparr, nbpark,&
                    nbparo)
    else if (typstr.eq.'AFFICHAGE') then
        call obcr26(nomstr, nbparb, nbpari, nbparr, nbpark,&
                    nbparo)
    else if (typstr.eq.'TABLEAU') then
        call obcr27(nomstr, nbparb, nbpari, nbparr, nbpark,&
                    nbparo)
    else if (typstr.eq.'TABLEAU_COLONNE') then
        call obcr28(nomstr, nbparb, nbpari, nbparr, nbpark,&
                    nbparo)
    else
        write(6,*) 'TYPE SD INTROUVABLE: ',typstr
        ASSERT(.false.)
    endif
!
! --- CREATION DESCRIPTEURS
!
    call wkvect(sddesc, 'V V I  ', nbdesc, jsdesc)
    zi(jsdesc-1+1) = nbparb
    zi(jsdesc-1+2) = nbpari
    zi(jsdesc-1+3) = nbparr
    zi(jsdesc-1+4) = nbpark
    zi(jsdesc-1+5) = nbparo
    call wkvect(sddesk, 'V V K24', nbdesk, jsdesk)
    zk24(jsdesk-1+1) = typstr
!
    call jedema()
end subroutine
