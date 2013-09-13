subroutine op0082()
!
    implicit none
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
! person_in_charge: patrick.massin at edf.fr
!
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_GRILLE
!
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cncinv.h"
#include "asterfort/copisd.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/xprcnu.h"
    integer :: ifm, niv, ibid
    character(len=8) :: grille, mail
    character(len=16) :: k16bid
    character(len=19) :: cnxinv
    character(len=24) :: vcn, grlr
    real(kind=8) :: lcmin
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     GRILLE A CREER
    call getres(grille, k16bid, k16bid)
!
!     MAILLAGE EN ENTREE
    call getvid(' ', 'MAILLAGE', scal=mail, nbret=ibid)
!
!     DUPLIQUE LA SD_MAILLAGE
    call copisd('MAILLAGE', 'G', mail, grille)
!
!     MET A JOUR LE NOM DU MAILLAGE DANS LA SD_MAILLAGE DUPLIQUEE
    call jeveuo(grille//'.COORDO    .REFE', 'E', ibid)
    zk24(ibid) = grille
!
!     CALCUL DES GRANDEURS DE LA GRILLE ET VERIFICATION QUE LE MAILLAGE
!     EN ENTREE PEUT BIEN ETRE UTILISE POUR LA DEFINITION DE LA GRILLE
    cnxinv = '&&OP0082.CNCINV'
    call cncinv(mail, ibid, 0, 'V', cnxinv)
    vcn=grille//'.GRLI'
    grlr=grille//'.GRLR'
    call xprcnu(mail, cnxinv, 'G', vcn, grlr,&
                lcmin)
!
!     STOCKE LA VALEUR DE LA PLUS PETITE ARETE DE LA GRILLE
    call jeveuo(grlr, 'E', ibid)
    zr(ibid) = lcmin
!
!     NETTOYAGE
    call jedetr(cnxinv)
!
!     INFO
    if (niv .gt. 0) then
        write(ifm,*)'  LONGUEUR DE LA PLUS PETITE ARETE DE LA GRILLE: '&
     &                ,lcmin
        write(ifm,*)' '
        write(ifm,*)'  BASE LOCALE DE LA GRILLE:'
        call jeveuo(grlr, 'E', ibid)
        ibid = ibid+1
        write(ifm,900)zr(ibid-1+1),zr(ibid-1+2),zr(ibid-1+3)
        write(ifm,901)zr(ibid-1+4),zr(ibid-1+5),zr(ibid-1+6)
        write(ifm,902)zr(ibid-1+7),zr(ibid-1+8),zr(ibid-1+9)
    endif
!
    900 format(6x,' ','X=(',e11.4,',',e11.4,',',e11.4,')')
    901 format(6x,' ','Y=(',e11.4,',',e11.4,',',e11.4,')')
    902 format(6x,' ','Z=(',e11.4,',',e11.4,',',e11.4,')')
!
    call jedema()
end subroutine
