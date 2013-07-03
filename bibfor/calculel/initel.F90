subroutine initel(ligrel)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterfort/creprn.h"
#include "asterfort/dismoi.h"
#include "asterfort/inigrl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/typele.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ligrel
! ----------------------------------------------------------------------
!     BUT:
!     INITIALISER LES TYPE_ELEMENTS PRESENTS DANS LE LIGREL (INI00K)
!     CREER (ET REMPLIR) LES OBJETS .PRNM ET/OU .PRNS DU LIGREL.
!
!     IN:
!     LIGREL : NOM DU LIGREL A INITIALISER
!
!     OUT:
!       - INITIALISATION DES ELREFE PRESENTS DANS LE LIGREL
!       - CALCUL DES OBJETS : '.PRNM' ET '.PRNS'
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: igr, ngr, ibid, nmaxob, nbobj, ierd, nbprin
    integer :: nbno, jprin, jnoma, jliel, jlliel, iconx1, iconx2, ier
    integer :: nute, nbel, iel, numa, nbnoma, ino, nuno
    parameter (nmaxob=30)
    integer :: adobj(nmaxob)
    character(len=24) :: noobj(nmaxob)
    character(len=1) :: k1bid, base
    character(len=8) :: exiele, ma, prin, nomail, resuco
    character(len=16) :: nomte, nomcmd, typcon
! ----------------------------------------------------------------------
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    call dismoi('F', 'EXI_ELEM', ligrel, 'LIGREL', ibid,&
                exiele, ierd)
    if (exiele(1:3) .eq. 'OUI') then
        call jelira(ligrel//'.LIEL', 'CLAS', ibid, base)
    else
!       -- UN LIGREL QUI N'A PAS D'ELEMENTS VIENT FORCEMENT
!          D'UN MODELE QUI DOIT AVOIR DES SOUS-STRUCTURES STATIQUES
        call jelira(ligrel//'.SSSA', 'CLAS', ibid, base)
        goto 20
    endif
!
    call jelira(ligrel//'.LIEL', 'NUTIOC', ngr, k1bid)
    do 10 igr = 1, ngr
        call inigrl(ligrel, igr, nmaxob, adobj, noobj,&
                    nbobj)
10  end do
20  continue
!
!
!     -- CALCUL DE .PRNM ET .PRNS :
    call creprn(ligrel, ' ', base, ligrel(1:19)//'.PRNM', ligrel(1:19)//'.PRNS')
!
!
!
!
!     -- ON VERIFIE QUE LES ELEMENTS DE "BORD" SONT COLLES AUX
!        ELEMENTS "PRINCIPAUX" (CEUX QUI CALCULENT LA RIGIDITE):
!     ------------------------------------------------------------
    call getres(resuco, typcon, nomcmd)
    if ((exiele(1:3).ne.'OUI') .or. (nomcmd.ne.'AFFE_MODELE')) goto 90
!
    call jeveuo(ligrel//'.LGRF', 'L', jnoma)
    call jeveuo(ligrel//'.LIEL', 'L', jliel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', jlliel)
    ma = zk8(jnoma)
    call jeveuo(ma//'.CONNEX', 'L', iconx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', iconx2)
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                k1bid, ier)
!
!     -- ON COCHE LES NOEUDS PORTES PAR LES ELEMENTS PRINCIPAUX :
    call wkvect('&&INITEL.PRIN', 'V V I', nbno, jprin)
    do 50 igr = 1, ngr
        nute = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
        call dismoi('F', 'CALC_RIGI', nomte, 'TYPE_ELEM', ibid,&
                    prin, ier)
        if (prin .ne. 'OUI') goto 50
        nbel = nbelem(ligrel,igr)
        do 40 iel = 1, nbel
            numa = zi(jliel-1+zi(jlliel+igr-1)+iel-1)
            if (numa .lt. 0) goto 40
            nbnoma = zi(iconx2+numa) - zi(iconx2+numa-1)
            do 30,ino = 1,nbnoma
            nuno = zi(iconx1-1+zi(iconx2+numa-1)+ino-1)
            zi(jprin-1+nuno) = 1
30          continue
40      continue
50  end do
!
!     -- ON VERIFIE LES NOEUDS DES ELEMENTS NON-PRINCIPAUX (BORD)
    nbprin=0
    do 80 igr = 1, ngr
        nute = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
        call dismoi('F', 'CALC_RIGI', nomte, 'TYPE_ELEM', ibid,&
                    prin, ier)
        nbel = nbelem(ligrel,igr)
        if (prin .eq. 'OUI') then
            if (nbel .gt. 0) nbprin=1
            goto 80
        endif
        do 70 iel = 1, nbel
            numa = zi(jliel-1+zi(jlliel+igr-1)+iel-1)
            if (numa .lt. 0) goto 70
            nbnoma = zi(iconx2+numa) - zi(iconx2+numa-1)
            do 60,ino = 1,nbnoma
            nuno = zi(iconx1-1+zi(iconx2+numa-1)+ino-1)
            if (zi(jprin-1+nuno) .ne. 1) then
                call jenuno(jexnum(ma//'.NOMMAI', numa), nomail)
                call u2mesk('A', 'CALCULEL2_63', 1, nomail)
                goto 71
            endif
60          continue
71          continue
70      continue
80  end do
!
!     -- SI C'EST LE LIGREL DU MODELE, ON VERIFIE QU'IL EXISTE AU MOINS
!        UN ELEMENT PRINCIPAL (QUI CALCULE DE LA RIGIDITE):
    if (nomcmd .eq. 'AFFE_MODELE' .and. resuco(1:8) .eq. ligrel(1:8)) then
        if (nbprin .eq. 0) call u2mesk('A', 'CALCULEL2_64', 1, resuco)
!
    endif
!
!
    call jedetr('&&INITEL.PRIN')
!
!
!
90  continue
    call jedema()
end subroutine
