subroutine crvrc1()
    implicit none
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
!
!     COMMANDE:  CREA_RESU
!     CREE UNE STRUCTURE DE DONNEE DE TYPE "EVOL_THER"  CONTENANT
!     LA TEMPERATURE SUR LES COUCHES DES COQUES MULTICOUCHE A PARTIR
!     D'UN CHAMP DE FONCTIONS (INST,EPAIS)
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/cesvar.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ibid, nbfac, n1, nbinst, kinst, jinst, jlinst
    integer :: nbin, iret, iexi, jtemp
    real(kind=8) :: vinst
    complex(kind=8) :: cbid
    character(len=8) :: kbid, resu, carele, paout, lpain(10), tempef
    character(len=8) :: model2, modele
    character(len=16) :: type, oper
    character(len=19) :: ligrmo, chout, chinst
    character(len=24) :: chcara(18), lchin(10)
    logical :: exicar
!
!----------------------------------------------------------------------
    call jemarq()
!
    call getfac('PREP_VRC1', nbfac)
    if (nbfac .eq. 0) goto 20
    ASSERT(nbfac.eq.1)
!
!
    call getres(resu, type, oper)
    call getvid('PREP_VRC1', 'MODELE', iocc=1, scal=modele, nbret=n1)
    call getvid('PREP_VRC1', 'CARA_ELEM', iocc=1, scal=carele, nbret=n1)
    call getvid('PREP_VRC1', 'CHAM_GD', iocc=1, scal=tempef, nbret=n1)
!
!     -- ON VERIFIE QUE LE CARA_ELEM S'APPUIE BIEN SUR LE MODELE
    call jeexin(carele//'.CANBSP    .CELK', iexi)
    if (iexi .eq. 0) then
        call utmess('F', 'CALCULEL4_14', sk=carele)
    endif
    call jeveuo(carele//'.CANBSP    .CELK', 'L', jtemp)
    model2=zk24(jtemp-1+1)(1:8)
    if (model2 .ne. modele) then
        call utmess('F', 'CALCULEL4_15', sk=carele)
    endif
!
!
!
!     -- INSTANTS DE L'EVOL_THER :
    call getvr8('PREP_VRC1', 'INST', iocc=1, nbval=0, nbret=n1)
    ASSERT(n1.lt.0)
    nbinst = -n1
    call wkvect('&&CRVRC1.LINST', 'V V R', nbinst, jlinst)
    call getvr8('PREP_VRC1', 'INST', iocc=1, nbval=nbinst, vect=zr(jlinst),&
                nbret=n1)
!
    call jeexin(resu//'           .DESC', iret)
    if (iret .ne. 0) then
        call utmess('F', 'CALCULEL7_6', sk=resu)
    else
        call rscrsd('G', resu, 'EVOL_THER', nbinst)
    endif
!
    ligrmo = modele//'.MODELE'
    paout = 'PTEMPCR'
    chinst = '&&CRVRC1.CHINST'
    call mecara(carele, exicar, chcara)
!
    lpain(1) = 'PNBSP_I'
    lchin(1) = chcara(1) (1:8)//'.CANBSP'
    lpain(2) = 'PTEMPEF'
    lchin(2) = tempef
    lpain(3) = 'PINST_R'
    lchin(3) = chinst
    lpain(4) = 'PCACOQU'
    lchin(4) = chcara(7)
    nbin = 4
!
!     -- BOUCLE SUR LES INSTANTS :
!     --------------------------------
    do 10,kinst = 1,nbinst
    vinst = zr(jlinst-1+kinst)
    call mecact('V', chinst, 'MODELE', ligrmo, 'INST_R',&
                1, 'INST', ibid, vinst, cbid,&
                kbid)
    call rsexch(' ', resu, 'TEMP', kinst, chout,&
                iret)
!
    call cesvar(carele, ' ', ligrmo, chout)
    call calcul('S', 'PREP_VRC', ligrmo, nbin, lchin,&
                lpain, 1, chout, paout, 'G',&
                'OUI')
    call detrsd('CHAM_ELEM_S', chout)
    call rsnoch(resu, 'TEMP', kinst)
    call rsadpa(resu, 'E', 1, 'INST', kinst,&
                0, jinst, kbid)
    zr(jinst) = vinst
    call detrsd('CHAMP', chinst)
    10 end do
!
!
    call jedetr('&&CRVRC1.LINST')
!
!
20  continue
    call jedema()
end subroutine
