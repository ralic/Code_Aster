subroutine vecvme(optio2, modelz, carelz, mate, compor,&
                  complz, numedd, cnchtp)
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
    implicit none
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmvcex.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
    character(len=*) :: modelz, carelz, complz, mate, cnchtp
    character(len=16) :: optio2
    character(len=24) :: numedd, compor
!
! ----------------------------------------------------------------------
!     CALCUL DU VECTEUR ASSEMBLE DU CHARGEMENT
!     1. HYDRIQUE
!     2. SECHAGE
!
! IN  OPTIO2  : NOM DE L'OPTION
! IN  MODELZ  : NOM DU MODELE
! IN  CARELZ  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : MATERIAU
! IN  COMPOR  : COMPORTEMENT
! IN  COMPLZ  : SD VARI_COM
! IN  NUMEDD  : NOM DE LA NUMEROTATION
! VAR CNCHTP  : VECTEUR ASSEMBLE DU CHARGEMENT
!
!
!
!
    character(len=6) :: nompro
    parameter ( nompro = 'VECVME' )
!
    character(len=1) :: typres
    character(len=8) :: lpain(13), lpaout(1)
    character(len=8) ::  newnom
    character(len=14) :: complu
    character(len=16) :: option
    character(len=24) :: chgeom, chcara(18), chtime
    character(len=24) :: ligrmo, lchin(13), lchout(1)
    character(len=24) :: vechvp, cnchvp, vrcplu, modele, carele
    character(len=19) :: vecel, chvref
    integer :: i, ibid, iret, jlve, jchtp, jchvp, jtp, jyp, lonch
    data cnchvp/' '/
    data typres/'R'/
!
    call jemarq()
    newnom = '.0000000'
    modele = modelz
    carele = carelz
    complu = complz
    chvref='&&'//nompro//'.CHVREF'
!
!    EXTRACTION DES VARIABLES DE COMMANDE
    call nmvcex('INST', complu, chtime)
    call nmvcex('TOUT', complu, vrcplu)
!
    vecel = '&&'//nompro
    call detrsd('VECT_ELEM', vecel)
    vechvp = vecel//'.RELR'
    call memare('V', vecel, modele(1:8), mate, carele,&
                'CHAR_MECA')
    call wkvect(vechvp, 'V V K24', 1, jlve)
!
    ligrmo = modele(1:8)//'.MODELE'
!
    call megeom(modele(1:8), chgeom)
    call mecara(carele(1:8), chcara)
!
!    VARIABLE DE COMMANDE DE REFERENCE
    call vrcref(modele(1:8), mate(1:8), carele(1:8), chvref)
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PTEMPSR'
    lchin(2) = chtime
    lpain(3) = 'PMATERC'
    lchin(3) = mate
    lpain(4) = 'PCACOQU'
    lchin(4) = chcara(7)
    lpain(5) = 'PCAGNPO'
    lchin(5) = chcara(6)
    lpain(6) = 'PCADISM'
    lchin(6) = chcara(3)
    lpain(7) = 'PCAORIE'
    lchin(7) = chcara(1)
    lpain(8) = 'PCAGNBA'
    lchin(8) = chcara(11)
    lpain(9) = 'PCAARPO'
    lchin(9) = chcara(9)
    lpain(10) = 'PCAMASS'
    lchin(10) = chcara(12)
    lpain(11) = 'PVARCPR'
    lchin(11) = vrcplu
    lpain(12) = 'PVARCRR'
    lchin(12) = chvref
    lpain(13) = 'PCOMPOR'
    lchin(13) = compor
    option = optio2
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = '&&'//nompro//'.???????'
!
    call gcnco2(newnom)
    lchout(1) (10:16) = newnom(2:8)
    call corich('E', lchout(1), -1, ibid)
    call calcul('S', option, ligrmo, 13, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
    zk24(jlve) = lchout(1)
    call jeecra(vechvp, 'LONUTI', 1)
!
! ----- ASSEMBLAGE DU VECT_ELEM
!
    call asasve(vechvp, numedd, typres, cnchvp)
!
! ----- SOMMATION DES CHAMPS DE DEFORMATION ANELASTIQUE
!
    call jeexin(cnchtp, iret)
    if (iret .eq. 0) then
        cnchtp = cnchvp
    else
        call jeveuo(cnchtp, 'L', jtp)
        call jelira(zk24(jtp) (1:19)//'.VALE', 'LONMAX', lonch)
        call jeveuo(zk24(jtp) (1:19)//'.VALE', 'E', jchtp)
        call jeveuo(cnchvp, 'L', jyp)
        call jeveuo(zk24(jyp) (1:19)//'.VALE', 'E', jchvp)
        do 10 i = 1, lonch
            zr(jchtp+i-1) = zr(jchtp+i-1) + zr(jchvp+i-1)
10      continue
    endif
!
    call jedema()
end subroutine
