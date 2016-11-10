subroutine vectme(modelz, carelz, mate, compor, complz,&
                  vecelz)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/nmvcex.h"
#include "asterfort/reajre.h"
#include "asterfort/vrcref.h"
#include "asterfort/xajcin.h"
    character(len=*) :: modelz, carelz, complz, vecelz, mate
    character(len=24) :: compor
!
! ----------------------------------------------------------------------
!     CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS THERMIQUES
!
! IN  MODELZ  : NOM DU MODELE
! IN  CARELZ  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : MATERIAU
! IN  COMPLU  : SD VARI_COM A L'INSTANT T+
! OUT/JXOUT  VECELZ  : VECT_ELEM RESULTAT.
!
    integer :: mxnbin, mxnbou, nbin, nbout
    parameter    (mxnbin=30,mxnbou=2)
    integer :: ibid, iret, ich
    character(len=8) :: lpain(mxnbin), lpaout(mxnbou), newnom
    character(len=14) :: complu
    character(len=16) :: option
    character(len=19) :: vecele, resuel, chvref, chsith
    character(len=24) :: chgeom, chcara(18), chtime, ligrmo, vrcplu
    character(len=24) :: lchin(mxnbin), lchout(mxnbou), modele, carele
    aster_logical :: ltemp, lxfem
!
    call jemarq()
    newnom = '.0000000'
    modele = modelz
    carele = carelz
    complu = complz
!     -- POUR NE CREER Q'UN SEUL CHAMP DE VARIABLES DE REFERENCE
    chvref = modele(1:8)//'.CHVCREF'
!
!     -- ALLOCATION DU VECT_ELEM RESULTAT :
!     -------------------------------------
    vecele = '&&VEMTPP           '
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), mate, carele,&
                'CHAR_MECA')
!
!     -- S'IL N'Y A PAS DE TEMPERATURE, IL N'Y A RIEN A FAIRE :
!     ---------------------------------------------------------
    call nmvcd2('TEMP', mate, ltemp)
    if (.not.ltemp) goto 999
!
!     -- S'AGIT-IL D'UN MODELE X-FEM
    call exixfe(modele, iret)
    lxfem = iret.ne.0
!
!     -- EXTRACTION DES VARIABLES DE COMMANDE
    call nmvcex('TOUT', complu, vrcplu)
    call nmvcex('INST', complu, chtime)
!
!     -- VARIABLE DE COMMANDE DE REFERENCE
    call vrcref(modele(1:8), mate(1:8), carele(1:8), chvref)
!
    ligrmo = modele(1:8)//'.MODELE'
!
    call megeom(modele(1:8), chgeom)
    call mecara(carele(1:8), chcara)
!
    option = 'CHAR_MECA_TEMP_R'
!
!     -- CHAMPS ET PARAMETRES IN
!     --------------------------
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
    lpain(13) = 'PCAGEPO'
    lchin(13) = chcara(5)
    lpain(14) = ' '
    lchin(14) = ' '
    lpain(15) = 'PNBSP_I'
    lchin(15) = chcara(1) (1:8)//'.CANBSP'
    lpain(16) = 'PFIBRES'
    lchin(16) = chcara(1) (1:8)//'.CAFIBR'
    lpain(17) = 'PCOMPOR'
    lchin(17) = compor
    nbin = 17
    do ich = nbin+1, mxnbin
        lchin(ich) = ' '
        lpain(ich) = ' '
    end do
!
!     -- CHAMPS IN SPECIFIQUE A X-FEM
    if (lxfem) then
        call xajcin(modele, option, mxnbin, lchin, lpain,&
                    nbin)
    endif
!
!     -- CHAMPS ET PARAMETRES OUT
!     ---------------------------
    call gcnco2(newnom)
    resuel = '&&VECTME.???????'
    resuel(10:16) = newnom(2:8)
    call corich('E', resuel, -1, ibid)
    lpaout(1) = 'PVECTUR'
    lchout(1) = resuel
    lpaout(2) = ' '
    lchout(2) = ' '
    nbout = 1
!
!     -- CHAMP OUT SPECIFIQUE A X-FEM
    if (lxfem) then
        chsith='&&VECTME.CHSITH'
        call alchml(ligrmo, 'SIEF_ELGA', 'PCONTRR', 'V', chsith,&
                    iret, ' ')
        ASSERT(iret.eq.0)
        lpaout(2) = 'PCONTRT'
        lchout(2) = chsith
        nbout = nbout+1
    endif
!
!     -- CALCUL DE L'OPTION
!     ---------------------
    call calcul('C', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
    call reajre(vecele, lchout(1), 'V')
!
!     -- MENAGE
!     ---------
    if (lxfem) then
        call detrsd('CHAM_ELEM', chsith)
    endif
!
999 continue
    vecelz = vecele//'.RELR'
!
    call jedema()
end subroutine
