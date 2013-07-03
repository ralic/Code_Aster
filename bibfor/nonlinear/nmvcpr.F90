subroutine nmvcpr(modelz, numedd, mate, carele, comref,&
                  compor, valinc, cnvcpr)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/alchml.h"
#include "asterfort/assvec.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmvccc.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/nmvcex.h"
#include "asterfort/reajre.h"
#include "asterfort/xajcin.h"
    character(len=*) :: modelz
    character(len=24) :: numedd, mate, comref, carele, compor
    character(len=24) :: cnvcpr
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DU VECTEUR ASSEMBLE DE LA VARIATION DES VARIABLES DE
! COMMANDE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NUMEROTATION
! IN  CARALE : CARACTERISTIQUES ELEMENTAIRES
! IN  MATE   : CHAMP DE MATERIAU
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT CNVCPR : VECTEUR ASSEMBLE DE LA VARIATION F_INT PAR RAPPORT
!              AUX VARIABLES DE COMMANDE
!
!
! ----------------------------------------------------------------------
!
!
    integer :: mxnbin, mxnbou, nbin, nbout
    parameter    (mxnbou=2, mxnbin=28)
    character(len=8) :: lpaout(mxnbou), lpain(mxnbin)
    character(len=19) :: lchout(mxnbou), lchin(mxnbin)
!
    logical :: exitem, exihyd, exipto, exisec, exiepa, exipha
    logical :: lbid, exiph1, exiph2, lxfem
    integer :: iret
    real(kind=8) :: x(2)
    character(len=19) :: vecel(2), vecelp, vecelm
    character(len=8) :: modele
    character(len=19) :: depmoi, sigmoi, varmoi, commoi, complu, chsith
    character(len=19) :: vrcplu, insplu
    character(len=19) :: vrcmoi, insmoi
    character(len=24) :: chgeom, chcara(18), chvref, ligrmo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    chvref = '&&NMVCPR.VREF'
    modele = modelz
    ligrmo = modele(1:8)//'.MODELE'
    vecelm = '&&VEVCOM           '
    vecelp = '&&VEVCOP           '
    call exixfe(modele, iret)
    lxfem = iret.ne.0
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'SIGMOI', sigmoi)
    call nmchex(valinc, 'VALINC', 'VARMOI', varmoi)
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(mxnbin, lpain, lchin, mxnbou, lpaout,&
                lchout)
!
! --- LECTURE DES VARIABLES DE COMMANDE EN T- ET T+ ET VAL. DE REF
!
    call nmvcex('TOUT', comref, chvref)
    call nmvcex('TOUT', complu, vrcplu)
    call nmvcex('INST', complu, insplu)
    call nmvcex('TOUT', commoi, vrcmoi)
    call nmvcex('INST', commoi, insmoi)
!
! --- VARIABLES DE COMMANDE PRESENTES
!
    call nmvcd2('HYDR', mate, exihyd, lbid)
    call nmvcd2('PTOT', mate, exipto, lbid)
    call nmvcd2('SECH', mate, exisec, lbid)
    call nmvcd2('EPSA', mate, exiepa, lbid)
    call nmvcd2('META_ZIRC', mate, exiph1, lbid)
    call nmvcd2('META_ACIER', mate, exiph2, lbid)
    call nmvcd2('TEMP', mate, exitem, lbid)
    exipha = exitem .and. (exiph1.or.exiph2)
!
! --- CHAMP DE GEOMETRIE
!
    call megeom(modele, chgeom)
!
! --- CHAMP DE CARACTERISTIQUES ELEMENTAIRES
!
    call mecara(carele(1:8), lbid, chcara)
!
! --- PREPARATION DES VECT_ELEM  (CINQ VARIABLES DE COMM EN DUR !)
!
    call jeexin(vecelm//'.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', vecelm, modele, mate, carele,&
                    'CHAR_MECA')
        call memare('V', vecelp, modele, mate, carele,&
                    'CHAR_MECA')
    endif
    call jedetr(vecelm//'.RELR')
    call jedetr(vecelp//'.RELR')
    call reajre(vecelm, ' ', 'V')
    call reajre(vecelp, ' ', 'V')
!
!
! --- REMPLISSAGE DES CHAMPS D'ENTREE
!
    lpain(1) = 'PVARCRR'
    lchin(1) = chvref(1:19)
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom(1:19)
    lpain(3) = 'PMATERC'
    lchin(3) = mate(1:19)
    lpain(4) = 'PCACOQU'
    lchin(4) = chcara(7)(1:19)
    lpain(5) = 'PCAGNPO'
    lchin(5) = chcara(6)(1:19)
    lpain(6) = 'PCADISM'
    lchin(6) = chcara(3)(1:19)
    lpain(7) = 'PCAORIE'
    lchin(7) = chcara(1)(1:19)
    lpain(8) = 'PCAGNBA'
    lchin(8) = chcara(11)(1:19)
    lpain(9) = 'PCAARPO'
    lchin(9) = chcara(9)(1:19)
    lpain(10) = 'PCAMASS'
    lchin(10) = chcara(12)(1:19)
    lpain(11) = 'PCAGEPO'
    lchin(11) = chcara(5)(1:19)
    lpain(12) = 'PCONTMR'
    lchin(12) = sigmoi
    lpain(13) = 'PVARIPR'
    lchin(13) = varmoi
    lpain(14) = 'PCOMPOR'
    lchin(14) = compor(1:19)
    lpain(17) = 'PNBSP_I'
    lchin(17) = chcara(1) (1:8)//'.CANBSP'
    lpain(18) = 'PFIBRES'
    lchin(18) = chcara(1) (1:8)//'.CAFIBR'
    nbin = 18
!
!     CHAMPS IN SPECIFIQUE A X-FEM
    if (lxfem .and. exitem) then
        call xajcin(modele, 'CHAR_MECA_TEMP_R', mxnbin, lchin, lpain,&
                    nbin)
    endif
!
! --- REMPLISSAGE DU CHAMP DE SORTIE
!
    lpaout(1) = 'PVECTUR'
    nbout = 1
!     CHAMP OUT SPECIFIQUE A X-FEM
    if (lxfem .and. exitem) then
        chsith='&&NMVCPR.CHSITH'
        call alchml(ligrmo, 'SIEF_ELGA', 'PCONTRR', 'V', chsith,&
                    iret, ' ')
        lpaout(2) = 'PCONTRT'
        lchout(2) = chsith
        nbout = nbout+1
    endif
!
! --- CALCUL DES OPTIONS EN T+
!
    lpain(15) = 'PTEMPSR'
    lchin(15) = insplu
    lpain(16) = 'PVARCPR'
    lchin(16) = vrcplu
    call nmvccc(modele, nbin, nbout, lpain, lchin,&
                lpaout, lchout, exitem, exihyd, exipto,&
                exisec, exiepa, exipha, vecelp)
!
! --- CALCUL DES OPTIONS EN T-
!
! --- CALCUL DES PHASES DEJA INCREMENTAL !
!
    exipha = .false.
    lpain(15) = 'PTEMPSR'
    lchin(15) = insmoi
    lpain(16) = 'PVARCPR'
    lchin(16) = vrcmoi
    call nmvccc(modele, nbin, nbout, lpain, lchin,&
                lpaout, lchout, exitem, exihyd, exipto,&
                exisec, exiepa, exipha, vecelm)
!
! --- ASSEMBLAGE
!
    x(1) = 1
    x(2) = -1
    vecel(1) = vecelp
    vecel(2) = vecelm
    call assvec('V', cnvcpr, 2, vecel, x,&
                numedd, ' ', 'ZERO', 1)
!
! --- MENAGE
!
    if (lxfem .and. exitem) then
        call detrsd('CHAM_ELEM', chsith)
    endif
!
    call jedema()
end subroutine
