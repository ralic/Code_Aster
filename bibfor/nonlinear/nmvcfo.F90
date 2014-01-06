subroutine nmvcfo(modelz, mate, carele, compor, comref,&
                  comval, vecelz)
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
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/nmvcex.h"
#include "asterfort/reajre.h"
#include "asterfort/xajcin.h"
    character(len=*) :: modelz, vecelz
    character(len=24) :: mate, comref, carele, compor
    character(len=19) :: comval
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! ESTIMATION D'UNE FORCE DE REFERENCE LIEE A L'ACTION DES VAR. COMM.
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMVAL : VARI_COM A L'INSTANT COURANT
! IN  COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! OUT VECELE : VECT_ELEM FORCE DE REFERENCE (DFINT/DA * A)
!
!
!
!
    integer :: mxnbin, mxnbou, nbin, nbout
    parameter    (mxnbou=2, mxnbin=26)
    character(len=8) :: lpaout(mxnbou), lpain(mxnbin)
    character(len=19) :: lchout(mxnbou), lchin(mxnbin)
!
    logical :: exitem, exihyd, exisec, exiepa
    logical :: lbid, lxfem
    integer :: nbres, iret
    character(len=6) :: masque
    character(len=8) :: modele
    character(len=19) :: vecele
    character(len=19) :: chvref, vrcplu, insplu, chsith
    character(len=24) :: ligrmo, chgeom, chcara(18)
    character(len=16) :: option
    integer :: ifmdbg, nivdbg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    modele = modelz
    ligrmo = modele(1:8)//'.MODELE'
    masque = '.VEXXX'
    vecele = vecelz
    call exixfe(modele, iret)
    lxfem = iret.ne.0
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(mxnbin, lpain, lchin, mxnbou, lpaout,&
                lchout)
!
! --- CHAMP DE GEOMETRIE
!
    call megeom(modele, chgeom)
!
! --- CHAMP DE CARACTERISTIQUES ELEMENTAIRES
!
    call mecara(carele(1:8), chcara)
!
! --- LECTURE DES VARIABLES DE COMMANDE EN T+ ET VAL. DE REF
!
    call nmvcex('TOUT', comref, chvref)
    call nmvcex('TOUT', comval, vrcplu)
    call nmvcex('INST', comval, insplu)
!
! --- VARIABLES DE COMMANDE PRESENTES
!
    call nmvcd2('TEMP', mate, exitem, lbid)
    call nmvcd2('HYDR', mate, exihyd, lbid)
    call nmvcd2('SECH', mate, exisec, lbid)
    call nmvcd2('EPSA', mate, exiepa, lbid)
!
! --- CHAMPS D'ENTREE
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PCACOQU'
    lchin(3) = chcara(7)(1:19)
    lpain(4) = 'PCAGNPO'
    lchin(4) = chcara(6)(1:19)
    lpain(5) = 'PCAORIE'
    lchin(5) = chcara(1)(1:19)
    lpain(6) = 'PCAGNBA'
    lchin(6) = chcara(11)(1:19)
    lpain(7) = 'PCAARPO'
    lchin(7) = chcara(9)(1:19)
    lpain(8) = 'PCAMASS'
    lchin(8) = chcara(12)(1:19)
    lpain(9) = 'PCAGEPO'
    lchin(9) = chcara(5)(1:19)
    lpain(10) = 'PTEMPSR'
    lchin(10) = insplu
    lpain(11) = 'PVARCPR'
    lchin(11) = vrcplu
    lpain(12) = 'PVARCRR'
    lchin(12) = chvref
    lpain(13) = ' '
    lchin(13) = ' '
    lpain(14) = 'PNBSP_I'
    lchin(14) = chcara(1) (1:8)//'.CANBSP'
    lpain(15) = 'PFIBRES'
    lchin(15) = chcara(1) (1:8)//'.CAFIBR'
    lpain(16) = 'PCOMPOR'
    lchin(16) = compor(1:19)
    nbin = 16
!
!     CHAMPS IN SPECIFIQUE A X-FEM
    if (lxfem .and. exitem) then
        call xajcin(modele, 'CHAR_MECA_TEMP_R', mxnbin, lchin, lpain,&
                    nbin)
    endif
!
! --- PREPARATION DES VECT_ELEM
!
    call jeexin(vecele(1:19)// '.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', vecele, modele, mate, carele,&
                    'CHAR_MECA')
    endif
    call jedetr(vecele(1:19)// '.RELR')
    call reajre(vecele, ' ', 'V')
!
! --- CALCUL DES OPTIONS EN T+
!
    nbres = 0
!
! --- THERMIQUE
!
    if (exitem) then
        nbres = nbres+1
        call codent(nbres, 'D0', masque(4:6))
        lpaout(1) = 'PVECTUR'
        lchout(1) = vecele(1:8)//masque
        nbout = 1
        option = 'CHAR_MECA_TEMP_R'
!       CHAMP OUT SPECIFIQUE A X-FEM
        if (lxfem) then
            chsith='&&NMVCFO.CHSITH'
            call alchml(ligrmo, 'SIEF_ELGA', 'PCONTRR', 'V', chsith,&
                        iret, ' ')
            lpaout(2) = 'PCONTRT'
            lchout(2) = chsith
            nbout = nbout+1
        endif
        call calcul('C', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call jeexin(lchout(1)//'.RESL', iret)
        if (iret .gt. 0) then
            call reajre(vecele, lchout(1), 'V')
        else
            nbres = nbres-1
        endif
    endif
!
! --- HYDRATATION
!
    if (exihyd) then
        nbres = nbres+1
        call codent(nbres, 'D0', masque(4:6))
        lpaout(1) = 'PVECTUR'
        lchout(1) = vecele(1:8)//masque
        nbout = 1
        option = 'CHAR_MECA_HYDR_R'
        call calcul('C', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecele, lchout(1), 'V')
    endif
!
! --- SECHAGE
!
    if (exisec) then
        nbres = nbres+1
        call codent(nbres, 'D0', masque(4:6))
        lpaout(1) = 'PVECTUR'
        lchout(1) = vecele(1:8)// masque
        nbout = 1
        option = 'CHAR_MECA_SECH_R'
        call calcul('C', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecele, lchout(1), 'V')
    endif
!
! --- DEFORMATION ANELASTIQUE
!
    if (exiepa) then
        nbres = nbres+1
        call codent(nbres, 'D0', masque(4:6))
        lpaout(1) = 'PVECTUR'
        lchout(1) = vecele(1:8)// masque
        nbout = 1
        option = 'CHAR_MECA_EPSA_R'
        call calcul('C', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecele, lchout(1), 'V')
    endif
!
! --- MENAGE
!
    if (lxfem .and. exitem) then
        call detrsd('CHAM_ELEM', chsith)
    endif
!
    call jedema()
end subroutine
