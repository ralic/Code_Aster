subroutine nmdepr(modele, ligrel, carele, charge, icha,&
                  instan, resufv)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
!
#include "asterc/gettco.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/rsinch.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: modele, carele, resufv(3), ligrel, charge
    real(kind=8) :: instan
    integer :: icha
! ----------------------------------------------------------------------
!   CALCUL DU SECOND MEMBRE ELEMENTAIRE CORRESPONDANT A EVOL_CHAR
!   POUR LA CHARGE CHARGE(ICHA)
!   SI IL N'Y A PAS DE EVOL_CHAR DANS CHARGE(ICHA):
!                             RESUFV N'EST PAS CALCULE
!
! IN  MODELE      : NOM DU MODELE
! IN  LIGREL      : (SOUS)-LIGREL DU MODELE
! IN  CARELE      : NOM DU CARA_ELEM
! IN  CHARGE      : LISTE DES CHARGES
! IN  ICHA        : NUMERO DE LA CHARGE
! IN  INSTAN      : INSTANT DE LA DETERMINATION
! IN/JXOUT RESUFV : RESUELEM CORRESPONDANT AU CALCUL DE EVOL_CHAR
!                       RESULTATS POSSIBLE
!                            1 - VOLUMIQUE
!                            2 - SURFACIQUE
!                            3 - PRESSION
!
    real(kind=8) :: valr
!
    logical :: exicar
    integer :: ibid, ier, jchar, jfnoe, nbcham
    integer :: vali
    character(len=8) :: fnocal, k8bid, lpain(6), paout
    character(len=16) :: tysd, option
    character(len=19) :: chfnoe, lchin(6)
    character(len=24) :: chgeom, nom24, chcara(18)
    character(len=24) :: valk(4)
!
    call jemarq()
    chfnoe = '&&NMDEPR.FNOE_CALC'
!
!     - 1. DETERMINATION DU CHAMP A L'INSTANT T
!     -----------------------------------------
    call jeveuo(charge, 'L', jchar)
!
    nom24 = zk24(jchar+icha-1) (1:8)//'.CHME.EVOL.CHAR'
    call jeexin(nom24, ier)
    if (ier .eq. 0) goto 40
!
! -----------------------------------------------------
    call jeveuo(nom24, 'L', jfnoe)
    fnocal = zk8(jfnoe)
!
    call gettco(fnocal, tysd)
!
    if (tysd .ne. 'EVOL_CHAR') then
        call u2mesk('F', 'ALGORITH7_15', 1, fnocal)
        goto 40
    endif
!
!     ----------------------------------
    call dismoi('F', 'NB_CHAMP_UTI', fnocal, 'RESULTAT', nbcham,&
                k8bid, ier)
!
    if (nbcham .le. 0) then
        call u2mesk('F', 'ALGORITH7_16', 1, fnocal)
        goto 40
    endif
!
! 1 - VOLUMIQUE
!     OPTION CHAR_MECA_FR2D2D OU CHAR_MECA_FR3D3D
!     -------------------------------------------
    option = ' '
    call rsinch(fnocal, 'FVOL_3D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_FR3D3D'
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk (1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_56', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
    call rsinch(fnocal, 'FVOL_2D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_FR2D2D'
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk (1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_57', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
!     CALCUL DES OPTIONS : CHAR_MECA_FR2D2D OU CHAR_MECA_FR3D3D
!     ---------------------------------------------------------
10  continue
    if (option .eq. 'CHAR_MECA_FR3D3D' .or. option .eq. 'CHAR_MECA_FR2D2D') then
        call megeom(modele, chgeom)
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        if (option .eq. 'CHAR_MECA_FR3D3D') lpain(2)='PFR3D3D'
        if (option .eq. 'CHAR_MECA_FR2D2D') lpain(2)='PFR2D2D'
        lchin(2) = chfnoe
        paout = 'PVECTUR'
!
        call corich('E', resufv(1), icha, ibid)
        call calcul('S', option, ligrel, 2, lchin,&
                    lpain, 1, resufv(1), paout, 'V',&
                    'OUI')
    endif
!
! 2 - SURFACIQUE
!     OPTION CHAR_MECA_FR2D3D OU CHAR_MECA_FR1D2D
!     -------------------------------------------
    call rsinch(fnocal, 'FSUR_3D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        if (option .eq. 'CHAR_MECA_FR2D2D') then
            valk (1) = fnocal
            valr = instan
            call u2mesg('F', 'ALGORITH13_58', 1, valk, 0,&
                        0, 1, valr)
        endif
        option = 'CHAR_MECA_FR2D3D'
        goto 20
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk (1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_59', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
    call rsinch(fnocal, 'FSUR_2D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        if (option .eq. 'CHAR_MECA_FR3D3D') then
            valk (1) = fnocal
            valr = instan
            call u2mesg('F', 'ALGORITH13_60', 1, valk, 0,&
                        0, 1, valr)
        endif
        option = 'CHAR_MECA_FR1D2D'
        goto 20
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk (1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_61', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
!     CALCUL DES OPTIONS : CHAR_MECA_FR2D3D OU CHAR_MECA_FR1D2D
!     ---------------------------------------------------------
20  continue
    if (option .eq. 'CHAR_MECA_FR2D3D' .or. option .eq. 'CHAR_MECA_FR1D2D') then
        call megeom(modele, chgeom)
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        if (option .eq. 'CHAR_MECA_FR2D3D') lpain(2) = 'PFR2D3D'
        if (option .eq. 'CHAR_MECA_FR1D2D') lpain(2) = 'PFR1D2D'
        lchin(2) = chfnoe
        paout = 'PVECTUR'
!
        call corich('E', resufv(2), icha, ibid)
        call calcul('S', option, ligrel, 2, lchin,&
                    lpain, 1, resufv(2), paout, 'V',&
                    'OUI')
    endif
!
! 3 - PRESSION
!     OPTION CHAR_MECA_PRES_R
!     -----------------------
    call rsinch(fnocal, 'PRES', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_PRES_R'
        goto 30
    else if (ier.eq.11 .or. ier.eq.12) then
        valk (1) = fnocal
        valk (2) = ' '
        valk (3) = ' '
        valk (4) = ' '
        valr = instan
        call u2mesg('F', 'ALGORITH13_62', 4, valk, 0,&
                    0, 1, valr)
    else if (ier.eq.20) then
        valk (1) = fnocal
        valk (2) = ' '
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_63', 2, valk, 1,&
                    vali, 1, valr)
    endif
!
!     CALCUL DE L'OPTION : CHAR_MECA_PRES_R
!     -------------------------------------
30  continue
    if (option .eq. 'CHAR_MECA_PRES_R') then
        call megeom(modele, chgeom)
        call mecara(carele, exicar, chcara)
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PPRESSR'
        lchin(2) = chfnoe
        lpain(3) = 'PCACOQU'
        lchin(3) = chcara(7)
        lpain(4) = 'PCAGEPO'
        lchin(4) = chcara(5)
        lpain(5) = 'PCAORIE'
        lchin(5) = chcara(1)
        lpain(6) = 'PNBSP_I'
        lchin(6) = chcara(1) (1:8)//'.CANBSP'
        paout = 'PVECTUR'
!
        call corich('E', resufv(3), icha, ibid)
        call calcul('S', option, ligrel, 6, lchin,&
                    lpain, 1, resufv(3), paout, 'V',&
                    'OUI')
    endif
!
!
40  continue
    call jedema()
end subroutine
