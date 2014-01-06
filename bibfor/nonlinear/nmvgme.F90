subroutine nmvgme(modele, ligrel, carele, charge, icha,&
                  instan, resufv, depmoi, depdel, vites)
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
! person_in_charge: jean-luc.flejou at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/barych.h"
#include "asterfort/calcul.h"
#include "asterfort/chpnua.h"
#include "asterfort/cnocre.h"
#include "asterfort/copisd.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/nuachp.h"
#include "asterfort/pronua.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtgpld.h"
!
    character(len=*) :: modele, carele, resufv(1), ligrel, charge
    character(len=*) :: depmoi, depdel, vites
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
!                            1 - VITESSE
!
    real(kind=8) :: valr
!
    integer :: ibid, ier, jchar, jfnoe, nbcham
    integer :: vali
    character(len=8) :: fnocal, k8bid
    character(len=16) :: tysd, option
    character(len=19) :: chfnoe
    character(len=24) :: chgeom, nom24, chcara(18)
    character(len=24) :: valk
!
    character(len=8) :: lpain(7), paout
    character(len=19) :: lchin(7)
!
    character(len=8) :: noma1, noma2, madef
    character(len=19) :: resu, nuage1, nuage2, method, resu1
    integer :: nbequa, kvale, nbno, dime, nx
!
    integer :: nblic
    character(len=8) :: licmp(3)
    data licmp/'DX','DY','DZ'/
!
    call jemarq()
    chfnoe = '&&NMVGME.FNOE_CALC'
!
!     - 1. DETERMINATION DU CHAMP A L'INSTANT T
!     -----------------------------------------
    call jeveuo(charge, 'L', jchar)
!
    nom24 = zk24(jchar+icha-1) (1:8)//'.CHME.EVOL.CHAR'
    call jeexin(nom24, ier)
    if (ier .eq. 0) goto 10
!
! -----------------------------------------------------
    call jeveuo(nom24, 'L', jfnoe)
    fnocal = zk8(jfnoe)
!
    call gettco(fnocal, tysd)
!
    if (tysd .ne. 'EVOL_CHAR') then
        call utmess('F', 'ALGORITH7_15', sk=fnocal)
    endif
!
!     ----------------------------------
    call dismoi('NB_CHAMP_MAX', fnocal, 'RESULTAT', repi=nbcham)
!
    if (nbcham .le. 0) then
        call utmess('F', 'ALGORITH7_16', sk=fnocal)
    endif
!
!
! 1 - VITESSE
!     OPTION CHAR_MECA_SR1D1D
!     -----------------------
    option=' '
    call rsinch(fnocal, 'VITE_VENT', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_SR1D1D'
        goto 110
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk = fnocal
        valr = instan
        vali = ier
        call utmess('F', 'ALGORITH13_68', sk=valk, si=vali, sr=valr)
    endif
!
    goto 10
!     CALCUL DES OPTIONS : CHAR_MECA_SR1D1D
!     -------------------------------------
110 continue
    if (option .eq. 'CHAR_MECA_SR1D1D') then
        call megeom(modele, chgeom)
        call mecara(carele, chcara)
!
!       NOM DE CONCEPT
        resu = '&&MNVGME.RESU_PROJE'
!
!       VERIF AVANT DE COMMENCER
        call jelira(chfnoe//'.VALE', 'LONMAX', ival=nbequa)
        call dismoi('NOM_MAILLA', chfnoe, 'CHAMP', repk=noma1)
!
!       -- DETERMINATION DE LA DIMENSION DE L'ESPACE :
        call dismoi('Z_CST', noma1, 'MAILLAGE', repk=k8bid)
        nx=3
        if (k8bid .eq. 'OUI') nx=2
!
        call jeveuo(noma1//'.DIME', 'E', kvale)
        nbno = zi(kvale)
        dime = zi(kvale+5)
        if (nbno * dime .ne. nbequa) then
            call utmess('F', 'ALGORITH8_77')
        endif
!
!       NOM DE CONCEPT MAILLAGE GEOMETRIE DEFORMEE UNIQUE
!       FABRIQUE A PARTIR DU MAILLAGE SOUS JACENT AU MODELE
        madef = '.0000000'
        call gcncon('.', madef)
        call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma2)
        call copisd('MAILLAGE', 'V', noma2, madef)
        call vtgpld('CUMU', noma2//'.COORDO', 1.d0, depmoi, 'V',&
                    madef//'.COORDO1')
        call vtgpld('CUMU', madef//'.COORDO1', 1.d0, depdel, 'V',&
                    madef//'.COORDO')
        call detrsd('CHAMP_GD', madef//'.COORDO1')
!
!       CREATION D'UN CHAM_NO POUR SERVIR DE MODELE
!         ==> CHAMP DE VITESSES AVEC MISE A ZERO
!         ==> MAILLAGE DEFORME DE LA STRUCTURE
        nblic = 3
        call cnocre(madef, 'DEPL_R', 0, [ibid], nblic,&
                    licmp, [ibid], 'V', ' ', resu)
!
        nuage1 = '&&NUAGE1'
        nuage2 = '&&NUAGE2'
        call chpnua(nx, chfnoe, ' ', nuage1)
        call chpnua(nx, resu, ' ', nuage2)
!
!       PROJECTION AVEC LA METHODE NUAGE
        method = 'NUAGE_DEG_1'
        call pronua(method, nuage1, nuage2)
        call nuachp(nuage2, ' ', resu)
        call detrsd('NUAGE', nuage1)
        call detrsd('NUAGE', nuage2)
!
!       LA PROJECTION EST FAITE SUR LE MAILLAGE DEFORME
!       ON REMET DANS RESU LE NOM DU MAILLAGE INITIAL
        call jeveuo(resu//'.REFE', 'E', kvale)
        zk24(kvale) = noma2
        call jedetc('V', madef, 1)
!
!       CONSTRUCTION DU CHAMP DE VITESSE RELATIVE
        nom24 = vites(1:19)//'.VALE'
        call jeexin(nom24, ier)
        if (ier .gt. 0) then
            resu1 = '.0000000'
            call gcncon('.', resu1)
            call copisd('CHAMP_GD', 'V', resu, resu1)
            call barych(resu1, vites(1:19), 1.0d0, -1.0d0, resu,&
                        'V')
            call detrsd('CHAMP_GD', resu1)
        endif
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom(1:19)
        lpain(2) = 'PVITER'
        lchin(2) = resu
        lpain(3) = 'PVENTCX'
        lchin(3) = chcara(14)(1:19)
        lpain(4) = 'PDEPLMR'
        lchin(4) = depmoi(1:19)
        lpain(5) = 'PDEPLPR'
        lchin(5) = depdel(1:19)
        lpain(6) = 'PCAGNPO'
        lchin(6) = chcara(6)(1:19)
        lpain(7) = 'PCAORIE'
        lchin(7) = chcara(1)(1:19)
!
        paout = 'PVECTUR'
!
        call corich('E', resufv(1), icha, ibid)
        call calcul('S', option, ligrel, 7, lchin,&
                    lpain, 1, resufv(1), paout, 'V',&
                    'OUI')
    endif
!
 10 continue
    call jedema()
end subroutine
