subroutine vecgme(modele, carele, mate, charge, infcha,&
                  instap, depmoz, depdez, vecelz, instam,&
                  compor, carcri, ligrez, vitez)
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
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmvgme.h"
#include "asterfort/reajre.h"
#include "asterfort/u2mess.h"
    character(len=*) :: mate, ligrez, vecelz, depmoz, depdez, vitez
    character(len=19) :: vecele
    character(len=24) :: modele, carele, charge, infcha, compor, carcri
    real(kind=8) :: instap, instam
! ----------------------------------------------------------------------
!     CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS MECANIQUES
!     DEPENDANT DE
!          LA GEOMETRIE
!          LA VITESSE
!          L'ACCELARATION
!     PRODUIT UN VECT_ELEM DEVANT ETRE ASSEMBLE PAR LA ROUTINE ASASVE
!
! IN  MODELE  : NOM DU MODELE
! IN  CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : NOM DU MATERIAU
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  INSTAP  : INSTANT DU CALCUL
! IN  DEPMOI  : DEPLACEMENT A L'INSTANT TEMMOI
! IN  DEPDEL  : INCREMENT DE DEPLACEMENT AU COURS DES ITERATIONS
! IN  INSTAM  : INSTANT MOINS
! IN  COMPOR  : COMPORTEMENT
! IN  CARCRI  : CRITERES DE CONVERGENCE (THETA)
! IN  LIGREZ  : (SOUS-)LIGREL DE MODELE POUR CALCUL REDUIT
!                  SI ' ', ON PREND LE LIGREL DU MODELE
! VAR/JXOUT  VECELZ  : VECT_ELEM RESULTAT.
! ----------------------------------------------------------------------
!
    character(len=5) :: suffix
    character(len=8) :: nomcha, lpain(16), paout, k8bid, affcha, kbid, newnom
    character(len=16) :: option
    character(len=24) :: chgeom, chcara(18), chtime, ligrel, ligrmo
    character(len=24) :: lchin(16), chtim2, ligrch, evolch
    character(len=19) :: resuel, resufv(1), depmoi, depdel, vites
    integer :: ibid, iret, nchar, ilve, jchar, jinf, k, icha, numchm
    integer :: ierd, jlchin, ier
    logical :: exicar, bidon
    complex(kind=8) :: cbid
    integer :: nbchmx, ii, somme
    parameter (nbchmx=7)
    integer :: nbopt(nbchmx), tab(nbchmx)
    character(len=6) :: nomlig(nbchmx), nompaf(nbchmx), nompar(nbchmx)
    character(len=6) :: nomopf(nbchmx), nomopr(nbchmx)
!
    data nomlig/'.F1D1D','.PESAN','.ROTAT','.PRESS','.VEASS','.FCO3D','.EFOND'/
    data nomopr/'SR1D1D','PESA_R','ROTA_R','PRSU_R','      ','SRCO3D','EFON_R'/
    data nomopf/'SF1D1D','??????','??????','PRSU_F','      ','SFCO3D','EFON_F'/
    data nompar/'FR1D1D','PESANR','ROTATR','PRESSR','      ','FRCO3D','PEFOND'/
    data nompaf/'FF1D1D','??????','??????','PRESSF','      ','FFCO3D','PEFOND'/
    data nbopt/10,15,10,9,0,9,16/
!     ------------------------------------------------------------------
!
    call jemarq()
    newnom = '.0000000'
!
!
    vecele = vecelz
    if (vecele .eq. ' ') vecele = '&&VEMSUI           '
    resuel = '&&VECGME.???????'
    depmoi = depmoz
    depdel = depdez
    vites = vitez
!
    bidon = .true.
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar)
        if (nchar .ne. 0) then
            call jeveuo(charge, 'L', jchar)
            call jeveuo(infcha, 'L', jinf)
            do k = 1, nchar
                if (zi(jinf+nchar+k) .eq. 4) bidon = .false.
            enddo
        endif
    endif
!
!     -- ALLOCATION DU VECT_ELEM RESULTAT :
!     -------------------------------------
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), mate, carele,&
                'CHAR_MECA')
    call reajre(vecele, ' ', 'V')
    if (bidon) goto 99
!
    ligrmo = ligrez
    if (ligrmo .eq. ' ') ligrmo = modele(1:8)//'.MODELE'
    ligrel = ligrmo
!
    call megeom(modele(1:8), chgeom)
    call mecara(carele(1:8), exicar, chcara)
!
    chtime = '&&VECHME.CH_INST_R'
    call mecact('V', chtime, 'LIGREL', ligrel, 'INST_R  ',&
                1, 'INST   ', ibid, instap, cbid,&
                kbid)
    chtim2 = '&&VECHME.CH_INST_M'
    call mecact('V', chtim2, 'LIGREL', ligrel, 'INST_R  ',&
                1, 'INST   ', ibid, instam, cbid,&
                kbid)
!
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
    lpain(3) = 'PTEMPSR'
    lchin(3) = chtime
    lpain(4) = 'PMATERC'
    lchin(4) = mate
    lpain(5) = 'PCACOQU'
    lchin(5) = chcara(7)
    lpain(6) = 'PCAGNPO'
    lchin(6) = chcara(6)
    lpain(7) = 'PCADISM'
    lchin(7) = chcara(3)
    lpain(8) = 'PDEPLMR'
    lchin(8) = depmoi
    lpain(9) = 'PDEPLPR'
    lchin(9) = depdel
    lpain(10) = 'PCAORIE'
    lchin(10) = chcara(1)
    lpain(11) = 'PCACABL'
    lchin(11) = chcara(10)
    lpain(12) = 'PCARCRI'
    lchin(12) = carcri
    lpain(13) = 'PINSTMR'
    lchin(13) = chtim2
    lpain(15) = 'PINSTPR'
    lchin(15) = chtime
    lpain(14) = 'PCOMPOR'
    lchin(14) = compor
    paout = 'PVECTUR'
!
    ilve = 0
    do icha = 1, nchar
        nomcha = zk24(jchar+icha-1) (1:8)
        ligrch = nomcha//'.CHME.LIGRE'
        numchm = zi(jinf+nchar+icha)
        call dismoi('F', 'TYPE_CHARGE', zk24(jchar+icha-1), 'CHARGE', ibid,&
                    affcha, ierd)
!
        if (numchm .eq. 4) then
            somme = 0
!
            do k = 1, nbchmx
                if (nomlig(k) .eq. '.FORNO') then
                    ligrel = ligrch
                else
                    ligrel = ligrmo
                endif
!
                if (nomlig(k) .eq. '.VEASS') then
                    suffix = '     '
                else
                    suffix = '.DESC'
                endif
                lchin(1) = nomcha//'.CHME'//nomlig(k)//suffix
                call exisd('CHAMP_GD', lchin(1), iret)
                tab(k)=iret
                if (iret .ne. 0) then
                    if (affcha(5:7) .eq. '_FO') then
                        option = 'CHAR_MECA_'//nomopf(k)
                        lpain(1) = 'P'//nompaf(k)
                    else
                        option = 'CHAR_MECA_'//nomopr(k)
                        lpain(1) = 'P'//nompar(k)
                    endif
!
! ----------------- For EFFE_FOND: you need two <CARTE>
!
                    if (option .eq. 'CHAR_MECA_EFON_R') then
                        lpain(16) = 'PPREFFR'
                        lchin(16) = nomcha//'.CHME.PREFF'
                        lpain(1)     = 'PEFOND'
                        lchin(1)     = nomcha//'.CHME.EFOND'
                    endif
                    if (option .eq. 'CHAR_MECA_EFON_F') then
                        lpain(16) = 'PPREFFF'
                        lchin(16) = nomcha//'.CHME.PREFF'
                        lpain(1)     = 'PEFOND'
                        lchin(1)     = nomcha//'.CHME.EFOND'
                    endif
!
                    call gcnco2(newnom)
                    resuel(10:16) = newnom(2:8)
                    call corich('E', resuel, icha, ibid)
!
                    if (nomlig(k) .eq. '.VEASS') then
                        call jeveuo(lchin(1), 'L', jlchin)
                        call copisd('CHAMP_GD', 'V', zk8(jlchin), resuel)
                    else
                        call calcul('S', option, ligrel, nbopt(k), lchin,&
                                    lpain, 1, resuel, paout, 'V',&
                                    'OUI')
                    endif
                    ilve = ilve + 1
                    call reajre(vecele, resuel, 'V')
                endif
                evolch= nomcha//'.CHME.EVOL.CHAR'
                call jeexin(evolch, ier)
                if ((tab(k).eq.1) .or. (ier.gt.0)) then
                    somme = somme + 1
                endif
            end do
            if (somme .eq. 0) then
                call u2mess('F', 'MECANONLINE2_4')
            endif
        endif
!       --TRAITEMENT DE AFFE_CHAR_MECA/EVOL_CHAR
!       ----------------------------------------
!       RESULTATS POSSIBLES
!          1 - VITESSE
        do  ii = 1, 1
            resufv(ii) = resuel
            call gcnco2(newnom)
            resufv(ii) (10:16) = newnom(2:8)
        end do
        call nmvgme(modele, ligrel, carele, charge, icha,&
                    instap, resufv, depmoi, depdel, vites)
        do ii = 1, 1
            call reajre(vecele, resufv(ii), 'V')
        end do
!
    end do
!
99  continue
!
    vecelz = vecele//'.RELR'
    call jedema()
end subroutine
