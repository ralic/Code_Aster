subroutine vefnme(modele, sigma, caraz, depmoi, depdel,&
                  vecelz, matcod, compor, nh, fnoevo,&
                  partps, carcri, chvarc, ligrez, option,&
                  strx)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/gcnco2.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    logical :: fnoevo
    real(kind=8) :: partps(*)
    character(len=*) :: modele, ligrez
    character(len=*) :: sigma, caraz, depmoi, depdel, vecelz
    character(len=*) :: matcod, compor, carcri, chvarc, strx
    character(len=16) :: option
    integer :: nh
!
! ----------------------------------------------------------------------
!
! ROUTINE CALCUL
!
! CALCUL DES VECTEURS ELEMENTAIRES (FORC_NODA)
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE (NECESSAIRE SI SIGMA EST UNE CARTE)
! IN  SIGMA  : NOM DU CHAM_ELEM (OU DE LA CARTE) DE CONTRAINTES
! IN  CARA   : NOM DU CARA_ELEM
! IN  DEPMOI : NOM DU CHAM_NO DE DEPLACEMENTS PRECEDENTS
! IN  DEPDEL : NOM DU CHAM_NO D'INCREMENT DEPLACEMENTS
! IN  MATCOD : NOM DU MATERIAU CODE
! IN  COMPOR : NOM DE LA CARTE DE COMPORTEMENT
! IN  NH     : NUMERO D'HARMONIQUE DE FOURIER
! IN  FNOEVO : VRAI SI FORCES NODALES EVOLUTIVES I E INSTANT PLUS ET
!              MOINS NECESSAIRES 5 STAT NON LINE TRAITANT DES PROBLEMES
!              PARABOLIQUES : APPLICATION A THM
! IN  PARTPS : INSTANT PRECEDENT, ACTUEL ET THETA
! IN  CARCRI : CARTE DES CRITERES ET DE THETA
! IN  CHVARC : NOM DU CHAMP DE VARIABLE DE COMMANDE
! IN  LIGREZ : (SOUS-)LIGREL DE MODELE POUR CALCUL REDUIT
!                  SI ' ', ON PREND LE LIGREL DU MODELE
! OUT VECELZ : VECT_ELEM RESULTAT.
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=33)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=8) :: k8bla, mailla, carele
    character(len=19) :: ligrel
    character(len=8) :: newnom
    character(len=19) :: numhar, tpsmoi, tpsplu
    character(len=19) :: chgeom, chcara(18), vecele
    character(len=16) :: optio2
    logical :: lbid
    integer :: ibid, ied, ier
    real(kind=8) :: instm, instp, rbid
    complex(kind=8) :: cbid
    character(len=19) :: pintto, cnseto, heavto, loncha, basloc, lsn, lst, stano
    character(len=19) :: pmilto, fissno
    logical :: debug
    integer :: ifmdbg, nivdbg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
    carele = caraz
    vecele = vecelz
    ligrel = ligrez
    numhar = '&&VEFNME.NUME_HARM'
    tpsmoi = '&&VEFNME.CH_INSTAM'
    tpsplu = '&&VEFNME.CH_INSTAP'
    k8bla = ' '
    optio2 = option
    if (option .ne. 'FONL_NOEU') optio2 = 'FORC_NODA'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    if (depmoi .ne. ' ') then
        call dismoi('F', 'NOM_MAILLA', depmoi, 'CHAM_NO', ibid,&
                    mailla, ied)
    else if (sigma.ne.' ') then
        call dismoi('F', 'NOM_MAILLA', sigma(1:19), 'CHAM_ELEM', ibid,&
                    mailla, ied)
    else
        call assert(.false.)
    endif
    chgeom = mailla(1:8)//'.COORDO'
    if (vecele .eq. ' ') then
        vecele = '&&VEFNME'
    endif
    if (ligrel .eq. ' ') then
        ligrel = modele(1:8)//'.MODELE'
    endif
!
! --- CARTE POUR LES CARA. ELEM.
!
    call mecara(carele(1:8), lbid, chcara)
!
! --- CARTE POUR LES HARMONIQUES DE FOURIER
!
    call mecact('V', numhar, 'MAILLA', mailla, 'HARMON',&
                1, 'NH', nh, rbid, cbid,&
                k8bla)
!
! --- CARTE DES INSTANTS POUR THM
!
    if (fnoevo) then
        instm = partps(1)
        instp = partps(2)
        call mecact('V', tpsmoi, 'MAILLA', mailla, 'INST_R',&
                    1, 'INST', ibid, instm, cbid,&
                    k8bla)
        call mecact('V', tpsplu, 'MAILLA', mailla, 'INST_R',&
                    1, 'INST', ibid, instp, cbid,&
                    k8bla)
    endif
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = matcod
    lpain(3) = 'PCAGNPO'
    lchin(3) = chcara(6)
    lpain(4) = 'PCAORIE'
    lchin(4) = chcara(1)
    lpain(5) = 'PCOMPOR'
    lchin(5) = compor
    lpain(6) = 'PCONTMR'
    lchin(6) = sigma
    lpain(7) = 'PDEPLMR'
    lchin(7) = depmoi
    lpain(8) = 'PDEPLPR'
    lchin(8) = depdel
    lpain(9) = 'PCAARPO'
    lchin(9) = chcara(9)
    lpain(10) = 'PCADISK'
    lchin(10) = chcara(2)
    lpain(11) = 'PCACOQU'
    lchin(11) = chcara(7)
    lpain(12) = 'PHARMON'
    lchin(12) = numhar
    lpain(13) = 'PCAMASS'
    lchin(13) = chcara(12)
    lpain(14) = 'PCARCRI'
    lchin(14) = carcri
    lpain(15) = 'PINSTMR'
    lchin(15) = tpsmoi
    lpain(16) = 'PINSTPR'
    lchin(16) = tpsplu
    lpain(17) = 'PVARCPR'
    lchin(17) = chvarc
    lpain(18) = 'PCAGEPO'
    lchin(18) = chcara(5)
    lpain(19) = 'PNBSP_I'
    lchin(19) = chcara(16)
    lpain(20) = 'PFIBRES'
    lchin(20) = chcara(17)
!
! --- CADRE X-FEM
!
    call exixfe(modele, ier)
    if (ier .ne. 0) then
        pintto =modele(1:8)//'.TOPOSE.PIN'
        cnseto =modele(1:8)//'.TOPOSE.CNS'
        heavto =modele(1:8)//'.TOPOSE.HEA'
        loncha =modele(1:8)//'.TOPOSE.LON'
        pmilto =modele(1:8)//'.TOPOSE.PMI'
        basloc =modele(1:8)//'.BASLOC'
        lsn =modele(1:8)//'.LNNO'
        lst =modele(1:8)//'.LTNO'
        stano = modele(1:8)//'.STNO'
        fissno = modele(1:8)//'.FISSNO'
    else
        pintto = '&&VEFNME.PINTTO.BID'
        cnseto = '&&VEFNME.CNSETO.BID'
        heavto = '&&VEFNME.HEAVTO.BID'
        loncha = '&&VEFNME.LONCHA.BID'
        basloc = '&&VEFNME.BASLOC.BID'
        pmilto = '&&VEFNME.PMILTO.BID'
        lsn = '&&VEFNME.LNNO.BID'
        lst = '&&VEFNME.LTNO.BID'
        stano = '&&VEFNME.STNO.BID'
        fissno = '&&VEFNME.FISSNO.BID'
    endif
!
    lpain(22) = 'PPINTTO'
    lchin(22) = pintto
    lpain(23) = 'PCNSETO'
    lchin(23) = cnseto
    lpain(24) = 'PHEAVTO'
    lchin(24) = heavto
    lpain(25) = 'PLONCHA'
    lchin(25) = loncha
    lpain(26) = 'PBASLOR'
    lchin(26) = basloc
    lpain(27) = 'PLSN'
    lchin(27) = lsn
    lpain(28) = 'PLST'
    lchin(28) = lst
    lpain(29) = 'PSTANO'
    lchin(29) = stano
    lpain(30) = 'PCINFDI'
    lchin(30) = chcara(15)
    lpain(31) = 'PPMILTO'
    lchin(31) = pmilto
    lpain(32) = 'PFISNO'
    lchin(32) = fissno
    lpain(33) = 'PSTRXMR'
    lchin(33) = strx
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PVECTUR'
    call gcnco2(newnom)
    lchout(1) = '&&VEFNME.???????'
    lchout(1)(10:16) = newnom(2:8)
    call corich('E', lchout(1), -1, ibid)
!
! --- PREPARATION DU VECT_ELEM RESULTAT
!
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele, ' ', carele,&
                'CHAR_MECA')
!
    if (debug) then
        call dbgcal(optio2, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
!
! --- APPEL A CALCUL
!
    call calcul('S', optio2, ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
!
!
    call reajre(vecele, lchout(1), 'V')
!
    vecelz = vecele//'.RELR'
!
! --- MENAGE
!
    call detrsd('CHAMP_GD', numhar)
    call detrsd('CHAMP_GD', tpsmoi)
    call detrsd('CHAMP_GD', tpsplu)
    call jedema()
end subroutine
