subroutine vefnme(option, base  , model , mate      , carele  ,&
                  compor, partps, nh    , ligrelz   , varicomz,&
                  sigmaz, strxz , deplz , depl_incrz, vecelz)
!
    implicit none
!
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
    character(len=16), intent(in) :: option
    character(len=1), intent(in) :: base
    character(len=8), intent(in) :: model
    real(kind=8), intent(in) :: partps(*)
    character(len=24), intent(in) :: carele
    character(len=24), intent(in) :: mate
    character(len=*), intent(in) :: ligrelz
    integer, intent(in) :: nh
    character(len=19), intent(in) :: compor
    character(len=*), intent(in) :: sigmaz
    character(len=*), intent(in) :: varicomz
    character(len=*), intent(in) :: strxz
    character(len=*), intent(in) :: deplz
    character(len=*), intent(in) :: depl_incrz
    character(len=*), intent(inout) :: vecelz
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Option: FORC_NODA
!         FONL_NOEU
!
! --------------------------------------------------------------------------------------------------
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
! IN  PARTPS : INSTANT PRECEDENT ET ACTUEL
! IN  CARCRI : CARTE DES CRITERES ET DE THETA
! IN  CHVARC : NOM DU CHAMP DE VARIABLE DE COMMANDE
! IN  LIGREZ : (SOUS-)LIGREL DE MODELE POUR CALCUL REDUIT
!                  SI ' ', ON PREND LE LIGREL DU MODELE
! OUT VECELZ : VECT_ELEM RESULTAT.
!
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=31)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=8) :: k8bla, mesh
    character(len=8) :: newnom
    character(len=19) :: numhar, tpsmoi, tpsplu, ligrel_local, ligrel
    character(len=19) :: chgeom, chcara(18), vecele
    character(len=16) :: optio2
    logical :: lbid
    integer :: ibid, iret
    real(kind=8) :: instm, instp, rbid
    complex(kind=8) :: cbid
    character(len=19) :: pintto, cnseto, heavto, loncha, basloc, lsn, lst, stano
    character(len=19) :: pmilto, fissno
    character(len=19) :: sigma, varicom, strx
    character(len=19) :: depl, depl_incr
    logical :: debug
    integer :: ifmdbg, nivdbg
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! - Initializations
!
    sigma     = sigmaz
    varicom   = varicomz
    strx      = strxz
    depl      = deplz
    depl_incr = depl_incrz
    ligrel    = ligrelz
    newnom = '.0000000'
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
!
! - Get mesh
!
    if (depl .ne. ' ') then
        call dismoi('F', 'NOM_MAILLA', depl , 'CHAM_NO', ibid,&
                    mesh, iret)
    else if (sigma.ne.' ') then
        call dismoi('F', 'NOM_MAILLA', sigma, 'CHAM_ELEM', ibid,&
                    mesh, iret)
    else
        ASSERT(.false.)
    endif
    chgeom = mesh(1:8)//'.COORDO'
!
! - VECT_ELEM name
!
    vecele = vecelz
    if (vecele .eq. ' ') then
        vecele = '&&VEFNME'
    endif
    if (ligrel .eq. ' ') then
        ligrel_local = model(1:8)//'.MODELE'
    else
        ligrel_local = ligrel
    endif
!
! - <CARTE> for structural elements
!
    call mecara(carele(1:8), lbid, chcara)
!
! - <CARTE> for Fourier mode
!
    call mecact('V', numhar, 'MAILLA', mesh, 'HARMON',&
                ncmp=1, nomcmp='NH', si=nh)
!
! - <CARTE> for instant
!
    instm = partps(1)
    instp = partps(2)
    call mecact('V', tpsmoi, 'MAILLA', mesh, 'INST_R',&
                ncmp=1, nomcmp='INST', sr=instm)
    call mecact('V', tpsplu, 'MAILLA', mesh, 'INST_R',&
                ncmp=1, nomcmp='INST', sr=instp)
!
! - Init fields
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PCAGNPO'
    lchin(3) = chcara(6)
    lpain(4) = 'PCAORIE'
    lchin(4) = chcara(1)
    lpain(5) = 'PCOMPOR'
    lchin(5) = compor
    lpain(6) = 'PCONTMR'
    lchin(6) = sigma
    lpain(7) = 'PDEPLMR'
    lchin(7) = depl
    lpain(8) = 'PDEPLPR'
    lchin(8) = depl_incr
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
    lpain(14) = 'PINSTMR'
    lchin(14) = tpsmoi
    lpain(15) = 'PINSTPR'
    lchin(15) = tpsplu
    lpain(16) = 'PVARCPR'
    lchin(16) = varicom
    lpain(17) = 'PCAGEPO'
    lchin(17) = chcara(5)
    lpain(18) = 'PNBSP_I'
    lchin(18) = chcara(16)
    lpain(19) = 'PFIBRES'
    lchin(19) = chcara(17)
!
! --- CADRE X-FEM
!
    call exixfe(model, iret)
    if (iret .ne. 0) then
        pintto = model(1:8)//'.TOPOSE.PIN'
        cnseto = model(1:8)//'.TOPOSE.CNS'
        heavto = model(1:8)//'.TOPOSE.HEA'
        loncha = model(1:8)//'.TOPOSE.LON'
        pmilto = model(1:8)//'.TOPOSE.PMI'
        basloc = model(1:8)//'.BASLOC'
        lsn    = model(1:8)//'.LNNO'
        lst    = model(1:8)//'.LTNO'
        stano  = model(1:8)//'.STNO'
        fissno = model(1:8)//'.FISSNO'
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
    lpain(20) = 'PPINTTO'
    lchin(20) = pintto
    lpain(21) = 'PCNSETO'
    lchin(21) = cnseto
    lpain(22) = 'PHEAVTO'
    lchin(22) = heavto
    lpain(23) = 'PLONCHA'
    lchin(23) = loncha
    lpain(24) = 'PBASLOR'
    lchin(24) = basloc
    lpain(25) = 'PLSN'
    lchin(25) = lsn
    lpain(26) = 'PLST'
    lchin(26) = lst
    lpain(27) = 'PSTANO'
    lchin(27) = stano
    lpain(28) = 'PCINFDI'
    lchin(28) = chcara(15)
    lpain(29) = 'PPMILTO'
    lchin(29) = pmilto
    lpain(30) = 'PFISNO'
    lchin(30) = fissno
    lpain(31) = 'PSTRXMR'
    lchin(31) = strx
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PVECTUR'
    call gcnco2(newnom)
    lchout(1) = vecele(1:8)//newnom
    call corich('E', lchout(1), -1, ibid)
!
! --- PREPARATION DU VECT_ELEM RESULTAT
!
    call detrsd('VECT_ELEM', vecele)
    call memare(base, vecele, model, ' ', carele,&
                'CHAR_MECA')
!
    if (debug) then
        call dbgcal(optio2, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
! - APPEL A CALCUL
!
    call calcul('S', optio2, ligrel_local, nbin, lchin,&
                lpain, nbout, lchout, lpaout, base,&
                'OUI')
    call reajre(vecele, lchout(1), base)
!
    vecelz = vecele//'.RELR'
!
! - Cleaning
!
    call detrsd('CHAMP_GD', numhar)
    call detrsd('CHAMP_GD', tpsmoi)
    call detrsd('CHAMP_GD', tpsplu)
    call jedema()
end subroutine
