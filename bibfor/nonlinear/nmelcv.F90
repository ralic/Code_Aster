subroutine nmelcv(phase, modele, ds_contact, mate,&
                  depmoi, depdel, vitmoi, vitplu, accmoi,&
                  vectce)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=4) :: phase
    character(len=19) :: vectce
    character(len=24) :: modele
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: depmoi, depdel, accmoi, vitmoi, vitplu
    character(len=*) :: mate
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - VECTEURS ELEMENTAIRES)
!
! CALCUL DES VECTEURS ELEMENTAIRES DES ELEMENTS DE CONTACT
!
! ----------------------------------------------------------------------
!
! IN  PHASE  : CONTACT OU FROTTEMENT
! In  ds_contact       : datastructure for contact management
! IN  DEPMOI : CHAM_NO DES DEPLACEMENTS A L'INSTANT PRECEDENT
! IN  MODELE : NOM DU MODELE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
! IN  ACCMOI : CHAM_NO DES ACCELERATIONS A L'INSTANT PRECEDENT
! IN  VITMOI : CHAM_NO DES VITESSES A L'INSTANT PRECEDENT
! IN  VITPLU : CHAM_NO DES VITESSES A L'INSTANT SUIVANT
! OUT VECTCE : VECT_ELEM DE CONTACT
!
! ----------------------------------------------------------------------
!
    integer, parameter :: nbout = 1
    integer, parameter :: nbin = 28
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=19) :: chgeom
    character(len=1) :: base
    integer :: ifm, niv
    aster_logical :: debug
    integer :: ifmdbg, nivdbg
    character(len=19) :: ligrel
    character(len=19) :: chmlcf
    character(len=16) :: option
    character(len=19) :: cpoint, cpinte, cainte, ccface
    character(len=19) :: lnno, ltno, stano, fissno, heavno, hea_no, hea_fa
    character(len=19) :: pinter, ainter, cface, faclon, baseco
    character(len=19) :: xdonco, xindco, xseuco, xcohes
    aster_logical :: lctcc, lxfcm, ltfcm, lallv
    character(len=24) :: nosdco
    integer :: jnosdc, jxc
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- TYPE DE CONTACT
!
    lallv = cfdisl(ds_contact%sdcont_defi, 'ALL_VERIF')
    if (lallv) then
        goto 99
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CALCUL DES SECDS MEMBRES ELEMENTAIRES'
    endif
!
! --- INITIALISATIONS
!
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    base = 'V'
!
! --- TYPE DE CO NTACT
!
    lctcc = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    lxfcm = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    ltfcm = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
!
! --- OPTION A CALCULER
!
    if (phase .eq. 'CONT') then
        if(lxfcm) then
            call jeveuo(modele(1:8)//'.XFEM_CONT','L',jxc)
            if(zi(jxc).eq.1.or.zi(jxc).eq.3) option = 'CHAR_MECA_CONT'
            if(zi(jxc).eq.2) option = 'CHAR_MECA_CONT_M'
        else
            option = 'CHAR_MECA_CONT'
        endif
    else if (phase.eq.'FROT') then
        option = 'CHAR_MECA_FROT'
    else
        ASSERT(.false.)
    endif
!
! --- RECUPERATION DE LA GEOMETRIE
!
    call megeom(modele, chgeom)
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- CHOIX DU LIGREL
!
    nosdco = ds_contact%sdcont_solv(1:14)//'.NOSDCO'
    call jeveuo(nosdco, 'L', jnosdc)
    if (lctcc) then
        ligrel = zk24(jnosdc+2-1)(1:19)
    else if (lxfcm) then
        if (ltfcm) then
            ligrel = zk24(jnosdc+3-1)(1:19)
        else
            ligrel = modele(1:8)//'.MODELE'
        endif
    else
        ASSERT(.false.)
    endif
!
! --- INITIALISATIONS DES CHAMPS
!
    cpoint = ' '
    cpinte = ' '
    cainte = ' '
    ccface = ' '
    lnno = ' '
    ltno = ' '
    pinter = ' '
    ainter = ' '
    cface = ' '
    faclon = ' '
    baseco = ' '
    stano = ' '
    fissno = ' '
    heavno = ' '
    xindco = ' '
    xdonco = ' '
    xseuco = ' '
    chmlcf = ' '
    xcohes = ' '
!
! --- CHAMPS METHODE CONTINUE
!
    if (lctcc) then
! ----- CHAM_ELEM POUR ELEMENTS TARDIFS DE CONTACT/FROTTEMENT
        chmlcf = ds_contact%sdcont_solv(1:14)//'.CHML'
    endif
!
! --- CHAMPS METHODE XFEM (PETITS GLISSEMENTS)
!
    if (lxfcm) then
        xindco = ds_contact%sdcont_solv(1:14)//'.XFIN'
        xdonco = ds_contact%sdcont_solv(1:14)//'.XFDO'
        xseuco = ds_contact%sdcont_solv(1:14)//'.XFSE'
        xcohes = ds_contact%sdcont_solv(1:14)//'.XCOH'
        lnno = modele(1:8)//'.LNNO'
        ltno = modele(1:8)//'.LTNO'
        pinter = modele(1:8)//'.TOPOFAC.OE'
        ainter = modele(1:8)//'.TOPOFAC.AI'
        cface = modele(1:8)//'.TOPOFAC.CF'
        faclon = modele(1:8)//'.TOPOFAC.LO'
        baseco = modele(1:8)//'.TOPOFAC.BA'
        stano = modele(1:8)//'.STNO'
        fissno = modele(1:8)//'.FISSNO'
        heavno = modele(1:8)//'.HEAVNO'
        hea_no = modele(1:8)//'.TOPONO.HNO'
        hea_fa = modele(1:8)//'.TOPONO.HFA'
    endif
!
! --- CHAMPS METHODE XFEM (GRANDS GLISSEMENTS)
!
    if (ltfcm) then
        cpoint = ds_contact%sdcont_solv(1:14)//'.XFPO'
        stano = ds_contact%sdcont_solv(1:14)//'.XFST'
        cpinte = ds_contact%sdcont_solv(1:14)//'.XFPI'
        cainte = ds_contact%sdcont_solv(1:14)//'.XFAI'
        ccface = ds_contact%sdcont_solv(1:14)//'.XFCF'
        heavno = ds_contact%sdcont_solv(1:14)//'.XFPL'
        hea_fa = ds_contact%sdcont_solv(1:14)//'.XFHF'
        hea_no = ds_contact%sdcont_solv(1:14)//'.XFHN'
    endif
!
! --- CREATION DES LISTES DES CHAMPS IN ET OUT
! --- GEOMETRIE ET DEPLACEMENTS
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PDEPL_M'
    lchin(2) = depmoi(1:19)
    lpain(3) = 'PDEPL_P'
    lchin(3) = depdel(1:19)
    lpain(4) = 'PVITE_M'
    lchin(4) = vitmoi(1:19)
    lpain(5) = 'PACCE_M'
    lchin(5) = accmoi(1:19)
    lpain(6) = 'PVITE_P'
    lchin(6) = vitplu(1:19)
!
! --- AUTRES CHAMPS IN ET OUT
!
    lpain(7) = 'PCONFR'
    lchin(7) = chmlcf
    lpain(8) = 'PCAR_PT'
    lchin(8) = cpoint
    lpain(9) = 'PCAR_PI'
    lchin(9) = cpinte
    lpain(10) = 'PCAR_AI'
    lchin(10) = cainte
    lpain(11) = 'PCAR_CF'
    lchin(11) = ccface
    lpain(12) = 'PINDCOI'
    lchin(12) = xindco
    lpain(13) = 'PDONCO'
    lchin(13) = xdonco
    lpain(14) = 'PLSN'
    lchin(14) = lnno
    lpain(15) = 'PLST'
    lchin(15) = ltno
    lpain(16) = 'PPINTER'
    lchin(16) = pinter
    lpain(17) = 'PAINTER'
    lchin(17) = ainter
    lpain(18) = 'PCFACE'
    lchin(18) = cface
    lpain(19) = 'PLONGCO'
    lchin(19) = faclon
    lpain(20) = 'PBASECO'
    lchin(20) = baseco
    lpain(21) = 'PSEUIL'
    lchin(21) = xseuco
    lpain(22) = 'PSTANO'
    lchin(22) = stano
    lpain(23) = 'PCOHES'
    lchin(23) = xcohes
    lpain(24) = 'PMATERC'
    lchin(24) = mate
    lpain(25) = 'PFISNO'
    lchin(25) = fissno
    lpain(26) = 'PHEAVNO'
    lchin(26) = heavno
    lpain(27) = 'PHEA_NO'
    lchin(27) = hea_no
    lpain(28) = 'PHEA_FA'
    lchin(28) = hea_fa
!
! --- ON DETRUIT LES VECTEURS ELEMENTAIRES S'ILS EXISTENT
!
    call detrsd('VECT_ELEM', vectce)
!
! --- PREPARATION DES VECTEURS ELEMENTAIRES
!
    call memare('V', vectce, modele, ' ', ' ',&
                'CHAR_MECA')
!
! --- CHAMPS DE SORTIE
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = vectce
!
! --- APPEL A CALCUL
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
    call calcul('S', option, ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, base,&
                'OUI')
    call reajre(vectce, lchout(1), base)
!
 99 continue
!
    call jedema()
!
end subroutine
