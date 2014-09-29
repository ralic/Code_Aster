subroutine nmrefe(modele, compor, mate, carele, depmoi,&
                  parcon, vecelz)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    real(kind=8) :: parcon(*)
    character(len=*) :: vecelz
    character(len=24) :: modele
    character(len=24) :: compor
    character(len=24) :: mate
    character(len=24) :: carele
    character(len=19) :: depmoi
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DE LA CARTE POUR RESI_RELA_REFE POUR UN ELEMENT CABLE/GAINE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE MECANIQUE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  DEPMOI : DEPLACEMENT EN T-
! IN  PARCON : PARAMETRES DU CRITERE DE CONVERGENCE REFERENCE
!                     1 : SIGM_REFE
!                     2 : EPSI_REFE
!                     3 : FLUX_THER_REFE
!                     4 : FLUX_HYD1_REFE
!                     5 : FLUX_HYD2_REFE
!                     6 : VARI_REFE
!                     7 : EFFORT (FORC_REFE)
!                     8 : MOMENT (FORC_REFE)
!                     9 : DEPL_REFE
!                    10 : LAGR_REFE
! OUT VECELZ : NOM DU VECT_ELEM
!
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=25)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: nbsig
    parameter    (nbsig=11)
    character(len=8) :: sigere(nbsig)
!
    character(len=19) :: vecele
    character(len=19) :: ligrmo, verefe, carte
    character(len=24) :: chgeom
    character(len=24) :: chcara(18)
    character(len=19) :: pintto, cnseto, heavto, loncha, pmilto
    character(len=19) :: pinter, ainter, baseco, ccface, lonfac
    aster_logical :: debug
    integer :: ifmdbg, nivdbg
    character(len=16) :: option
!
    data  sigere / 'SIGM','EPSI','FTHERM','FHYDR1','FHYDR2','VARI',&
     &               'EFFORT','MOMENT','DEPL','LAG_GV','PI' /
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    carte = '&&NMREFE.SIGERE'
    verefe = '&&NMREFE.VEREFE'
    vecele = vecelz
    ligrmo = modele(1:8) // '.MODELE'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    option = 'REFE_FORC_NODA'
!
! --- CREATION CARTE DES VALEURS DE REFRENCES
!
    call mecact('V', carte, 'MODELE', ligrmo, 'PREC',&
                ncmp=nbsig, lnomcmp=sigere, vr=parcon)
!
! --- CARTE DE LA GEOMETRIE
!
    call megeom(modele, chgeom)
!
! --- CARTE POUR LES CARA. ELEM.
!
    call mecara(carele(1:8), chcara)
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- RECUPERATION DES DONNEES XFEM (TOPOSE)
!
    pintto = modele(1:8)//'.TOPOSE.PIN'
    cnseto = modele(1:8)//'.TOPOSE.CNS'
    heavto = modele(1:8)//'.TOPOSE.HEA'
    loncha = modele(1:8)//'.TOPOSE.LON'
    pmilto = modele(1:8)//'.TOPOSE.PMI'
!
! --- RECUPERATION DES DONNEES XFEM (TOPOFAC)
!
    pinter = modele(1:8)//'.TOPOFAC.OE'
    ainter = modele(1:8)//'.TOPOFAC.AI'
    ccface = modele(1:8)//'.TOPOFAC.CF'
    baseco = modele(1:8)//'.TOPOFAC.BA'
    lonfac = modele(1:8)//'.TOPOFAC.LO'
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PREFCO'
    lchin(2) = carte
    lpain(3) = 'PCAORIE'
    lchin(3) = chcara(1)(1:19)
    lpain(4) = 'PCOMPOR'
    lchin(4) = compor(1:19)
    lpain(5) = 'PMATERC'
    lchin(5) = mate(1:19)
    lpain(6) = 'PDEPLMR'
    lchin(6) = depmoi
    lpain(7) = 'PCACOQU'
    lchin(7) = chcara(7)(1:19)
    lpain(8) = 'PCAGEPO'
    lchin(8) = chcara(5)(1:19)
    lpain(9) = 'PNBSP_I'
    lchin(9) = chcara(1) (1:8)//'.CANBSP'
    lpain(10) = 'PPINTTO'
    lchin(10) = pintto
    lpain(11) = 'PHEAVTO'
    lchin(11) = heavto
    lpain(12) = 'PLONCHA'
    lchin(12) = loncha
    lpain(13) = 'PCNSETO'
    lchin(13) = cnseto
    lpain(14) = 'PBASLOR'
    lchin(14) = modele(1:8)//'.BASLOC'
    lpain(15) = 'PLSN'
    lchin(15) = modele(1:8)//'.LNNO'
    lpain(16) = 'PLST'
    lchin(16) = modele(1:8)//'.LTNO'
    lpain(17) = 'PCAMASS'
    lchin(17) = chcara(12) (1:19)
    lpain(18) = 'PPMILTO'
    lchin(18) = pmilto
    lpain(19) = 'PCINFDI'
    lchin(19) = chcara(15)(1:19)
    lpain(20) = 'PPINTER'
    lchin(20) = pinter
    lpain(21) = 'PAINTER'
    lchin(21) = ainter
    lpain(22) = 'PCFACE'
    lchin(22) = ccface
    lpain(23) = 'PBASECO'
    lchin(23) = baseco
    lpain(24) = 'PLONFA'
    lchin(24) = lonfac
    lpain(25) = 'PCAGNBA'
    lchin(25) = chcara(11)(1:19)
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = verefe
!
! --- PREPARATION DES VECT_ELEM
!
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), ' ', ' ',&
                'CHAR_MECA')
!
! --- APPEL A CALCUL
!
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call reajre(vecele, lchout(1), 'V')
!
    call jedema()
!
end subroutine
