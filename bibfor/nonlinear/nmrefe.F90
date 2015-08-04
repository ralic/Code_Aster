subroutine nmrefe(model , compor, mate  , cara_elem, nume_dof,&
                  parcon, valinc, veelem, veasse)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assmiv.h"
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
#include "asterfort/nmchex.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: nume_dof
    real(kind=8), intent(in) :: parcon(*)
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: veelem(*)
    character(len=19), intent(in) :: veasse(*)
!
! ----------------------------------------------------------------------
!
! MECA_NON_LINE - Computation
!
! Compute reference vector for RESI_REFE_RELA
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
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=26)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: nbsig
    parameter    (nbsig=11)
    character(len=8) :: sigere(nbsig)
!
    character(len=19) :: vect_elem, vect_asse, disp_prev
    character(len=19) :: ligrmo, verefe, carte
    character(len=24) :: chgeom
    character(len=24) :: chcara(18)
    character(len=19) :: pintto, cnseto, heavto, loncha, pmilto, hea_no
    character(len=19) :: pinter, ainter, baseco, ccface, lonfac
    aster_logical :: debug
    integer :: ifmdbg, nivdbg
    character(len=16) :: option
!
    data  sigere / 'SIGM','EPSI','FTHERM','FHYDR1','FHYDR2','VARI',&
                   'EFFORT','MOMENT','DEPL','LAG_GV','PI' /
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! - Get names of fields
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(veelem, 'VEELEM', 'CNREFE', vect_elem)
    call nmchex(veasse, 'VEASSE', 'CNREFE', vect_asse)
!
! --- INITIALISATIONS
! 
    carte = '&&NMREFE.SIGERE'
    verefe = '&&NMREFE.VEREFE'
    ligrmo = model(1:8) // '.MODELE'
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
    call megeom(model, chgeom)
!
! --- CARTE POUR LES CARA. ELEM.
!
    call mecara(cara_elem, chcara)
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- RECUPERATION DES DONNEES XFEM (TOPOSE)
!
    pintto = model(1:8)//'.TOPOSE.PIN'
    cnseto = model(1:8)//'.TOPOSE.CNS'
    heavto = model(1:8)//'.TOPOSE.HEA'
    loncha = model(1:8)//'.TOPOSE.LON'
    pmilto = model(1:8)//'.TOPOSE.PMI'
!
! --- RECUPERATION DES DONNEES XFEM (TOPONO)
!
    hea_no = model(1:8)//'.TOPONO.HNO'
!
! --- RECUPERATION DES DONNEES XFEM (TOPOFAC)
!
    pinter = model(1:8)//'.TOPOFAC.OE'
    ainter = model(1:8)//'.TOPOFAC.AI'
    ccface = model(1:8)//'.TOPOFAC.CF'
    baseco = model(1:8)//'.TOPOFAC.BA'
    lonfac = model(1:8)//'.TOPOFAC.LO'
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
    lchin(6) = disp_prev
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
    lchin(14) = model(1:8)//'.BASLOC'
    lpain(15) = 'PLSN'
    lchin(15) = model(1:8)//'.LNNO'
    lpain(16) = 'PLST'
    lchin(16) = model(1:8)//'.LTNO'
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
    lpain(26) = 'PHEA_NO'
    lchin(26) = hea_no
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = verefe
!
! --- PREPARATION DES VECT_ELEM
!
    call detrsd('VECT_ELEM', vect_elem)
    call memare('V', vect_elem, model(1:8), ' ', ' ',&
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
    call reajre(vect_elem, lchout(1), 'V')
!
! - Assembly
!
    call assmiv('V', vect_asse, 1, vect_elem, [1.d0],&
                nume_dof, ' ', 'ZERO', 1)
!
    call jedema()
!
end subroutine
