subroutine memame(option, modele, nchar, lchar, mate,&
                  carele, exitim, instan, compor, matelz,&
                  base)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecham.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/redetr.h"
#include "asterfort/vrcins.h"
    integer :: nchar
    real(kind=8) :: instan
    character(len=8) :: lchar(*)
    character(len=1) :: base
    character(len=*) :: option, modele, mate, carele, compor, matelz
    aster_logical :: exitim
!
! ----------------------------------------------------------------------
!
! ROUTINE CALCUL
!
! CALCUL DES MATRICES ELEMENTAIRES DE MASSE MECA
!
! ----------------------------------------------------------------------
!
!
! IN   OPTION : OPTION DE CALCUL
! IN   MODELE : NOM DU MODELE (OBLIGATOIRE)
! IN   NCHAR  : NOMBRE DE CHARGES
! IN   LCHAR  : LISTE DES CHARGES
! IN   MATE   : CARTE DE MATERIAUX
! IN   CARELE : CHAMP DE CARAC_ELEM
! IN   EXITIM : VRAI SI L'INSTANT EST DONNE
! IN   INSTAN : INSTANT DE CALCUL
! IN   COMPOR : CARTE DE COMPORTEMENT
! IN   BASEZ  : NOM DE LA BASE
! OUT  MATELE : NOM DU MATR_ELEM RESULTAT
! IN   BASE   : NOM DE LA BASE
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=2, nbin=18)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=24) :: lchout(nbout), lchin(nbin)
    character(len=19) :: pintto, cnseto, heavto, loncha, basloc, lsn, lst, stano
    character(len=19) :: pmilto, hea_no
!
    character(len=2) :: codret
    character(len=19) :: chvarc, matele
    character(len=24) :: ligrmo
    character(len=24) :: chgeom, chcara(18), chharm
    integer :: nbout2
    integer :: nh, iret, icode
    integer :: ifmdbg, nivdbg, ier
    aster_logical :: debug
    character(len=24), pointer :: rerr(:) => null()
    data chvarc /'&&MEMAME.VARC'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    matele = matelz
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    nh = 0
    if (modele(1:1) .eq. ' ') then
        ASSERT(.false.)
    else
        ligrmo = modele(1:8)//'.MODELE'
    endif
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- CREATION DES CHAMPS GEOM. CARAC_ELEM ET HARM_FOURIER
!
    call mecham(option, modele, carele, nh, chgeom,&
                chcara, chharm, icode)
!
! --- CREATION DU CHAMP DE VARIABLES DE COMMANDE
!
    call vrcins(modele, mate, carele, instan, chvarc,&
                codret)
!
! --- PREPARATION DES MATR_ELEM
!
    call memare(base, matele, modele, mate, carele,&
                option)
    call jeveuo(matele(1:19)//'.RERR', 'E', vk24=rerr)
    rerr(3) (1:3) = 'OUI'
    call jeexin(matele(1:19)//'.RELR', iret)
    if (iret .gt. 0) call jedetr(matele(1:19)//'.RELR')
    if (icode .eq. 1) goto 10
!
!  -----VERIFICATION DE L'EXISTENCE D'UN MODELE X-FEM------
    call exixfe(modele, ier)
    if (ier .ne. 0) then
!
!  -----CAS DU MODELE X-FEM-----------------------
!
        pintto = modele(1:8)//'.TOPOSE.PIN'
        cnseto = modele(1:8)//'.TOPOSE.CNS'
        heavto = modele(1:8)//'.TOPOSE.HEA'
        loncha = modele(1:8)//'.TOPOSE.LON'
        pmilto = modele(1:8)//'.TOPOSE.PMI'
        hea_no = modele(1:8)//'.TOPONO.HNO'
        basloc = modele(1:8)//'.BASLOC'
        lsn = modele(1:8)//'.LNNO'
        lst = modele(1:8)//'.LTNO'
        stano = modele(1:8)//'.STNO'
!
! ----- REMPLISSAGE DES CHAMPS D'ENTREE
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PPINTTO'
        lchin(3) = pintto
        lpain(4) = 'PHEAVTO'
        lchin(4) = heavto
        lpain(5) = 'PLONCHA'
        lchin(5) = loncha
        lpain(6) = 'PCNSETO'
        lchin(6) = cnseto
        lpain(7) = 'PBASLOR'
        lchin(7) = basloc
        lpain(8) = 'PLSN'
        lchin(8) = lsn
        lpain(9) = 'PLST'
        lchin(9) = lst
        lpain(10) = 'PSTANO'
        lchin(10) = stano
        lpain(11) = 'PHEA_NO'
        lchin(11) = hea_no
        lpain(12) = 'PPMILTO'
        lchin(12) = pmilto
!
! --- CHAMPS DE SORTIE
!
        lpaout(1) = 'PMATUUR'
        lchout(1) = matele(1:15)//'.M01'
!
        call calcul('S', option, ligrmo, 11, lchin,&
                    lpain, 1, lchout, lpaout, base,&
                    'OUI')
        call reajre(matelz, lchout(1), base)
!
    else if (ier.eq.0) then
!
!----------CAS FEM CLASSIQUE-------------------------
!
! ----- CHAMPS D'ENTREE
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PCAORIE'
        lchin(3) = chcara(1)
        lpain(4) = 'PCADISM'
        lchin(4) = chcara(3)
        lpain(5) = 'PCAGNPO'
        lchin(5) = chcara(6)
        lpain(6) = 'PCACOQU'
        lchin(6) = chcara(7)
        lpain(7) = 'PCASECT'
        lchin(7) = chcara(8)
        lpain(8) = 'PVARCPR'
        lchin(8) = chvarc
        lpain(9) = 'PCAARPO'
        lchin(9) = chcara(9)
        lpain(10) = 'PCACABL'
        lchin(10) = chcara(10)
        lpain(11) = 'PCAGEPO'
        lchin(11) = chcara(5)
        lpain(12) = 'PABSCUR'
        lchin(12) = chgeom(1:8)//'.ABSC_CURV'
        lpain(13) = 'PCAGNBA'
        lchin(13) = chcara(11)
        lpain(14) = 'PCAPOUF'
        lchin(14) = chcara(13)
        lpain(15) = 'PCOMPOR'
        lchin(15) = compor
        lpain(16) = 'PNBSP_I'
        lchin(16) = chcara(16)
        lpain(17) = 'PFIBRES'
        lchin(17) = chcara(17)
        lpain(18) = 'PCINFDI'
        lchin(18) = chcara(15)
!
! ----- CHAMPS DE SORTIE
!
        lpaout(1) = 'PMATUUR'
        lchout(1) = matele(1:15)//'.M01'
        lpaout(2) = 'PMATUNS'
        lchout(2) = matele(1:15)//'.M02'
        if (option .eq. 'MASS_MECA') then
            nbout2 = 2
        else
            nbout2 = 1
        endif
!
! ----- APPEL A CALCUL
!
        if (debug) then
            call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                        nbout2, lpaout, lchout)
        endif
!
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout2, lchout, lpaout, base,&
                    'OUI')
!
! ----- STOCKAGE DES RESU_ELEM
!
        call reajre(matelz, lchout(1), base)
        call reajre(matelz, lchout(2), base)
    endif
!
 10 continue
!
!     -- DESTRUCTION DES RESUELEM NULS :
    call redetr(matelz)
!
    call detrsd('CHAMP_GD', chvarc)
!
    call jedema()
end subroutine
