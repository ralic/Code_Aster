subroutine merimp(modele, carele, mate, comref, compor,&
                  carcri, iterat, sddyna, valinc, solalg,&
                  caco3d, nbin, lpain, lchin)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesvar.h"
#include "asterfort/copisd.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmvcex.h"
    integer :: iterat
    character(len=*) :: mate
    character(len=19) :: sddyna, solalg(*)
    character(len=24) :: modele, carele, compor, comref
    character(len=24) :: carcri, caco3d
    character(len=19) :: valinc(*)
    integer :: nbin
    character(len=8) :: lpain(nbin)
    character(len=19) :: lchin(nbin)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! PREPARATION DES CHAMPS D'ENTREE POUR RIGIDITE TANGENTE (MERIMO)
!
! ----------------------------------------------------------------------
!
!
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
!
!
!
    integer :: ibid
    integer :: iret, ier
    logical :: exicar
    complex(kind=8) :: c16bid
    character(len=8) :: k8bid
    character(len=24) :: chgeom, chcara(18), chiter
    character(len=19) :: depent, vitent
    character(len=19) :: depmoi, sigmoi, varmoi, commoi, strmoi
    character(len=19) :: depplu, sigplu, varplu, complu, strplu
    character(len=19) :: insmoi, vrcmoi
    character(len=19) :: insplu, vrcplu
    character(len=24) :: vrcref
    character(len=24) :: varmoj, strmoj
    character(len=19) :: depkm1, vitkm1, acckm1
    character(len=19) :: vitplu, accplu, romkm1, romk
    character(len=24) :: ligrmo
    character(len=19) :: ddepla, depdel, k19bla
    character(len=19) :: pintto, cnseto, heavto, loncha, basloc, lsn, lst, stano
    character(len=19) :: stadyn, pmilto, fissno
    logical :: ldyna
    integer :: ifm, niv
    real(kind=8) :: iter
!
    data varmoj/'&&MERIMO.VARMOJ'/
    data strmoj/'&&MERIMO.STRMOJ'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    ligrmo = modele(1:8)//'.MODELE'
    chiter = '&&MERIMO.CH_ITERAT'
    ASSERT(mate(9:18).eq.'.MATE_CODE')
    k19bla = ' '
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(valinc, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(valinc, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(valinc, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(valinc, 'VALINC', 'ROMK  ', romk)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmchex(valinc, 'VALINC', 'STRPLU', strplu)
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'SIGMOI', sigmoi)
    call nmchex(valinc, 'VALINC', 'VARMOI', varmoi)
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'STRMOI', strmoi)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
!
! --- ACCES AUX OBJETS DE LA SD SDDYNA
!
    if (ldyna) then
        call ndynkk(sddyna, 'DEPENT', depent)
        call ndynkk(sddyna, 'VITENT', vitent)
        call ndynkk(sddyna, 'STADYN', stadyn)
    else
        depent = k19bla
        vitent = k19bla
        stadyn = k19bla
    endif
!
! --- CADRE X-FEM
!
    call exixfe(modele, ier)
    if (ier .ne. 0) then
        pintto = modele(1:8)//'.TOPOSE.PIN'
        cnseto = modele(1:8)//'.TOPOSE.CNS'
        heavto = modele(1:8)//'.TOPOSE.HEA'
        loncha = modele(1:8)//'.TOPOSE.LON'
        pmilto = modele(1:8)//'.TOPOSE.PMI'
        basloc = modele(1:8)//'.BASLOC'
        lsn = modele(1:8)//'.LNNO'
        lst = modele(1:8)//'.LTNO'
        stano = modele(1:8)//'.STNO'
        fissno = modele(1:8)//'.FISSNO'
    else
        pintto = '&&MERIMO.PINTTO.BID'
        cnseto = '&&MERIMO.CNSETO.BID'
        heavto = '&&MERIMO.HEAVTO.BID'
        loncha = '&&MERIMO.LONCHA.BID'
        basloc = '&&MERIMO.BASLOC.BID'
        pmilto = '&&MERIMO.PMILTO.BID'
        lsn = '&&MERIMO.LNNO.BID'
        lst = '&&MERIMO.LTNO.BID'
        stano = '&&MERIMO.STNO.BID'
        fissno = '&&MERIMO.FISSNO.BID'
    endif
!
! --- LECTURE DES VARIABLES DE COMMANDE EN T- ET T+ ET VAL. DE REF
!
    call nmvcex('TOUT', commoi, vrcmoi)
    call nmvcex('INST', commoi, insmoi)
    call nmvcex('TOUT', complu, vrcplu)
    call nmvcex('INST', complu, insplu)
    call nmvcex('TOUT', comref, vrcref)
!
! --- VARIABLES INTERNES ISSUES DE L'ITERATION PRECEDENTE
!
    call exisd('CHAMP_GD', varplu(1:19), iret)
    if (iret .ne. 0) then
        call copisd('CHAMP_GD', 'V', varplu(1:19), varmoj(1:19))
    else
        call copisd('CHAMP_GD', 'V', varmoi(1:19), varmoj(1:19))
    endif
!
! --- VARIABLES DE STRUCTURE ISSUES DE L'ITERATION PRECEDENTE
!
    call exisd('CHAMP_GD', strplu(1:19), iret)
    if (iret .ne. 0) then
        call copisd('CHAMP_GD', 'V', strplu(1:19), strmoj(1:19))
    else
        call copisd('CHAMP_GD', 'V', strmoi(1:19), strmoj(1:19))
    endif
!
! --- CREATION DU CHAM_ELEM_S POUR ETENDRE LE CHAM_ELEM DE VARI_R
!
    call exisd('CHAM_ELEM_S', compor, iret)
    if (iret .eq. 0) call cesvar(carele, compor, ligrmo, compor)
    call copisd('CHAM_ELEM_S', 'V', compor, varplu)
    call copisd('CHAM_ELEM_S', 'V', compor, sigplu)
!
! --- CHAMP DE GEOMETRIE
!
    call megeom(modele, chgeom)
!
! --- CHAMP DE CARACTERISTIQUES ELEMENTAIRES
!
    call mecara(carele(1:8), exicar, chcara)
!
! --- CHAMP POUR NUMERO DE L'ITERATION
!
    iter = iterat
    call mecact('V', chiter, 'MODELE', ligrmo, 'NEUT_R',&
                1, 'X1', ibid, iter, c16bid,&
                k8bid)
!
! --- REMPLISSAGE DES CHAMPS D'ENTREE
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PCONTMR'
    lchin(3) = sigmoi(1:19)
    lpain(4) = 'PVARIMR'
    lchin(4) = varmoi(1:19)
    lpain(5) = 'PCOMPOR'
    lchin(5) = compor(1:19)
    lpain(6) = 'PDEPLMR'
    lchin(6) = depmoi(1:19)
    lpain(7) = 'PDEPLPR'
    lchin(7) = depdel(1:19)
    lpain(8) = 'PCACABL'
    lchin(8) = chcara(10)(1:19)
    lpain(9) = 'PINSTMR'
    lchin(9) = insmoi(1:19)
    lpain(10) = 'PINSTPR'
    lchin(10) = insplu(1:19)
    lpain(11) = 'PCARCRI'
    lchin(11) = carcri(1:19)
    lpain(12) = ' '
    lchin(12) = ' '
    lpain(13) = ' '
    lchin(13) = ' '
    lpain(14) = 'PCAGNPO'
    lchin(14) = chcara(6)(1:19)
    lpain(15) = 'PCAORIE'
    lchin(15) = chcara(1)(1:19)
    lpain(16) = 'PCADISK'
    lchin(16) = chcara(2)(1:19)
    lpain(17) = 'PCACOQU'
    lchin(17) = chcara(7)(1:19)
    lpain(18) = 'PITERAT'
    lchin(18) = chiter(1:19)
    lpain(19) = 'PDDEPLA'
    lchin(19) = ddepla(1:19)
    lpain(20) = 'PDEPKM1'
    lchin(20) = depkm1(1:19)
    lpain(21) = 'PVITKM1'
    lchin(21) = vitkm1(1:19)
    lpain(22) = 'PACCKM1'
    lchin(22) = acckm1(1:19)
    lpain(23) = 'PVITPLU'
    lchin(23) = vitplu(1:19)
    lpain(24) = 'PACCPLU'
    lchin(24) = accplu(1:19)
    lpain(25) = 'PROMKM1'
    lchin(25) = romkm1(1:19)
    lpain(26) = 'PROMK'
    lchin(26) = romk(1:19)
    lpain(27) = 'PSTADYN'
    lchin(27) = stadyn(1:19)
    lpain(28) = 'PVARIMP'
    lchin(28) = varmoj(1:19)
    lpain(29) = 'PCAGNBA'
    lchin(29) = chcara(11)(1:19)
    lpain(30) = 'PCAMASS'
    lchin(30) = chcara(12)(1:19)
    lpain(31) = 'PCAGEPO'
    lchin(31) = chcara(5)(1:19)
    lpain(32) = 'PDEPENT'
    lchin(32) = depent(1:19)
    lpain(33) = 'PVITENT'
    lchin(33) = vitent(1:19)
    lpain(34) = 'PVARCMR'
    lchin(34) = vrcmoi(1:19)
    lpain(35) = 'PVARCPR'
    lchin(35) = vrcplu(1:19)
    lpain(36) = 'PNBSP_I'
    lchin(36) = chcara(16)(1:19)
    lpain(37) = 'PFIBRES'
    lchin(37) = chcara(17)(1:19)
    lpain(38) = 'PCINFDI'
    lchin(38) = chcara(15)(1:19)
    lpain(39) = ' '
    lchin(39) = ' '
    lpain(40) = 'PVARCRR'
    lchin(40) = vrcref(1:19)
    lpain(42) = 'PPINTTO'
    lchin(42) = pintto
    lpain(43) = 'PHEAVTO'
    lchin(43) = heavto
    lpain(44) = 'PLONCHA'
    lchin(44) = loncha
    lpain(45) = 'PCNSETO'
    lchin(45) = cnseto
    lpain(46) = 'PBASLOR'
    lchin(46) = basloc
    lpain(47) = 'PCACO3D'
    lchin(47) = caco3d(1:19)
    lpain(48) = 'PLSN'
    lchin(48) = lsn
    lpain(49) = 'PLST'
    lchin(49) = lst
    lpain(50) = ' '
    lchin(50) = ' '
    lpain(51) = 'PSTANO'
    lchin(51) = stano
    lpain(52) = 'PCAARPO'
    lchin(52) = chcara(9)(1:19)
    lpain(53) = 'PPMILTO'
    lchin(53) = pmilto
    lpain(54) = 'PFISNO'
    lchin(54) = fissno
    lpain(55) = 'PSTRXMR'
    lchin(55) = strmoi(1:19)
    lpain(56) = 'PSTRXMP'
    lchin(56) = strmoj(1:19)
!
    call jedema()
end subroutine
