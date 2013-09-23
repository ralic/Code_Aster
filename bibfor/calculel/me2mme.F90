subroutine me2mme(modelz, nchar, lchar, mate, caraz,&
                  exitim, time, matelz, nh, basez)
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
! aslint: disable=W1501
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/meharm.h"
#include "asterfort/memare.h"
#include "asterfort/nmdepr.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/reajre.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: modele, cara, kbid, lcmp(5)
    character(len=*) :: modelz, caraz, matelz, lchar(*), mate, basez
    character(len=19) :: matel
    real(kind=8) :: time
    logical :: exitim, lfonc
    integer :: nchar
! ----------------------------------------------------------------------
!
!     CALCUL DES SECONDS MEMBRES ELEMENTAIRES
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELZ : NOM DU MODELE
!        NCHAR  : NOMBRE DE CHARGES
!        LCHAR  : LISTE DES CHARGES
!        MATE   : CHAMP DE MATERIAUX
!        CARAZ  : CHAMP DE CARAC_ELEM
!        MATELZ : NOM DU MATEL (N RESUELEM) PRODUIT
!        NH     : NUMERO DE L'HARMONIQUE DE FOURIER
!        BASEZ  : NOM DE LA BASE
!
!        EXITIM : VRAI SI L'INSTANT EST DONNE
!        TIME   : INSTANT DE CALCUL
!
!     SORTIES:
!     SONT TRAITES LES CHARGEMENTS :
!        LCHAR(ICHA)//'.CHME.CIMPO'
!        LCHAR(ICHA)//'.CHME.FORNO'
!        LCHAR(ICHA)//'.CHME.F3D3D'
!        LCHAR(ICHA)//'.CHME.FCO2D'
!        LCHAR(ICHA)//'.CHME.FCO3D'
!        LCHAR(ICHA)//'.CHME.F2D3D'
!        LCHAR(ICHA)//'.CHME.F1D3D'
!        LCHAR(ICHA)//'.CHME.F2D2D'
!        LCHAR(ICHA)//'.CHME.F1D2D'
!        LCHAR(ICHA)//'.CHME.F1D1D'
!        LCHAR(ICHA)//'.CHME.PESAN'
!        LCHAR(ICHA)//'.CHME.ROTAT'
!        LCHAR(ICHA)//'.CHME.FELEC'
!        LCHAR(ICHA)//'.CHME.FL1??'
!        LCHAR(ICHA)//'.CHME.PRESS'
!        LCHAR(ICHA)//'.CHME.EPSIN'
!        LCHAR(ICHA)//'.CHME.TEMPE'
!        LCHAR(ICHA)//'.CHME.VNOR'
!        LCHAR(ICHA)//'.CHME.ONDE'
!        LCHAR(ICHA)//'.CHME.EVOL.CHAR'
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    logical :: exicar
    character(len=1) :: base
    character(len=2) :: codret
    integer :: nbin
!-----------------------------------------------------------------------
    integer :: ibid, icha, ier, ierd, ifla, ilires, iret
    integer :: j, jveass, nh
!-----------------------------------------------------------------------
    parameter(nbin=43)
    character(len=8) :: lpain(nbin), lpaout(1), noma, exiele
    character(len=16) :: option
    character(len=19) :: pintto, cnseto, heavto, loncha, basloc, lsn, lst, stano
    character(len=19) :: pmilto, fissno, pinter
    character(len=24) :: chgeom, lchin(nbin), lchout(1), kcmp(5)
    character(len=24) :: ligrmo, ligrch, chtime, chlapl, chcara(18)
    character(len=24) :: chharm
    character(len=19) :: chvarc, chvref
!
    character(len=19) :: resufv(3)
    character(len=24) :: charge
    integer :: jad, i
    logical :: ltemp, ltref
!
!
    call jemarq()
    modele=modelz
    cara=caraz
    matel=matelz
    base=basez
!
    do 10,i=1,nbin
    lchin(i)=' '
    lpain(i)=' '
    10 end do
!
    chvarc='&&ME2MME.VARC'
    chvref='&&ME2MME.VARC.REF'
!
!
!     -- CALCUL DE .RERR:
    call memare(base, matel, modele, mate, cara,&
                'CHAR_MECA')
!
!     -- S'IL N' Y A PAS D'ELEMENTS CLASSIQUES, ON RESSORT:
    call dismoi('F', 'EXI_ELEM', modele, 'MODELE', ibid,&
                exiele, ierd)
!
!
!     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
!        ET CHAM_MATER :
!
    call megeom(modele, chgeom)
    call mecara(cara, exicar, chcara)
!
!     LES CHAMPS "IN" PRIS DANS CARA_ELEM SONT NUMEROTES DE 21 A 32 :
!     ---------------------------------------------------------------
    lpain(21)='PCAARPO'
    lchin(21)=chcara(9)
    lpain(22)='PCACOQU'
    lchin(22)=chcara(7)
    lpain(23)='PCADISM'
    lchin(23)=chcara(3)
    lpain(24)='PCAGEPO'
    lchin(24)=chcara(5)
    lpain(25)='PCAGNBA'
    lchin(25)=chcara(11)
    lpain(26)='PCAGNPO'
    lchin(26)=chcara(6)
    lpain(27)='PCAMASS'
    lchin(27)=chcara(12)
    lpain(28)='PCAORIE'
    lchin(28)=chcara(1)
    lpain(29)='PCASECT'
    lchin(29)=chcara(8)
    lpain(30)='PCINFDI'
    lchin(30)=chcara(15)
    lpain(31)='PFIBRES'
    lchin(31)=chcara(17)
    lpain(32)='PNBSP_I'
    lchin(32)=chcara(16)
    ilires=0
!
!  ---VERIFICATION DE L'EXISTENCE D'UN MODELE X-FEM-------
    call exixfe(modele, ier)
!
    if (ier .ne. 0) then
!
!  ---  CAS DU MODELE X-FEM-----------
!
        ilires=ilires+1
        pintto=modele(1:8)//'.TOPOSE.PIN'
        cnseto=modele(1:8)//'.TOPOSE.CNS'
        heavto=modele(1:8)//'.TOPOSE.HEA'
        loncha=modele(1:8)//'.TOPOSE.LON'
        pmilto=modele(1:8)//'.TOPOSE.PMI'
        basloc=modele(1:8)//'.BASLOC'
        lsn=modele(1:8)//'.LNNO'
        lst=modele(1:8)//'.LTNO'
        stano=modele(1:8)//'.STNO'
        fissno=modele(1:8)//'.FISSNO'
        pinter=modele(1:8)//'.TOPOFAC.OE'
    else
        pintto='&&ME2MME.PINTTO.BID'
        cnseto='&&ME2MME.CNSETO.BID'
        heavto='&&ME2MME.HEAVTO.BID'
        loncha='&&ME2MME.LONCHA.BID'
        basloc='&&ME2MME.BASLOC.BID'
        pmilto='&&ME2MME.PMILTO.BID'
        lsn='&&ME2MME.LNNO.BID'
        lst='&&ME2MME.LTNO.BID'
        stano='&&ME2MME.STNO.BID'
        fissno='&&ME2MME.FISSNO.BID'
        pinter='&&ME2MME.PINTER.BID'
    endif
!
    if (ier .ne. 0) then
        lpain(33)='PPINTTO'
        lchin(33)=pintto
        lpain(34)='PHEAVTO'
        lchin(34)=heavto
        lpain(35)='PLONCHA'
        lchin(35)=loncha
        lpain(36)='PCNSETO'
        lchin(36)=cnseto
        lpain(37)='PBASLOR'
        lchin(37)=basloc
        lpain(38)='PLSN'
        lchin(38)=lsn
        lpain(39)='PLST'
        lchin(39)=lst
        lpain(40)='PSTANO'
        lchin(40)=stano
        lpain(41)='PPMILTO'
        lchin(41)=pmilto
        lpain(42)='PFISNO'
        lchin(42)=fissno
        lpain(43)='PPINTER'
        lchin(43)=pinter
    endif
! ----- REMPLISSAGE DES CHAMPS D'ENTREE
!
!
    noma=chgeom(1:8)
    call vrcins(modele, mate, cara, time, chvarc,&
                codret)
    call vrcref(modele, mate(1:8), cara, chvref(1:19))
!
    if (nchar .eq. 0) goto 60
    call jeexin(matel//'.RELR', iret)
    if (iret .gt. 0) call jedetr(matel//'.RELR')
!
    lpaout(1)='PVECTUR'
    lchout(1)=matel(1:8)//'.VEXXX'
    lpain(1)='PGEOMER'
    lchin(1)=chgeom
    lpain(2)='PMATERC'
    lchin(2)=mate
    lpain(3)='PVARCPR'
    lchin(3)=chvarc
!
    ifla=0
!
    ligrmo=modele//'.MODELE'
!
!        -- EN PRINCIPE, EXITIM EST TOUJOURS .TRUE.
    chtime='&&ME2MME.CH_INST_R'
    call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                ncmp=1, nomcmp='INST   ',sr=time)
    lpain(5)='PTEMPSR'
    lchin(5)=chtime
!
!
!
    do 50 icha = 1, nchar
        call dismoi('F', 'TYPE_CHARGE', lchar(icha), 'CHARGE', ibid,&
                    kbid, ierd)
        if (kbid(5:7) .eq. '_FO') then
            lfonc=.true.
        else
            lfonc=.false.
        endif
        ligrch=lchar(icha)//'.CHME.LIGRE'
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.CIMPO', iret)
        if (iret .ne. 0) then
!
            if (lfonc) then
                option='MECA_DDLI_F'
                lpain(4)='PDDLIMF'
            else
                option='MECA_DDLI_R'
                lpain(4)='PDDLIMR'
            endif
            lchin(4)=ligrch(1:13)//'.CIMPO.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrch, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.FORNO', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_FORC_F'
                lpain(4)='PFORNOF'
            else
                option='CHAR_MECA_FORC_R'
                lpain(4)='PFORNOR'
            endif
            lchin(4)=ligrch(1:13)//'.FORNO.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrch, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
!
!      -- SI LE MODELE NE CONTIENT PAS D'ELEMENTS CLASSIQUES, ON SAUTE:
!      ----------------------------------------------------------------
        if (exiele(1:3) .eq. 'NON') goto 50
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.F3D3D', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_FF3D3D'
                lpain(4)='PFF3D3D'
            else
                option='CHAR_MECA_FR3D3D'
                lpain(4)='PFR3D3D'
            endif
            lchin(4)=ligrch(1:13)//'.F3D3D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.FCO2D', iret)
        if (iret .ne. 0) then
!
            if (lfonc) then
                option='CHAR_MECA_FFCO2D'
                lpain(4)='PFFCO2D'
            else
                option='CHAR_MECA_FRCO2D'
                lpain(4)='PFRCO2D'
            endif
            lchin(4)=ligrch(1:13)//'.FCO2D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.FCO3D', iret)
        if (iret .ne. 0) then
!
            if (lfonc) then
                option='CHAR_MECA_FFCO3D'
                lpain(4)='PFFCO3D'
            else
                option='CHAR_MECA_FRCO3D'
                lpain(4)='PFRCO3D'
            endif
            lchin(4)=ligrch(1:13)//'.FCO3D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.F2D3D', iret)
        if (iret .ne. 0) then
!
            if (lfonc) then
                option='CHAR_MECA_FF2D3D'
                lpain(4)='PFF2D3D'
            else
                option='CHAR_MECA_FR2D3D'
                lpain(4)='PFR2D3D'
            endif
            lchin(4)=ligrch(1:13)//'.F2D3D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.F1D3D', iret)
        if (iret .ne. 0) then
!
            if (lfonc) then
                option='CHAR_MECA_FF1D3D'
                lpain(4)='PFF1D3D'
            else
                option='CHAR_MECA_FR1D3D'
                lpain(4)='PFR1D3D'
            endif
            lchin(4)=ligrch(1:13)//'.F1D3D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.F2D2D', iret)
        if (iret .ne. 0) then
!
            if (lfonc) then
                option='CHAR_MECA_FF2D2D'
                lpain(4)='PFF2D2D'
            else
                option='CHAR_MECA_FR2D2D'
                lpain(4)='PFR2D2D'
            endif
            lchin(4)=ligrch(1:13)//'.F2D2D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.F1D2D', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_FF1D2D'
                lpain(4)='PFF1D2D'
            else
                option='CHAR_MECA_FR1D2D'
                lpain(4)='PFR1D2D'
            endif
            lchin(4)=ligrch(1:13)//'.F1D2D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.F1D1D', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_FF1D1D'
                lpain(4)='PFF1D1D'
            else
                option='CHAR_MECA_FR1D1D'
                lpain(4)='PFR1D1D'
            endif
            lchin(4)=ligrch(1:13)//'.F1D1D.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.PESAN', iret)
        if (iret .ne. 0) then
            option='CHAR_MECA_PESA_R'
            lpain(2)='PMATERC'
            lchin(2)=mate
            lpain(4)='PPESANR'
            lchin(4)=ligrch(1:13)//'.PESAN.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.ROTAT', iret)
        if (iret .ne. 0) then
            option='CHAR_MECA_ROTA_R'
            lpain(2)='PMATERC'
            lchin(2)=mate
            lpain(4)='PROTATR'
            lchin(4)=ligrch(1:13)//'.ROTAT.DESC'
            lpain(15)='PCOMPOR'
            lchin(15)=mate(1:8)//'.COMPOR'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.EPSIN', iret)
        if (iret .ne. 0) then
            lpain(2)='PMATERC'
            lchin(2)=mate
            if (lfonc) then
                option='CHAR_MECA_EPSI_F'
                lpain(4)='PEPSINF'
            else
                option='CHAR_MECA_EPSI_R'
                lpain(4)='PEPSINR'
            endif
            lchin(4)=ligrch(1:13)//'.EPSIN.DESC'
            call meharm(modele, nh, chharm)
            lpain(13)='PHARMON'
            lchin(13)=chharm
            lpain(15)='PCOMPOR'
            lchin(15)=mate(1:8)//'.COMPOR'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.FELEC', iret)
        if (iret .ne. 0) then
            option='CHAR_MECA_FRELEC'
            lpain(4)='PFRELEC'
            lchin(4)=ligrch(1:13)//'.FELEC.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
!         -- LA BOUCLE 30 SERT A TRAITER LES FORCES ELECTRIQUES LAPLACE
!
        do 20 j = 1, 99
            lchin(13)(1:17)=ligrch(1:13)//'.FL1'
            call codent(j, 'D0', lchin(13)(18:19))
            lchin(13)=lchin(13)(1:19)//'.DESC'
            call jeexin(lchin(13), iret)
            if (iret .eq. 0) goto 30
            lpain(12)='PHARMON'
            lchin(12)=' '
            lpain(13)='PLISTMA'
            if (ifla .eq. 0) then
                chlapl='&&ME2MME.CH_FLAPLA'
                lcmp(1)='NOMAIL'
                lcmp(2)='NOGEOM'
                kcmp(1)=noma
                kcmp(2)=chgeom(1:19)
                call mecact('V', chlapl, 'MAILLA', noma, 'FLAPLA  ',&
                            ncmp=2, lnomcmp=lcmp(1), vk=kcmp(1))
                ifla=1
            endif
            option='CHAR_MECA_FRLAPL'
            lpain(4)='PFLAPLA'
            lchin(4)=chlapl
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
20      continue
30      continue
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.PRESS', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_PRES_F'
                lpain(4)='PPRESSF'
            else
                option='CHAR_MECA_PRES_R'
                lpain(4)='PPRESSR'
            endif
            lchin(4)=ligrch(1:13)//'.PRESS.DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.VNOR ', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_VNOR_F'
                lpain(4)='PSOURCF'
            else
                option='CHAR_MECA_VNOR'
                lpain(4)='PSOURCR'
            endif
            lchin(4)=ligrch(1:13)//'.VNOR .DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call jeexin(ligrch(1:13)//'.VEASS', iret)
        if (iret .gt. 0) then
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call jeveuo(ligrch(1:13)//'.VEASS', 'L', jveass)
            call copisd('CHAMP_GD', base, zk8(jveass), lchout(1))
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
        call exisd('CHAMP_GD', ligrch(1:13)//'.ONDE ', iret)
        if (iret .ne. 0) then
            if (lfonc) then
                option='CHAR_MECA_ONDE_F'
                lpain(4)='PONDECF'
            else
                option='CHAR_MECA_ONDE'
                lpain(4)='PONDECR'
            endif
!
            lchin(4)=ligrch(1:13)//'.ONDE .DESC'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
! ====================================================================
! CHARGE DE TYPE EVOL_CHAR
!
        charge='&&ME2MME.INTERF.NMDEPR'
        call wkvect(charge, 'V V K24', 1, jad)
        zk24(jad)=lchar(icha)
!
        resufv(1)='&&ME2MME.VE001'
        resufv(2)='&&ME2MME.VE002'
        resufv(3)='&&ME2MME.VE003'
!
        call nmdepr(modele, ligrmo, cara, charge, 1,&
                    time, resufv)
!
        do 40 i = 1, 3
            call exisd('CHAMP_GD', resufv(i), iret)
            if (iret .ne. 0) then
                ilires=ilires+1
                call codent(ilires, 'D0', lchout(1)(12:14))
                call copisd('CHAMP_GD', base, resufv(i), lchout(1))
                call detrsd('CHAMP_GD', resufv(i))
                call reajre(matel, lchout(1), base)
            endif
40      continue
        call jedetr(charge)
!
! ====================================================================
! CHARGE DE TYPE ONDE_PLANE :
        call exisd('CHAMP_GD', ligrch(1:13)//'.ONDPL', iret)
        if (iret .ne. 0) then
            option='ONDE_PLAN'
            lpain(4)='PONDPLA'
            lchin(4)=ligrch(1:13)//'.ONDPL'
            lpain(6)='PONDPLR'
            lchin(6)=ligrch(1:13)//'.ONDPR'
            ilires=ilires+1
            call codent(ilires, 'D0', lchout(1)(12:14))
            call calcul('S', option, ligrmo, 6, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
            call reajre(matel, lchout(1), base)
        endif
!
! ====================================================================
50  end do
!
!
! ====================================================================
!       -- CHARGEMENT DE DILATATION THERMIQUE :
    call nmvcd2('TEMP', mate, ltemp, ltref)
    if (ltemp) then
        call vrcins(modele, mate, cara, time, chvarc,&
                    codret)
        option='CHAR_MECA_TEMP_R'
        lpain(2)='PMATERC'
        lchin(2)=mate
        lpain(4)='PVARCRR'
        lchin(4)=chvref
        call meharm(modele, nh, chharm)
        lpain(13)='PHARMON'
        lchin(13)=chharm
        lpain(16)=' '
        lchin(16)=' '
        ilires=ilires+1
        call codent(ilires, 'D0', lchout(1)(12:14))
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, 1, lchout, lpaout, base,&
                    'OUI')
        call reajre(matel, lchout(1), base)
        call detrsd('CHAMP_GD', chvarc)
    endif
!
!
!
!
60  continue
    call detrsd('CHAMP_GD', chvarc)
    call detrsd('CHAMP_GD', chvref)
!
    call jedema()
end subroutine
