subroutine merifs(modele, nchar, lchar, mate, cara,&
                  exitim, time, matel, nh)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecham.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    include 'asterfort/vrcins.h'
    integer :: nchar, nh
    real(kind=8) :: time
    character(len=8) :: modele, cara
    character(len=19) :: matel
    character(len=*) :: lchar(*), mate
    logical :: exitim
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     CALCUL DES MATRICES ELEMENTAIRES DE RIGIDITE MECA
!      OPTION 'RIGI_FLUI_STRU'.
! ----------------------------------------------------------------------
! IN  : MODELE : NOM DU MODELE  (PAS OBLIGATOIRE)
! IN  : NCHAR  : NOMBRE DE CHARGES
! IN  : LCHAR  : LISTE DES CHARGES
! IN  : MATE   : CARTE DE MATERIAU
! IN  : CARA   : CHAMP DE CARAC_ELEM
! IN  : MATEL  : NOM DU MATR_ELEM RESULTAT
! IN  : EXITIM : VRAI SI L'INSTANT EST DONNE
! IN  : TIME   : INSTANT DE CALCUL
! IN  : NH     : NUMERO D'HARMONIQUE DE FOURIER
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=2) :: codret
    character(len=8) :: lpain(15), lpaout(1)
    character(len=16) :: option
    character(len=19) :: chvarc
    character(len=24) :: ligrmo, ligrch, lchin(15), lchout(10)
    character(len=24) :: chgeom, chcara(18), chharm
!-----------------------------------------------------------------------
    integer :: iarefe, icha, icode, ilires, iret, iret1
!-----------------------------------------------------------------------
    data chvarc /'&&MERIFS.CHVARC'/
!
    call jemarq()
    option = 'RIGI_FLUI_STRU'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, icode)
!
    call vrcins(modele, mate, cara, time, chvarc,&
                codret)
!
    call memare('G', matel, modele, mate, cara,&
                option)
!     SI LA RIGIDITE EST CALCULEE SUR LE MODELE, ON ACTIVE LES S_STRUC:
    call jeveuo(matel//'.RERR', 'E', iarefe)
    zk24(iarefe-1+3) (1:3) = 'OUI'
!
    call jeexin(matel//'.RELR', iret1)
    if (iret1 .gt. 0) call jedetr(matel//'.RELR')
    ilires = 0
!
    lpaout(1) = 'PMATUUR'
    lchout(1) = matel(1:8)//'.ME001'
!
    if (icode .eq. 0) then
        ligrmo = modele//'.MODELE'
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PCAORIE'
        lchin(3) = chcara(1)
        lpain(4) = 'PCADISK'
        lchin(4) = chcara(2)
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
        lpain(10) = 'PHARMON'
        lchin(10) = chharm
        lpain(11) = 'PABSCUR'
        lchin(11) = chgeom(1:8)//'.ABS_CURV'
        lpain(12) = 'PNBSP_I'
        lchin(12) = chcara(16)
        lpain(13) = 'PFIBRES'
        lchin(13) = chcara(17)
        lpain(14) = 'PCOMPOR'
        lchin(14) = mate(1:8)//'.COMPOR'
        lpain(15) = 'PCINFDI'
        lchin(15) = chcara(15)
        call calcul('S', option, ligrmo, 15, lchin,&
                    lpain, 1, lchout, lpaout, 'G',&
                    'OUI')
        call reajre(matel, lchout(1), 'G')
        ilires=ilires+1
    endif
!
    do 10 icha = 1, nchar
        ligrch = lchar(icha) (1:8)//'.CHME.LIGRE'
        call jeexin(lchar(icha) (1:8)//'.CHME.LIGRE.LIEL', iret)
        if (iret .le. 0) goto 10
        lchin(1) = lchar(icha) (1:8)//'.CHME.CMULT'
        call exisd('CHAMP_GD', lchar(icha) (1:8)//'.CHME.CMULT', iret)
        if (iret .le. 0) goto 10
!
        lpain(1) = 'PDDLMUR'
        ilires=ilires+1
        call codent(ilires, 'D0', lchout(1) (12:14))
        option = 'MECA_DDLM_R'
        call calcul('S', option, ligrch, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'G',&
                    'OUI')
        call reajre(matel, lchout(1), 'G')
10  end do
! --- MENAGE
    call detrsd('CHAM_ELEM', chvarc)
!
    call jedema()
end subroutine
