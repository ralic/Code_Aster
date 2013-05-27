subroutine metnth(modele, lchar, cara, mate, time,&
                  chtni, metrnl)
!
!
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
!
!     ARGUMENTS:
!     ----------
    implicit none
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: lchar, mate
    character(len=24) :: modele, cara, metrnl, time, chtni
! ----------------------------------------------------------------------
!
!     CALCUL DES MATRICES ELEMENTAIRES DE CONVECTION NATURELLE
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELE : NOM DU MODELE
!        LCHAR  : OBJET CONTENANT LA LISTE DES CHARGES
!        MATE   : CHAMP DE MATERIAUX
!        CARA   : CHAMP DE CARAC_ELEM
!        TIME   : CHAMPS DE TEMPSR
!        CHTNI  : IEME ITEREE DU CHAMP DE TEMPERATURE
!        METRNL : NOM DU MATR_ELEM (N RESUELEM) PRODUIT
!
!     SORTIES:
!        METRNL  : EST REMPLI.
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
!
    character(len=8) :: nomcha, lpain(7), lpaout(1), k8bid
    character(len=8) :: vitess, decent
    character(len=16) :: option
    character(len=24) :: lchin(7), lchout(1), chgeom, chcara(18)
    character(len=24) :: chvite, ligrmo, carte, convch, carele
    integer :: iret, ilires
    integer :: nchar, jchar
    complex(kind=8) :: cbid
    logical :: exicar
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ichar, iconv, jvites
!-----------------------------------------------------------------------
    call jemarq()
!     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
    if (modele(1:1) .eq. ' ') then
        call u2mess('F', 'CALCULEL3_50')
    endif
!
    call jeexin(lchar, iret)
    if (iret .ne. 0) then
        call jelira(lchar, 'LONMAX', nchar, k8bid)
        call jeveuo(lchar, 'L', jchar)
    else
        nchar = 0
    endif
!
    call megeom(modele, chgeom)
    call mecara(cara, exicar, chcara)
!
    call jeexin(metrnl, iret)
    if (iret .eq. 0) then
        metrnl = '&&METNTH           .RELR'
        call memare('V', metrnl, modele(1:8), mate, carele,&
                    'RIGI_THER_CONV_T')
    else
        call jedetr(metrnl)
    endif
!
    chvite = '????'
!
    iconv = 0
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = metrnl(1:8)//'.ME000'
    do 10 ichar = 1, nchar
        nomcha = zk24(jchar+ichar-1) (1:8)
        convch = nomcha//'.CHTH'//'.CONVE'//'.VALE'
        call jeexin(convch, iret)
        if (iret .gt. 0) then
            iconv = iconv + 1
            if (iconv .gt. 1) call u2mess('F', 'CALCULEL3_72')
!
            decent = 'OUI'
            option = 'RIGI_THER_CONV'
            if (decent .eq. 'OUI') option = 'RIGI_THER_CONV_T'
            call memare('V', metrnl, modele(1:8), mate, cara,&
                        option)
!
            call jeveuo(convch, 'L', jvites)
            vitess = zk8(jvites)
            chvite = vitess
            carte = '&&METNTH'//'.CONVECT.DECENT'
            call mecact('V', carte, 'MODELE', modele(1:8)//'.MODELE', 'NEUT_K24',&
                        1, 'Z1', 0, 0.d0, cbid,&
                        decent)
            lpain(1) = 'PGEOMER'
            lchin(1) = chgeom
            lpain(2) = 'PMATERC'
            lchin(2) = mate
            lpain(3) = 'PCACOQU'
            lchin(3) = chcara(7)
            lpain(4) = 'PTEMPSR'
            lchin(4) = time
            lpain(5) = 'PVITESR'
            lchin(5) = chvite
            lpain(7) = 'PNEUK24'
            lchin(7) = carte
            lpain(6) = 'PTEMPEI'
            lchin(6) = chtni
!
!
            ligrmo = modele(1:8)//'.MODELE'
            ilires = 0
            ilires = ilires + 1
            call codent(ilires, 'D0', lchout(1) (12:14))
            call calcul('S', option, ligrmo, 7, lchin,&
                        lpain, 1, lchout, lpaout, 'V',&
                        'OUI')
            call reajre(metrnl, lchout(1), 'V')
!
        endif
10  end do
!
    call jedema()
!
end subroutine
