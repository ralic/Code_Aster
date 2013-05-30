subroutine mertth(modele, charge, infcha, carele, mate,&
                  inst, chtn, chtni, merigi)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
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
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    character(len=24) :: modele, charge, infcha, carele, inst, chtn, chtni
    character(len=24) :: merigi, mate
! ----------------------------------------------------------------------
! CALCUL DES MATRICES ELEMENTAIRES DE RIGIDITE EN TRANSPORT
! THERMIQUE NON LINEAIRE
!  - TERMES DE VOLUME
!  - TERMES DE SURFACE DUS AUX CONDITIONS LIMITES
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : MATERIAU CODE
! IN  INST    : CARTE CONTENANT LA VALEUR DE L'INSTANT
! IN  CHTN    : CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
! IN  CHTNI   : IEME ITEREE DU CHAMP DE TEMPERATURE
! OUT MERIGI  : MATRICES ELEMENTAIRES
!
!
!
    character(len=8) :: nomcha, lpain(6), lpaout(1), k8bid
    character(len=16) :: option
    character(len=24) :: ligrel(2), lchin(6), lchout(1)
    character(len=24) :: chgeom, chcara(18)
    integer :: iret, nchar, ilires, icha, jchar, jinf
    logical :: exicar
! ----------------------------------------------------------------------
    integer :: nbchmx
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
    parameter (nbchmx=4)
    integer :: nbopt(nbchmx), nligr(nbchmx)
    character(len=6) :: nomopr(nbchmx), nomopf(nbchmx), nomchp(nbchmx)
    character(len=7) :: nompar(nbchmx), nompaf(nbchmx)
    data nomchp/'.COEFH','.FLUNL','.HECHP','.COEFH'/
    data nomopr/'COEH_R','      ','PARO_R','COET_R'/
    data nomopf/'COEH_F','FLUTNL','PARO_F','COET_F'/
    data nompar/'PCOEFHR','       ','PHECHPR','PCOEFHR'/
    data nompaf/'PCOEFHF','PFLUXNL','PHECHPF','PCOEFHF'/
    data nbopt/3,4,5,3/
    data nligr/1,1,2,1/
! DEB ------------------------------------------------------------------
    call jemarq()
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar, k8bid)
        call jeveuo(charge, 'L', jchar)
    else
        nchar = 0
    endif
!
    call megeom(modele, chgeom)
    call mecara(carele, exicar, chcara)
!
    call jeexin(merigi, iret)
    if (iret .eq. 0) then
        merigi = '&&METRIG           .RELR'
        call memare('V', merigi, modele(1:8), mate, carele,&
                    'RIGI_THER')
    else
        call jedetr(merigi)
    endif
!
    ligrel(1) = modele(1:8)//'.MODELE'
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = merigi(1:8)//'.ME001'
    ilires = 0
!
    if (modele .ne. '        ') then
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PCACOQU'
        lchin(3) = chcara(7)
        lpain(4) = 'PTEMPSR'
        lchin(4) = inst
        lpain(5) = 'PTEMPER'
        lchin(5) = chtn
        lpain(6) = 'PTEMPEI'
        lchin(6) = chtni
        option = 'RIGI_THER_TRANS'
        ilires = ilires + 1
        call codent(ilires, 'D0', lchout(1) (12:14))
        call calcul('S', option, ligrel(1), 6, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(merigi, lchout(1), 'V')
!
    endif
!
    if (nchar .gt. 0) then
        call jeveuo(infcha, 'L', jinf)
        do 20 icha = 1, nchar
            if (zi(jinf+nchar+icha) .gt. 0) then
                nomcha = zk24(jchar+icha-1) (1:8)
                ligrel(2) = nomcha//'.CHTH.LIGRE'
                lpain(1) = 'PGEOMER'
                lchin(1) = chgeom
                lpain(3) = 'PTEMPSR'
                lchin(3) = inst
                lpain(4) = 'PTEMPER'
                lchin(4) = chtni
                lpain(5) = 'PDEPLAR'
                lchin(5) = '&&DEPPLU'
                lpaout(1) = 'PMATTTR'
                lchout(1) = merigi(1:8)//'.ME001'
!
                do 10 k = 1, nbchmx
                    lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH'// nomchp(k)// '.DESC'
                    call jeexin(lchin(2), iret)
                    if (iret .gt. 0) then
                        if (zi(jinf+nchar+icha) .eq. 1) then
                            option = 'RIGI_THER_'//nomopr(k)
                            lpain(2) = nompar(k)
                            else if (zi(jinf+nchar+icha).eq.2 .or.&
                        zi(jinf+nchar+icha).eq.3) then
                            option = 'RIGI_THER_'//nomopf(k)
                            lpain(2) = nompaf(k)
                        endif
                        if (option(11:14) .eq. 'PARO') then
                            lpain(3) = 'PTEMPSR'
                            lchin(3) = '&&OP0171.TIMEMO'
                        endif
                        if (k .eq. 2) lchin(4) = chtni
                        ilires = ilires + 1
                        call codent(ilires, 'D0', lchout(1) (12:14))
                        call calcul('S', option, ligrel(nligr(k)), nbopt( k), lchin,&
                                    lpain, 1, lchout, lpaout, 'V',&
                                    'OUI')
                        call reajre(merigi, lchout(1), 'V')
                    endif
10              continue
            endif
!
20      continue
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
