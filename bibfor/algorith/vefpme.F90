subroutine vefpme(modele, carele, mate, chargz, infchz,&
                  partps, templu, lvechz, ligrez)
    implicit none
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/copisd.h'
    include 'asterfort/corich.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/exixfe.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    character(len=*) :: modele, carele, mate, chargz, infchz
    character(len=*) :: templu, lvechz, ligrez
    character(len=19) :: lvechp
    real(kind=8) :: partps(*)
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
!
!     CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS MECANIQUES
!     DE NEUMANN NON SUIVEURS ET PILOTABLES.
!
! IN  MODELE  : NOM DU MODELE
! IN  CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : MATERIAU
! IN  CHARGZ  : LISTE DES CHARGES
! IN  INFCHZ  : INFORMATIONS SUR LES CHARGEMENTS
! IN  PARTPS  : TABLEAU DONNANT T+, DELTAT ET THETA (POUR LE THM)
! IN  TEMPLU  : CHAMP DE TEMPERATURE A L'INSTANT T+
! IN  LIGREZ  : (SOUS-)LIGREL DE MODELE POUR CALCUL REDUIT
!                  SI ' ', ON PREND LE LIGREL DU MODELE
! VAR LVECHZ  : VECT_ELEM
!
!
!
!
!-----------------------------------------------------------------------
    integer :: jlchin, nbchmx
!-----------------------------------------------------------------------
    parameter (nbchmx=13)
    integer :: jchar, jinf, jtyp
    integer :: ibid, iret, nchar, k, icha, numchm, ier, nchin
    character(len=5) :: suffix
    character(len=6) :: nomlig(nbchmx), nompaf(nbchmx), nompar(nbchmx)
    character(len=6) :: nomopf(nbchmx), nomopr(nbchmx)
    character(len=7) :: nomcmp(3)
    character(len=8) :: nomcha, lpain(27), lpaout(1), k8bid, newnom
    character(len=16) :: option
    character(len=24) :: chgeom, chcara(18), chtime, ligrel
    character(len=24) :: ligrmo, ligrch, lchin(27), lchout(1)
    character(len=24) :: charge, infcha
    logical :: exicar, bidon
    complex(kind=8) :: cbid
!
    data nomlig/'.FORNO','.F3D3D','.F2D3D','.F1D3D','.F2D2D','.F1D2D',&
     &   '.F1D1D','.PRESS','.FCO3D','.FCO2D','.FLUX','.PESAN','.VEASS'/
    data nomopf/'FORC_F','FF3D3D','FF2D3D','FF1D3D','FF2D2D','FF1D2D',&
     &   'FF1D1D','PRES_F','FFCO3D','FFCO2D','FLUX_F','PESA_R','     '/
    data nompaf/'FORNOF','FF3D3D','FF2D3D','FF1D3D','FF2D2D','FF1D2D',&
     &   'FF1D1D','PRESSF','FFCO3D','FFCO2D','FLUXF','PESANR','      '/
    data nomopr/'FORC_R','FR3D3D','FR2D3D','FR1D3D','FR2D2D','FR1D2D',&
     &   'FR1D1D','PRES_R','FRCO3D','FRCO2D','FLUX_R','PESA_R','     '/
    data nompar/'FORNOR','FR3D3D','FR2D3D','FR1D3D','FR2D2D','FR1D2D',&
     &    'FR1D1D','PRESSR','FRCO3D','FRCO2D','FLUXR','PESANR','     '/
!
!
    call jemarq()
    newnom = '.0000000'
    charge = chargz
    infcha = infchz
    lvechp = lvechz
    ligrmo = ligrez
    if (ligrmo .eq. ' ') ligrmo = modele(1:8)//'.MODELE'
    if (lvechp .eq. ' ') lvechp = '&&VEMFPI           '
!
    bidon = .true.
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar, k8bid)
        if (nchar .ne. 0) then
            bidon = .false.
            call jeveuo(charge, 'L', jchar)
            call jeveuo(infcha, 'L', jinf)
        endif
    endif
!
    call detrsd('VECT_ELEM', lvechp)
    call memare('V', lvechp, modele, mate, carele,&
                'CHAR_MECA')
    call reajre(lvechp, ' ', 'V')
    if (bidon) goto 30
!
    call megeom(modele, chgeom)
    call mecara(carele, exicar, chcara)
!
    chtime = '&&VEFPME.CH_INST_R'
    nomcmp(1) = 'INST   '
    nomcmp(2) = 'DELTAT '
    nomcmp(3) = 'THETA  '
    call mecact('V', chtime, 'LIGREL', ligrmo, 'INST_R  ',&
                3, nomcmp, ibid, partps, cbid,&
                k8bid)
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
    lpain(3) = 'PTEMPSR'
    lchin(3) = chtime
    lpain(4) = 'PMATERC'
    lchin(4) = mate
    lpain(5) = 'PCACOQU'
    lchin(5) = chcara(7)
    lpain(6) = 'PCAGNPO'
    lchin(6) = chcara(6)
    lpain(7) = 'PCADISM'
    lchin(7) = chcara(3)
    lpain(8) = 'PCAORIE'
    lchin(8) = chcara(1)
    lpain(9) = 'PCACABL'
    lchin(9) = chcara(10)
    lpain(10) = 'PCAARPO'
    lchin(10) = chcara(9)
    lpain(11) = 'PCAGNBA'
    lchin(11) = chcara(11)
    lchin(12) = templu
    lpain(12) = 'PTEMPER'
    lpain(13) = 'PCAMASS'
    lchin(13) = chcara(12)
    lpaout(1) = 'PVECTUR'
!
    do 20 icha = 1, nchar
        nomcha = zk24(jchar+icha-1) (1:8)
        ligrch = nomcha//'.CHME.LIGRE'
        numchm = zi(jinf+nchar+icha)
        if (numchm .eq. 5) then
            do 10 k = 1, nbchmx
                if (k .eq. 1) then
                    ligrel = ligrch
                else
                    ligrel = ligrmo
                endif
                if (nomlig(k) .eq. '.VEASS') then
                    suffix = '     '
                else
                    suffix = '.DESC'
                endif
                lchin(1) = ligrch(1:13)//nomlig(k)//suffix
                call jeexin(lchin(1), iret)
                if (iret .ne. 0) then
                    call jeveuo(nomcha//'.TYPE', 'L', jtyp)
                    if (zk8(jtyp) .eq. 'MECA_RE ') then
                        option = 'CHAR_MECA_'//nomopr(k)
                        lpain(1) = 'P'//nompar(k)
                    else if (zk8(jtyp).eq.'MECA_FO ') then
                        option = 'CHAR_MECA_'//nomopf(k)
                        lpain(1) = 'P'//nompaf(k)
                    endif
                    lchout(1) = '&&VEFPME.???????'
                    call gcnco2(newnom)
                    lchout(1) (10:16) = newnom(2:8)
                    call corich('E', lchout(1), icha, ibid)
!
!           POUR LES ELEMENTS X-FEM
                    call exixfe(modele, ier)
                    nchin = 13
                    if (ier .ne. 0) then
                        lpain(nchin + 1) = 'PPINTTO'
                        lchin(nchin + 1) = modele(1:8)//'.TOPOSE.PIN'
                        lpain(nchin + 2) = 'PCNSETO'
                        lchin(nchin + 2) = modele(1:8)//'.TOPOSE.CNS'
                        lpain(nchin + 3) = 'PHEAVTO'
                        lchin(nchin + 3) = modele(1:8)//'.TOPOSE.HEA'
                        lpain(nchin + 4) = 'PLONCHA'
                        lchin(nchin + 4) = modele(1:8)//'.TOPOSE.LON'
                        lpain(nchin + 5) = 'PLSN'
                        lchin(nchin + 5) = modele(1:8)//'.LNNO'
                        lpain(nchin + 6) = 'PLST'
                        lchin(nchin + 6) = modele(1:8)//'.LTNO'
                        lpain(nchin + 7) = 'PSTANO'
                        lchin(nchin + 7) = modele(1:8)//'.STNO'
                        lpain(nchin + 8) = 'PPMILTO'
                        lchin(nchin + 8) = modele(1:8)//'.TOPOSE.PMI'
                        lpain(nchin + 9) = 'PFISNO'
                        lchin(nchin + 9) = modele(1:8)//'.FISSNO'
                        nchin = nchin + 9
                        if (option .eq. 'CHAR_MECA_PRES_R' .or. option .eq.&
                            'CHAR_MECA_PRES_F') then
                            lpain(nchin + 1) = 'PPINTER'
!                  LCHIN(NCHIN + 1) = MODELE(1:8)//'.TOPOFAC.PI'
                            lchin(nchin + 1) = modele(1:8)// '.TOPOFAC.OE'
                            lpain(nchin + 2) = 'PAINTER'
                            lchin(nchin + 2) = modele(1:8)// '.TOPOFAC.AI'
                            lpain(nchin + 3) = 'PCFACE'
                            lchin(nchin + 3) = modele(1:8)// '.TOPOFAC.CF'
                            lpain(nchin + 4) = 'PLONGCO'
                            lchin(nchin + 4) = modele(1:8)// '.TOPOFAC.LO'
                            lpain(nchin + 5) = 'PBASECO'
                            lchin(nchin + 5) = modele(1:8)// '.TOPOFAC.BA'
                            nchin = nchin + 5
                        endif
                    endif
!
                    if (nomlig(k) .eq. '.VEASS') then
                        call jeveuo(lchin(1), 'L', jlchin)
                        call copisd('CHAMP_GD', 'V', zk8(jlchin), lchout( 1))
                    else
                        call calcul('S', option, ligrel, nchin, lchin,&
                                    lpain, 1, lchout, lpaout, 'V',&
                                    'OUI')
                    endif
                    call reajre(lvechp, lchout(1), 'V')
                endif
10          continue
        endif
!
20  end do
!
30  continue
!
    lvechz = lvechp//'.RELR'
    call jedema()
end subroutine
