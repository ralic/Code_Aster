subroutine vetnth(optioz, modelz, carelz, matcdz, instz,&
                  chtnz, compoz, tpchiz, tpchfz, chhyz,&
                  vecelz, veceiz)
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
! ----------------------------------------------------------------------
! CALCUL DES VECTEURS ELEMENTAIRES DUS AU CHAMP A L'INSTANT PRECEDENT
!
! IN  OPTIOZ  : OPTION CALCULEE: CHAR_THER_EVOL
!                                CHAR_THER_EVOLNI
! IN  MODELZ : NOM DU MODELE
! IN  CARELZ : CHAMP DE CARA_ELEM
! IN  MATCDZ : MATERIAU CODE
! IN  INSTZ  : CARTE CONTENANT LA VALEUR DU TEMPS
! IN  CHHYZ  : CHAMP D HYDRATATION A L'INSTANT PRECEDENT
! . POUR LE CALCUL DE LA TEMPERATURE :
! IN  CHTNZ  : CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
! OUT VECEL/VECEI : VECT_ELEM
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE:INFNIV.
!       JEVEUX:JEMARQ,JEDEMA,JEEXIN,WKVECT,JEVEUO,JEECRA.
!       CALCUL: CALCUL.
!       FICH COMM: GETRES.
!       MANIP SD: RSEXCH,MEGEOM,MECARA,EXISD.
!       DIVERS: GCNCO2,CORICH.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit   none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/exisd.h'
    include 'asterfort/exixfe.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    character(len=*) :: optioz, modelz, carelz, matcdz, instz, chtnz, vecelz
    character(len=*) :: veceiz, compoz, tpchiz, tpchfz, chhyz
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter (nompro='VETNTH')
    integer :: nchinx, nchoux
    parameter (nchinx=24,nchoux=2)
    integer :: iret, ibid, nchin, nchout, i, ifm, niv
    character(len=8) :: lpain(nchinx), lpaout(nchoux), newnom
    character(len=16) :: option
    character(len=19) :: chvarc, stano, pintto, cnseto, heavto, loncha, basloc
    character(len=19) :: lsn, lst
    character(len=24) :: modele, carele, matcod, inst, chtn, vecel, vecei
    character(len=24) :: compor, tmpchi, tmpchf, chhy, ligrmo, lchin(nchinx)
    character(len=24) :: chgeom, lchout(nchoux), chcara(18)
    logical :: exicar, lnlin
!
    chvarc = '&&NXACMV.CHVARC'
!
!
! DEB ------------------------------------------------------------------
!====
! 1.1 PREALABLES LIES AUX OPTIONS
!====
    call infniv(ifm, niv)
    do 10 i = 1, nchinx
        lpain(i) = '        '
        lchin(i) = '                        '
10  end do
    call jemarq()
    newnom = '.0000000'
    option = optioz
    modele = modelz
    carele = carelz
    matcod = matcdz
    inst = instz
    chtn = chtnz
    vecel = vecelz
    vecei = veceiz
    compor = compoz
    tmpchi = tpchiz
    tmpchf = tpchfz
    chhy = chhyz
    lnlin = .false.
    if (option .eq. 'CHAR_THER_EVOLNI') then
        lnlin = .true.
    endif
! AFFICHAGE
    if (niv .eq. 2) then
        write (ifm,*) '*******************************************'
        write (ifm,*) ' CALCUL DE SECOND MEMBRE THERMIQUE: VETNTH'
        write (ifm,*) ' CHAMP MATERIAU CODE  : ',matcod
    endif
!
! CADRE X-FEM
    call exixfe(modele, iret)
    if (iret .ne. 0) then
        stano = modele(1:8)//'.STNO'
        pintto = modele(1:8)//'.TOPOSE.PIN'
        cnseto = modele(1:8)//'.TOPOSE.CNS'
        heavto = modele(1:8)//'.TOPOSE.HEA'
        loncha = modele(1:8)//'.TOPOSE.LON'
        basloc = modele(1:8)//'.BASLOC'
        lsn = modele(1:8)//'.LNNO'
        lst = modele(1:8)//'.LTNO'
    else
        stano = '&&VETNTH.STNO.BID'
        pintto = '&&VETNTH.PINTTO.BID'
        cnseto = '&&VETNTH.CNSETO.BID'
        heavto = '&&VETNTH.HEAVTO.BID'
        loncha = '&&VETNTH.LONCHA.BID'
        basloc = '&&VETNTH.BASLOC.BID'
        lsn = '&&VETNTH.LNNO.BID'
        lst = '&&VETNTH.LTNO.BID'
    endif
!
!====
! 1.2 PREALABLES LIES AU VECT_ELEM VECEL
!====
!
! RECHERCHE DU CHAMP DE GEOMETRIE CHGEOM ASSOCIE AU MODELE
    call megeom(modele, chgeom)
! RECHERCHE DES NOMS DES CARAELEM CHCARA DANS LA CARTE CARELE
    call mecara(carele, exicar, chcara)
! TEST D'EXISTENCE DE L'OBJET JEVEUX VECEL
    call jeexin(vecel, iret)
    if (iret .eq. 0) then
! L'OBJET VECEL N'EXISTE PAS
! CREATION DE L'OBJET '.REFE_RESU' DE VECEL ASSOCIE AU MODELE, A
! MATERIAU MATCODE, AU CARAELEM CARELE ET A LA SUR_OPTION 'MASS_THER'
        vecel = '&&'//nompro//'           .RELR'
        call memare('V', vecel, modele(1:8), matcod, carele,&
                    'MASS_THER')
    else
! L'OBJET EXISTE
        call jedetr(vecel)
    endif
    ligrmo = modele(1:8)//'.MODELE'
!
!====
! 1.3 PREALABLES LIES A L OPTION 'CHAR_THER_EVOLNI'
!====
!
    if (lnlin) then
        call jeexin(vecei, iret)
        if (iret .eq. 0) then
            vecei = '&&VETNTI           .RELR'
            call memare('V', vecei(1:8), modele(1:8), matcod, carele,&
                        'MASS_THER')
        else
            call jedetr(vecei)
        endif
    endif
!
!====
! 2. PREPARATION DES CALCULS ELEMENTAIRES (TRONC COMMUN EN IN)
!====
!
! CHAMP LOCAL CONTENANT LA CARTE DES NOEUDS (X Y Z)
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
! ... LE CHAM_NO DE TEMPERATURE OU DE LA DERIVEE A L'INSTANT
!     PRECEDENT (TEMP)
    lpain(2) = 'PTEMPER'
    lchin(2) = chtn
! ... LA CARTE MATERIAU (I1) STD
    lpain(3) = 'PMATERC'
    lchin(3) = matcod
! ... LA CARTE DES INSTANTS (INST DELTAT THETA KHI  R RHO)
    lpain(4) = 'PTEMPSR'
    lchin(4) = inst
! ... CARTE LIEE A DES CARACTERISTIQUES DE COQUE
    lpain(5) = 'PCACOQU'
    lchin(5) = chcara(7)
    lpain(6) = 'PVARCPR'
    lchin(6) = chvarc
    nchin = 6
!
!====
! 3. PREPARATION DES CALCULS ELEMENTAIRES (CAS PARTICULIERS EN IN)
!====
!
    if (.not.lnlin) then
        nchin = nchin + 1
! ... CARTE LIEE AUX EF MASSIFS
        lpain(nchin) = 'PCAMASS'
        lchin(nchin) = chcara(12)
! ... CHAMPS IN POUR X-FEM (SEULEMENT EN THERMIQUE LINEAIRE)
        nchin = nchin + 1
        lpain(nchin) = 'PSTANO'
        lchin(nchin) = stano
        nchin = nchin + 1
        lpain(nchin) = 'PPINTTO'
        lchin(nchin) = pintto
        nchin = nchin + 1
        lpain(nchin) = 'PCNSETO'
        lchin(nchin) = cnseto
        nchin = nchin + 1
        lpain(nchin) = 'PHEAVTO'
        lchin(nchin) = heavto
        nchin = nchin + 1
        lpain(nchin) = 'PLONCHA'
        lchin(nchin) = loncha
        nchin = nchin + 1
        lpain(nchin) = 'PBASLOR'
        lchin(nchin) = basloc
        nchin = nchin + 1
        lpain(nchin) = 'PLSN'
        lchin(nchin) = lsn
        nchin = nchin + 1
        lpain(nchin) = 'PLST'
        lchin(nchin) = lst
    else
        nchin = nchin + 1
        lpain(nchin) = 'PCOMPOR'
        lchin(nchin) = compor
        nchin = nchin + 1
        lpain(nchin) = 'PTMPCHI'
        lchin(nchin) = tmpchi
        nchin = nchin + 1
        lpain(nchin) = 'PTMPCHF'
        lchin(nchin) = tmpchf
    endif
!
! ... LE CHAMP RESULTAT
    lpaout(1) = 'PVECTTR'
    lchout(1) = '&&'//nompro//'.???????'
!
!====
! 4. PREPA DES CALCULS ELEM EN OUT
!    PRETRAITEMENTS POUR TENIR COMPTE DE FONC_MULT
!====
    call gcnco2(newnom)
    lchout(1) (10:16) = newnom(2:8)
    call corich('E', lchout(1), -1, ibid)
!
    if (lnlin) then
        lpaout(2) = 'PVECTTI'
        lchout(2) = '&&'//nompro//'.???????'
        call gcnco2(newnom)
        lchout(2) (10:16) = newnom(2:8)
        call corich('E', lchout(2), -1, ibid)
        nchout = 2
        nchin = nchin + 1
        lpain(nchin) = 'PHYDRPM'
        lchin(nchin) = chhy
    else
        nchout = 1
    endif
!
!====
! 5. LANCEMENT DES CALCULS ELEMENTAIRES OPTION
!====
! TEST D'EXISTENCE DU CHAMP DE TEMP A L'INSTANT
! PRECEDENT. SI IL EXISTE ON LANCE LE CALCUL SIN PB.
    call exisd('CHAMP_GD', chtn(1:19), iret)
    call assert(iret.gt.0)
    if (niv .eq. 2) then
        write (ifm,*) '-->  OPTION         :',option
        do 20 i = 1, nchin
            write (ifm,*) '     LPAIN/LCHIN    :',lpain(i),' ',lchin(i)
20      continue
    endif
!
!C    PRINT * ,'DANS ',NOMPRO,' OPTION = ',OPTION,' ET STYPSE = ',STYPSE
    call calcul('S', option, ligrmo, nchin, lchin,&
                lpain, nchout, lchout, lpaout, 'V',&
                'OUI')
!
! SI ON GENERE DE MANIERE EFFECTIVE UN RESULTAT LCHOUT(1) (VOIR (2))
! INCREMENTATION DE LONUTI ET STOCKAGE DU RESULTAT
!
    call reajre(vecel, lchout(1), 'V')
    if (lnlin) then
        call reajre(vecei, lchout(2), 'V')
    endif
!
    vecelz = vecel
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
