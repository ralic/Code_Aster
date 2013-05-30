subroutine verstp(modele, charge, infcha, carele, mate,&
                  inst, compor, chti, chtip, chthy,&
                  chthyp, chtsci, chtscf, veres)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/reajre.h'
    character(len=24) :: modele, charge, infcha, carele, inst, chti, chtip
    character(len=24) :: veres, mate, chthy, chthyp, chtsci, chtscf, compor
! ----------------------------------------------------------------------
! CALCUL DES VECTEURS ELEMENTAIRES RESIDU
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGES
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : CHAMP DE MATERIAU
! IN  COMPOR  : CHAMP DE COMPORTEMENT (POUR TESTER L HYDRATATION)
! IN  INST    : CARTE CONTENANT LA VALEUR DU TEMPS ET AUTRES PARAMETRES
! IN  CHTI    : TEMPERATURE AU TEMPS PRECEDENT
! IN  CHTIP   : I EME ITERE DU CHAMP DE TEMPERATURE
! IN  CHTHY   : HYDRATATION AU TEMPS PRECEDENT
! IN  CHTSCI  : CHAMP DE TEMPERAT. A T    (POUR LE CALCUL DE D-SECHAGE)
! IN  CHTSCF  : CHAMP DE TEMPERAT. A T+DT (POUR LE CALCUL DE D-SECHAGE)
! OUT CHTHYP  : I EME ITERE DU CHAMP D HYDRATATION
! OUT VERES   : VECTEURS ELEMENTAIRES (RESIDU)
!
!
!
    character(len=1) :: c1
    character(len=8) :: nomcha, lpain(10), lpaout(2), k8bid, newnom
    character(len=16) :: option
    character(len=24) :: ligrmo, lchin(10), lchout(2), ligrch
    character(len=24) :: chgeom, chcara(18)
    integer :: iret, jinf
    integer :: icha, jchar, nchar
    logical :: exicar
!
!-----------------------------------------------------------------------
    integer :: ibid
!-----------------------------------------------------------------------
    call jemarq()
    newnom = '.0000000'
    ligrmo = modele(1:8)//'.MODELE'
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar, k8bid)
        call jeveuo(charge, 'L', jchar)
        call jeveuo(infcha, 'L', jinf)
    else
        nchar = 0
    endif
!
    call megeom(modele, chgeom)
    call mecara(carele, exicar, chcara)
!
    lpaout(1) = 'PRESIDU'
    lchout(1) = '&&VERSTP.???????'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(3) = 'PTEMPSR'
    lchin(3) = inst
    lpain(4) = 'PTEMPEI'
    lchin(4) = chtip
!
! --- TERME VOLUMIQUE PROVENANT DU COMPORTEMENT
!
    option = 'RESI_RIGI_MASS'
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(5) = 'PHYDRPM'
    lchin(5) = chthy
    lpain(6) = 'PCOMPOR'
    lchin(6) = compor
    lpain(7) = 'PTEMPER'
    lchin(7) = chti
    lpain(8) = 'PTMPCHI'
    lchin(8) = chtsci
    lpain(9) = 'PTMPCHF'
    lchin(9) = chtscf
    lpain(10) = 'PVARCPR'
    lchin(10) = '&&NXACMV.CHVARC'
!
    lpaout(2) = 'PHYDRPP'
    lchout(2) = chthyp
    call gcnco2(newnom)
    lchout(1) (10:16) = newnom(2:8)
    call corich('E', lchout(1), -1, ibid)
    call calcul('S', option, ligrmo, 10, lchin,&
                lpain, 2, lchout, lpaout, 'V',&
                'OUI')
    call reajre(veres, lchout(1), 'V')
!
! --- TERME SURFACIQUE PROVENANT DES CONDITIONS AUX LIMITES
! --- ET VOLUMIQUE PROVENANT D'UNE SOURCE NON LINEAIRE
!
    if (nchar .gt. 0) then
        do 10 icha = 1, nchar
!
            lpain(2) = 'PFLUXNL'
            lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.FLUNL.DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                option = 'RESI_THER_FLUXNL'
                call gcnco2(newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrmo, 4, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
!
            lpain(2) = 'PSOURNL'
            lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.SOUNL.DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                option = 'RESI_THER_SOURNL'
                call gcnco2(newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrmo, 4, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
!
            lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.T_EXT.DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                c1 = 'R'
                if (zi(jinf+nchar+icha) .gt. 1) then
                    c1 = 'F'
                endif
                option = 'RESI_THER_COEF_'//c1
                lpain(2) = 'PCOEFH'//c1
                lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.COEFH'
                call gcnco2(newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrmo, 4, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
!
            lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.RAYO .DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                c1 = 'R'
                if (zi(jinf+nchar+icha) .gt. 1) then
                    c1 = 'F'
                endif
                option = 'RESI_THER_RAYO_'//c1
                lpain(2) = 'PRAYON'//c1
                lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.RAYO'
                call gcnco2(newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrmo, 4, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
!
            nomcha = zk24(jchar+icha-1) (1:8)
            ligrch = nomcha//'.CHTH.LIGRE'
            lchin(2) = nomcha//'.CHTH.HECHP.DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                c1 = 'R'
                if (zi(jinf+nchar+icha) .gt. 1) then
                    c1 = 'F'
                endif
                option = 'RESI_THER_PARO_'//c1
                lpain(2) = 'PHECHP'//c1
                lchin(2) = nomcha//'.CHTH.HECHP'
                call gcnco2(newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrch, 4, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
!
10      continue
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
