subroutine vechnl(modele, charge, infcha, carele, inst,&
                  chtn, lvechn)
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
! ----------------------------------------------------------------------
! CALCUL DES VECTEURS ELEMENTAIRES TERMES D EVOLUTION
! POUR LES CHARGES NON LINEAIRES ( FLUXNL, RAYONNEMENT, SOUR_NL )
! IN  MODELE : NOM DU MODELE
! IN  CHARGE : LISTE DES CHARGES
! IN  INFCHA : INFORMATIONS SUR LES CHARGES
! IN  CARELE : CHAMP DE CARA_ELEM
! IN  INST   : CARTE CONTENANT LA VALEUR DU TEMPS ET AUTRES PARAMETRES
! IN  CHTN   : ITERE A L INSTANT PRECEDENT DU CHAMP DE TEMPERATURE
! OUT LVECHN : VECT_ELEM
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/reajre.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: modele, charge, infcha, carele, inst, chtn, lvechn
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
    character(len=1) :: c1
    character(len=8) :: lpain(7), lpaout(1), k8bid, newnom, nomcha, nomch2
    character(len=16) :: option
    character(len=24) :: ligrmo, lchin(7), lchout(1), chgeom, chcara(18)
    integer :: iret, ifm, niv, exicha, nchin, i, nchar, jchar, jinf, icha, ibid
    integer :: jlvn
    logical :: exicar
!
!====
! 1.1 PREALABLES LIES AUX OPTIONS
!====
    call jemarq()
    call infniv(ifm, niv)
!
    do 10 i = 1, 6
        lpain(i) = '        '
        lchin(i) = '                        '
10  end do
!====
! 1.2 PREALABLES LIES AUX CHARGES
!====
!
    newnom = '.0000000'
    ligrmo = modele(1:8)//'.MODELE'
! TEST D'EXISTENCE DE L'OBJET JEVEUX CHARGE
    call jeexin(charge, iret)
    if (iret .ne. 0) then
! LECTURE DU NBRE DE CHARGE NCHAR DANS L'OBJET JEVEUX CHARGE
        call jelira(charge, 'LONMAX', nchar, k8bid)
! LECTURE DES ADRESSES JEVEUX DES CHARGES ET DES INFOS AFFERENTES
        call jeveuo(charge, 'L', jchar)
        call jeveuo(infcha, 'L', jinf)
    else
        nchar = 0
    endif
!
!====
! 2.1 PREPARATION DES CALCULS ELEMENTAIRES
!====
!
    call megeom(modele, chgeom)
    call mecara(carele, exicar, chcara)
    call jeexin(lvechn, iret)
    if (iret .eq. 0) then
        call wkvect(lvechn, 'V V K24', 2*nchar, jlvn)
        call jeecra(lvechn, 'LONUTI', 0, k8bid)
    endif
!
! PAS DE CHARGE ==> EXIT
    if (nchar .eq. 0) goto 70
!
! CHAMP LOCAL RESULTAT
    lpaout(1) = 'PVECTTR'
    lchout(1) = '&&VECHNL.???????'
! ... LA CARTE DES NOEUDS (X Y Z)
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
! ... LA CARTE DES INSTANTS (INST DELTAT THETA KHI  R RHO)
    lpain(3) = 'PTEMPSR'
    lchin(3) = inst
! ... LE CHAM_NO T- OU (DT/DS)-
    lpain(4) = 'PTEMPER'
    lchin(4) = chtn
!
!====
! 2.2 IMPRESSIONS NIVEAU 2 POUR DIAGNOSTICS...
!====
!
    if (niv .eq. 2) then
        write (ifm,*) '*******************************************'
        write (ifm,*) ' CALCUL DE SECOND MEMBRE THERMIQUE: VECHNL'
        write (ifm,*)
        write (ifm,*) ' LIGREL/MODELE    :',ligrmo
        write (ifm,*) ' OBJ JEVEUX CHARGE:',charge
        write (ifm,*) '            INFOCH:',infcha
        write (ifm,*) ' NBRE DE CHARGES  :',nchar
        write (ifm,*) ' BOUCLE SUR LES CHARGES DE TYPE NEUMANN NON-LIN'
    endif
!
!====
! 3. BOUCLE SUR LES AFFE_CHAR_THER ==================================
!====
!
    if (nchar .gt. 0) then
        do 60 icha = 1, nchar
!
! NOM DE LA CHARGE
            nomcha = zk24(jchar+icha-1) (1:8)
            if (niv .eq. 2) then
                write (ifm,*) ' '
                write (ifm,*) '   CHARGE         :',nomcha
            endif
!
! ON CONSTRUIT LES SECONDS MEMBRES ELEM
            exicha = 0
            nomch2 = nomcha
!
            if (exicha .eq. 0) then
!
!====
! 3.2 TEST D'EXISTENCE DE LA CL DE FLUX NON-LINEAIRE
!====
                iret = 0
                lpain(2) = 'PFLUXNL'
                lchin(2) = nomch2(1:8)//'.CHTH.FLUNL.DESC'
                call jeexin(lchin(2), iret)
!
                if (iret .ne. 0) then
                    option = 'CHAR_THER_FLUNL'
! CALCUL DU SECOND MEMBRE DU PB STD
                    nchin = 4
!====
! 3.2.1 PRETRAITEMENTS POUR TENIR COMPTE DE FONC_MULT
!====
                    call gcnco2(newnom)
                    lchout(1) (10:16) = newnom(2:8)
                    call corich('E', lchout(1), -1, ibid)
!
!====
! 3.2.2 LANCEMENT DES CALCULS ELEMENTAIRES
!====
                    if (niv .eq. 2) then
                        write (ifm,*) '     OPTION         :',option
                        do 20 i = 1, nchin
                            write (ifm,*) '     LPAIN/LCHIN    :',lpain(i),' ',&
     &              lchin(i)
20                      continue
                    endif
                    call calcul('S', option, ligrmo, nchin, lchin,&
                                lpain, 1, lchout, lpaout, 'V',&
                                'OUI')
!
! INCREMENTATION DE LONUTI ET STOCKAGE DU RESULTAT
                    call reajre(lvechn, lchout(1), 'V')
! FIN DU IF IRET POUR FLUX_NL
                endif
!
!====
! 3.3 TEST D'EXISTENCE DE LA CL DE RAYONNEMENT
!====
                iret = 0
                lpain(2) = 'PRAYONF'
                lchin(2) = nomch2(1:8)//'.CHTH.RAYO .DESC'
                call jeexin(lchin(2), iret)
!
                if (iret .ne. 0) then
! CALCUL DU SECOND MEMBRE
                    nchin = 4
                    c1 = 'R'
                    if (zi(jinf+nchar+icha) .gt. 1) c1 = 'F'
                    option = 'CHAR_THER_RAYO_'//c1
                    lpain(2) = 'PRAYON'//c1
                    lchin(2) = nomch2(1:8)//'.CHTH.RAYO'
!
!====
! 3.3.1 PRETRAITEMENTS POUR TENIR COMPTE DE FONC_MULT
!====
                    call gcnco2(newnom)
                    lchout(1) (10:16) = newnom(2:8)
                    call corich('E', lchout(1), -1, ibid)
!
!====
! 3.3.2 LANCEMENT DES CALCULS ELEMENTAIRES
!====
                    if (niv .eq. 2) then
                        write (ifm,*) '     OPTION         :',option
                        do 30 i = 1, nchin
                            write (ifm,*) '     LPAIN/LCHIN    :',lpain(i),' ',&
     &              lchin(i)
30                      continue
                    endif
                    call calcul('S', option, ligrmo, nchin, lchin,&
                                lpain, 1, lchout, lpaout, 'V',&
                                'OUI')
!
! INCREMENTATION DE LONUTI ET STOCKAGE DU RESULTAT
                    call reajre(lvechn, lchout(1), 'V')
! FIN DU IF IRET POUR RAYONNEMENT
                endif
!
!
!====
! 3.4 TEST D'EXISTENCE DE LA CHARGE DE SOURCE NON LINEAIRE
!====
                iret = 0
                lpain(2) = 'PSOURNL'
                lchin(2) = nomch2(1:8)//'.CHTH.SOUNL.DESC'
                call jeexin(lchin(2), iret)
!
                if (iret .ne. 0) then
                    option = 'CHAR_THER_SOURNL'
! CALCUL DU SECOND MEMBRE DU PB STD
                    nchin = 4
!====
! 3.4.1 PRETRAITEMENTS POUR TENIR COMPTE DE FONC_MULT
!====
                    call gcnco2(newnom)
                    lchout(1) (10:16) = newnom(2:8)
                    call corich('E', lchout(1), -1, ibid)
!
!====
! 3.4.2 LANCEMENT DES CALCULS ELEMENTAIRES
!====
                    if (niv .eq. 2) then
                        write (ifm,*) '     OPTION         :',option
                        do 25 i = 1, nchin
                            write (ifm,*) '     LPAIN/LCHIN    :',lpain(i),' ',&
     &              lchin(i)
25                      continue
                    endif
                    call calcul('S', option, ligrmo, nchin, lchin,&
                                lpain, 1, lchout, lpaout, 'V',&
                                'OUI')
!
! INCREMENTATION DE LONUTI ET STOCKAGE DU RESULTAT
                    call reajre(lvechn, lchout(1), 'V')
! FIN DU IF IRET POUR SOUR_NL
                endif
!
! FIN DU IF EXICHA
            endif
!
! 5. FIN DE BOUCLE SUR LES CHARGES
!====
60      continue
! FIN DU IF NCHAR
    endif
!
! SORTIE DE SECOURS EN CAS D'ABSENCE DE CHARGE
70  continue
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
