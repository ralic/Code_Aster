subroutine racotu(iprno, lonlis, klisno, noepou, noma,&
                  ligrel, mod, cara, numddl, typlag,&
                  lisrel, coorig)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/afretu.h'
    include 'asterfort/assvec.h'
    include 'asterfort/calcul.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/mecact.h'
    include 'asterfort/memare.h'
    include 'asterfort/normev.h'
    include 'asterfort/raorfi.h'
    include 'asterfort/reajre.h'
    include 'asterfort/vdiff.h'
    integer :: lonlis, iprno(*)
    character(len=2) :: typlag
    character(len=8) :: klisno(lonlis), noepou, noma, cara, mod
    character(len=14) :: numddl
    character(len=19) :: ligrel, lisrel
    real(kind=8) :: coorig(3)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! -------------------------------------------------------
!     RACCORD COQUE_TUYAU PAR DES RELATIONS LINEAIRES
!
    integer :: nbcmp, nbmode, numno1, jcoor
    parameter (nbmode=3,nbcmp=6* (nbmode-1))
    character(len=1) :: k1bid
    character(len=8) :: nocmp(nbcmp), lpain(6), lpaout(3), nomddl(4)
    character(len=24) :: lchin(6), lchout(3), valech
    real(kind=8) :: rbid, coef(4), eg1(3), eg2(3), eg3(3)
    real(kind=8) :: rayon, coori1(3), gp1(3)
    complex(kind=8) :: cbid
    integer :: imod, info, ifm
    integer :: nbcoef, idec
!
    call jemarq()
    call infniv(ifm, info)
!
!     CALCUL DU RAYON DU MAILLAGE COQUE A L'AIDE DU PREMIER N
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jenonu(jexnom(noma//'.NOMNOE', klisno(1)), numno1)
    coori1(1) = zr(jcoor-1+3* (numno1-1)+1)
    coori1(2) = zr(jcoor-1+3* (numno1-1)+2)
    coori1(3) = zr(jcoor-1+3* (numno1-1)+3)
    call vdiff(3, coorig, coori1, gp1)
    call normev(gp1, rayon)
!
!     CREATION D'UNE CARTE CONTENANT LE POINT P ORIGINE DE PHI
!
    call raorfi(noma, ligrel, noepou, cara, coorig,&
                eg1, eg2, eg3, '&&RACOTU', rayon)
!
! --- DETERMINATION DE 3 LISTES  DE VECTEURS PAR ELEMENT PRENANT
! --- LEURS VALEURS AUX NOEUDS DES ELEMENTS. CES VALEURS SONT :
!     VECT_COEF_UM
! --- 1/PI* SOMME/S_ELEMENT(COS(M.PHI).P(1,J).NI)DS
! --- 1/PI* SOMME/S_ELEMENT(SIN(M.PHI).P(1,J).NI)DS
!     VECT_COEF_VM
! --- 1/PI* SOMME/S_ELEMENT(COS(M.PHI).P(2,J).NI)DS
! --- 1/PI* SOMME/S_ELEMENT(SIN(M.PHI).P(2,J).NI)DS
!     VECT_COEF_WM
! --- 1/PI* SOMME/S_ELEMENT(COS(M.PHI).P(3,J).NI)DS
! --- 1/PI* SOMME/S_ELEMENT(SIN(M.PHI).P(3,J).NI)DS
!
! --- OU P EST LA MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
! --- (E1,E2,E3) DEFINI SUR LE BORD ORIENTE DE LA COQUE
!     ------------------------------
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpain(2) = 'PORIGIN'
    lchin(2) = '&&RAPOCO.CAORIGE'
    lpain(3) = 'PCACOQU'
    lchin(3) = cara//'.CARCOQUE'
    lpain(4) = 'PCAORIE'
    lchin(4) = '&&RACOTU.CAXE_TUY'
    lpain(5) = 'PORIGFI'
    lchin(5) = '&&RACOTU.CAORIFI'
    lpain(6) = 'PNUMMOD'
    lchin(6) = '&&RAPOTU.NUME_MODE'
    lpaout(1) = 'PVECTU1'
    lchout(1) = '&&RAPOTU.COEF_UM'
    lpaout(2) = 'PVECTU2'
    lchout(2) = '&&RAPOTU.COEF_VM'
    lpaout(3) = 'PVECTU3'
    lchout(3) = '&&RAPOTU.COEF_WM'
!
! --- CREATION DES .RERR DES VECTEURS EN SORTIE DE CALCUL
    call memare('V', '&&RAPOTU', mod, ' ', ' ',&
                'CHAR_MECA')
!
! --- CREATION DU .RELR
    call jedetr('&&RAPOTU           .RELR')
    call reajre('&&RAPOTU', ' ', 'V')
!
!     RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL WO
!
    imod = 0
    call mecact('V', lchin(6), 'LIGREL', ligrel, 'NUMMOD',&
                1, 'NUM', imod, rbid, cbid,&
                k1bid)
    call calcul('S', 'CARA_SECT_POUT5', ligrel, 6, lchin,&
                lpain, 3, lchout, lpaout, 'V',&
                'OUI')
    call jedetr('&&RAPOTU           .RELR')
    call reajre('&&RAPOTU', lchout(3), 'V')
    call assvec('V', 'CH_DEPL_3', 1, '&&RAPOTU           .RELR', 1.d0,&
                numddl, ' ', 'ZERO', 1)
    valech = 'CH_DEPL_3          .VALE'
    nbcoef = 1
    idec = 0
    nomddl(1) = 'WO'
    coef(1) = -2.d0
    call afretu(iprno, lonlis, klisno, noepou, noma,&
                valech, nbcoef, idec, coef, nomddl,&
                typlag, lisrel)
!        FIN IMOD=0
!
!   RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE
!   DDL UIM, UOM, VIM, VOM, WIM, WOM, M VARIANT DE 2 A NBMODE
!
    nocmp(1) = 'UI2'
    nocmp(2) = 'VI2'
    nocmp(3) = 'WI2'
    nocmp(4) = 'UO2'
    nocmp(5) = 'VO2'
    nocmp(6) = 'WO2'
    nocmp(7) = 'UI3'
    nocmp(8) = 'VI3'
    nocmp(9) = 'WI3'
    nocmp(10) = 'UO3'
    nocmp(11) = 'VO3'
    nocmp(12) = 'WO3'
!
    do 10 imod = 1, nbmode
        if (info .eq. 2) then
            write (ifm,*) 'RELATIONS SUR LE MODE ',imod
        endif
        call mecact('V', lchin(6), 'LIGREL', ligrel, 'NUMMOD',&
                    1, 'NUM', imod, rbid, cbid,&
                    k1bid)
        call calcul('S', 'CARA_SECT_POUT5', ligrel, 6, lchin,&
                    lpain, 3, lchout, lpaout, 'V',&
                    'OUI')
        call jedetr('&&RAPOTU           .RELR')
        call reajre('&&RAPOTU', lchout(1), 'V')
        call assvec('V', 'CH_DEPL_1', 1, '&&RAPOTU           .RELR', 1.d0,&
                    numddl, ' ', 'ZERO', 1)
        valech = 'CH_DEPL_1          .VALE'
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL UIM
!
        idec = 0
        if (imod .ne. 1) then
            nbcoef = 1
            nomddl(1) = nocmp(6* (imod-2)+1)
            coef(1) = -1.d0
            call afretu(iprno, lonlis, klisno, noepou, noma,&
                        valech, nbcoef, idec, coef, nomddl,&
                        typlag, lisrel)
!
        endif
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL UOM
!
        idec = 3
        if (imod .ne. 1) then
            nbcoef = 1
            nomddl(1) = nocmp(6* (imod-2)+4)
            coef(1) = -1.d0
            call afretu(iprno, lonlis, klisno, noepou, noma,&
                        valech, nbcoef, idec, coef, nomddl,&
                        typlag, lisrel)
        endif
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL VOM
        call jedetr('&&RAPOTU           .RELR')
        call reajre('&&RAPOTU', lchout(2), 'V')
        call assvec('V', 'CH_DEPL_2', 1, '&&RAPOTU           .RELR', 1.d0,&
                    numddl, ' ', 'ZERO', 1)
        valech = 'CH_DEPL_2          .VALE'
        idec = 0
        if (imod .ne. 1) then
            nbcoef = 1
            nomddl(1) = nocmp(6* (imod-2)+5)
!CCC            COEF(1)  =  -1.D0
            coef(1) = -1.d0
            call afretu(iprno, lonlis, klisno, noepou, noma,&
                        valech, nbcoef, idec, coef, nomddl,&
                        typlag, lisrel)
        endif
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL VIM
!        IDEC=3 SIGNIFIE QUE ON UTILISE LES TERMES EN SIN(M*PHI)
!
        idec = 3
        if (imod .ne. 1) then
            nbcoef = 1
            nomddl(1) = nocmp(6* (imod-2)+2)
!CCC            COEF(1)  =  1.D0
            coef(1) = -1.d0
            call afretu(iprno, lonlis, klisno, noepou, noma,&
                        valech, nbcoef, idec, coef, nomddl,&
                        typlag, lisrel)
        endif
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL WIM
!        OU SI IMOD=1 LE DDL DZ DANS REPERE LOCAL DU TUYAU ET WI1
!        IDEC=0 SIGNIFIE QUE ON UTILISE LES TERMES EN COS(M*PHI)
!
        call jedetr('&&RAPOTU           .RELR')
        call reajre('&&RAPOTU', lchout(3), 'V')
        call assvec('V', 'CH_DEPL_3', 1, '&&RAPOTU           .RELR', 1.d0,&
                    numddl, ' ', 'ZERO', 1)
        valech = 'CH_DEPL_3          .VALE'
        idec = 0
!
        if (imod .eq. 1) then
            nbcoef = 4
            nomddl(1) = 'DX'
            nomddl(2) = 'DY'
            nomddl(3) = 'DZ'
            nomddl(4) = 'WI1'
            coef(1) = eg3(1)
            coef(2) = eg3(2)
            coef(3) = eg3(3)
            coef(4) = -1.d0
        else
!         IF (IMOD.NE.1) THEN
            nbcoef = 1
            nomddl(1) = nocmp(6* (imod-2)+3)
            coef(1) = -1.d0
        endif
        call afretu(iprno, lonlis, klisno, noepou, noma,&
                    valech, nbcoef, idec, coef, nomddl,&
                    typlag, lisrel)
!         ENDIF
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD POUTRE DDL WOM
!        OU SI IMOD=1 LE DDL DY DANS REPERE LOCAL DU TUYAU ET WO1
!        IDEC=3 SIGNIFIE QUE ON UTILISE LES TERMES EN SIN(M*PHI)
!
        idec = 3
        if (imod .eq. 1) then
            nbcoef = 4
            nomddl(1) = 'DX'
            nomddl(2) = 'DY'
            nomddl(3) = 'DZ'
            nomddl(4) = 'WO1'
            coef(1) = eg2(1)
            coef(2) = eg2(2)
            coef(3) = eg2(3)
            coef(4) = -1.d0
        else
!         IF (IMOD.NE.1) THEN
            nbcoef = 1
            nomddl(1) = nocmp(6* (imod-2)+6)
            coef(1) = -1.d0
        endif
        call afretu(iprno, lonlis, klisno, noepou, noma,&
                    valech, nbcoef, idec, coef, nomddl,&
                    typlag, lisrel)
!         ENDIF
!
!     FIN DE LA BOUCLE SUR LES MODES
!
10  end do
!
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr('&&RAPOTU           .RELR')
    call jedetr('&&RAPOTU           .RERR')
    call detrsd('CARTE', '&&RAPOTU.NUME_MODE')
    call detrsd('RESUELEM', '&&RAPOTU.COEF_UM')
    call detrsd('RESUELEM', '&&RAPOTU.COEF_VM')
    call detrsd('RESUELEM', '&&RAPOTU.COEF_WM')
    call detrsd('CHAMP_GD', '&&RACOTU.CAXE_TUY')
    call detrsd('CHAMP_GD', '&&RACOTU.CAORIFI')
    call detrsd('CHAMP_GD', 'CH_DEPL_1')
    call detrsd('CHAMP_GD', 'CH_DEPL_2')
    call detrsd('CHAMP_GD', 'CH_DEPL_3')
!
    call jedema()
end subroutine
