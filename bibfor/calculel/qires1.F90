subroutine qires1(modele, ligrel, chtime, sigmap, sigmad,&
                  lcharp, lchard, ncharp, nchard, chs,&
                  mate, chvois, tabido, chelem)
! ----------------------------------------------------------------------
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
! TOLE CRP_20
!
!     BUT:
!         CALCUL DE L'ESTIMATEUR D'ERREUR EN QUANTITE D'INTERET
!         AVEC LES RESIDUS EXPLICITES.
!
!         OPTION : 'QIRE_ELEM'
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
!     MODELE : NOM DU MODELE
!     LIGREL : NOM DU LIGREL
!     CHTIME : NOM DU CHAMP DES INSTANTS
!     SIGMAP : CHAMP DE CONTRAINTES DU PB. PRIMAL (CHAM_ELEM_SIEF_R)
!     SIGMAD : CHAMP DE CONTRAINTES DU PB. DUAL (CHAM_ELEM_SIEF_R)
!     LCHARP : LISTE DES CHARGEMENTS DU PROBLEME PRIMAL
!     LCHARD : LISTE DES CHARGEMENTS DU PROBLEME DUAL
!     NCHARP : NOMBRE DE CHARGEMENTS DU PROBLEME PRIMAL
!     NCHARD : NOMBRE DE CHARGEMENTS DU PROBLEME DUAL
!     CHS    : CARTE CONSTANTE DU COEFFICIENT DE PONDERATION S
!     MATE   : NOM DU CHAMP MATERIAU
!     CHVOIS : NOM DU CHAMP DES VOISINS
!     TABIDO : TABLEAU D'ENTIERS CONTENANT DES ADRESSES
!          1 : IATYMA : ADRESSE DU VECTEUR TYPE MAILLE (NUMERO <-> NOM)
!          2 : IAGD   : ADRESSE DU VECTEUR GRANDEUR (NUMERO <-> NOM)
!          3 : IACMP  : ADRESSE DU VECTEUR NOMBRE DE COMPOSANTES
!                 (NUMERO DE GRANDEUR <-> NOMBRE DE COMPOSANTES)
!          4 : ICONX1 : ADRESSE DE LA COLLECTION CONNECTIVITE
!          5 : ICONX2 : ADRESSE DU POINTEUR DE LONGUEUR DE LA
!                       CONNECTIVITE
!
!      SORTIE :
!-------------
!      CHELEM : NOM DU CHAM_ELEM_ERREUR PRODUIT
!               SI CHELEM EXISTE DEJA, ON LE DETRUIT.
!
! REMARQUE : RESLOC ET QIRES1 DOIVENT RESTER TRES SIMILAIRES
! ......................................................................
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/etenca.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/mecact.h'
    include 'asterfort/megeom.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: ncharp, nchard
    integer :: tabido(5)
    character(len=8) :: modele, lcharp(1), lchard(1)
    character(len=24) :: sigmap, sigmad
    character(len=24) :: chtime, chs, chvois, chelem
    character(len=*) :: ligrel, mate
!
! DECLARATION VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'QIRES1' )
!
    integer :: nbcmp
    parameter ( nbcmp = 12 )
!
    integer :: nbchix
    parameter ( nbchix = 17 )
!
    integer :: i, iret, iret1, iret2, iret3, iret4, iret5, iret6, iret7
    integer :: iret8, iret9, iret10, iret11, iret12, iret13, iret14
    integer :: ibid, ier, iarepe
    integer :: iatyma, iagd, iacmp, iconx1, iconx2
    integer :: iadep1, iadep2, iavap1, iavap2
    integer :: iaded1, iaded2, iavad1, iavad2
    integer :: jceldp, jcelvp, jceldd, jcelvd
    integer :: iptmp1, iptmp2, numgp1, numgp2
    integer :: iptmd1, iptmd2, numgd1, numgd2
    integer :: icmpp(nbcmp ), icmpd(nbcmp )
    integer :: nbrin
!
    real(kind=8) :: r8bid
!
    character(len=1) :: base
    character(len=8) :: lpain(nbchix), lpaout(1), k8bid
    character(len=8) :: licmpp(nbcmp ), licmpd(nbcmp )
    character(len=8) :: typcp3, typcd3
    character(len=16) :: option
    character(len=19) :: cartp1, cartp2, nomgp1, nomgp2
    character(len=19) :: cartd1, cartd2, nomgd1, nomgd2
    character(len=24) :: lchin(nbchix), lchout(1), chgeom
    character(len=24) :: chfop1, chfop2, chfop3
    character(len=24) :: chfod1, chfod2, chfod3
!
    complex(kind=8) :: c16bid
!
!
! ----------------------------------------------------------------------
    base = 'V'
!
    call megeom(modele, chgeom)
!
! ------- DEBUT TEST SUR LE TYPE DE CHARGE DES BORDS POUR LE PB. PRIMAL
!
!   ATTENTION : POUR UN MEME CHARGEMENT (FORCE_FACE OU PRES_REP), SEULE
!   LA DERNIERE CHARGE EST CONSIDEREE (REGLE DE SURCHARGE ACTUELLEMENT)
! --- ON ALARME POUR LES CHARGES NON TRAITEES
!
    cartp1 = ' '
    cartp2 = ' '
    nomgp1 = ' '
    nomgp2 = ' '
    iret1 = 0
    iret2 = 0
    iret3 = 0
    do 10 i = 1, ncharp
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.F1D2D', iret1)
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.F2D3D', iret2)
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.PRESS', iret3)
        if (iret1 .ne. 0) then
            cartp1 = lcharp(i)//'.CHME.F1D2D'
            call dismoi('F', 'NOM_GD', cartp1, 'CARTE', ibid,&
                        nomgp1, ier)
            call etenca(cartp1, ligrel, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'CALCULEL4_67')
            endif
        else if (iret2.ne.0) then
            cartp1 = lcharp(i)//'.CHME.F2D3D'
            call dismoi('F', 'NOM_GD', cartp1, 'CARTE', ibid,&
                        nomgp1, ier)
            call etenca(cartp1, ligrel, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'CALCULEL4_67')
            endif
        endif
        if (iret3 .ne. 0) then
            cartp2 = lcharp(i)//'.CHME.PRESS'
            call dismoi('F', 'NOM_GD', cartp2, 'CARTE', ibid,&
                        nomgp2, ier)
            call etenca(cartp2, ligrel, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'CALCULEL4_67')
            endif
        endif
10  end do
!
! ------- FIN TEST SUR LE TYPE DE CHARGE DES BORDS POUR LE PB. PRIMAL
!
! ------- DEBUT TEST SUR LE TYPE DE CHARGE DES BORDS POUR LE PB. DUAL
!
!   ATTENTION : POUR UN MEME CHARGEMENT (FORCE_FACE OU PRES_REP), SEULE
!   LA DERNIERE CHARGE EST CONSIDEREE (REGLE DE SURCHARGE ACTUELLEMENT)
!
    cartd1 = ' '
    cartd2 = ' '
    nomgd1 = ' '
    nomgd2 = ' '
    iret4 = 0
    iret5 = 0
    iret6 = 0
    do 11 i = 1, nchard
        call exisd('CHAMP_GD', lchard(i)//'.CHME.F1D2D', iret4)
        call exisd('CHAMP_GD', lchard(i)//'.CHME.F2D3D', iret5)
        call exisd('CHAMP_GD', lchard(i)//'.CHME.PRESS', iret6)
        if (iret4 .ne. 0) then
            cartd1 = lchard(i)//'.CHME.F1D2D'
            call dismoi('F', 'NOM_GD', cartd1, 'CARTE', ibid,&
                        nomgd1, ier)
            call etenca(cartd1, ligrel, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'CALCULEL4_68')
            endif
        else if (iret5.ne.0) then
            cartd1 = lchard(i)//'.CHME.F2D3D'
            call dismoi('F', 'NOM_GD', cartd1, 'CARTE', ibid,&
                        nomgd1, ier)
            call etenca(cartd1, ligrel, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'CALCULEL4_68')
            endif
        endif
        if (iret6 .ne. 0) then
            cartd2 = lchard(i)//'.CHME.PRESS'
            call dismoi('F', 'NOM_GD', cartd2, 'CARTE', ibid,&
                        nomgd2, ier)
            call etenca(cartd2, ligrel, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'CALCULEL4_68')
            endif
        endif
11  end do
!
! ------- FIN TEST SUR LE TYPE DE CHARGE DES BORDS POUR LE PB. DUAL
!
!
! ------- CREATION DE 2 CARTES CONTENANT DES ADRESSES D'OBJETS JEVEUX --
! ------------------------- PROBLEME PRIMAL ----------------------------
!
    licmpp(1) = 'X1'
    licmpp(2) = 'X2'
    licmpp(3) = 'X3'
    licmpp(4) = 'X4'
    licmpp(5) = 'X5'
    licmpp(6) = 'X6'
    licmpp(7) = 'X7'
    licmpp(8) = 'X8'
    licmpp(9) = 'X9'
    licmpp(10) = 'X10'
    licmpp(11) = 'X11'
    licmpp(12) = 'X12'
!
    call jeveuo(ligrel(1:19)//'.REPE', 'L', iarepe)
    call jeveuo(sigmap(1:19)//'.CELD', 'L', jceldp)
    call jeveuo(sigmap(1:19)//'.CELV', 'L', jcelvp)
!
    if (cartp1 .ne. ' ') then
        call jeveuo(cartp1//'.DESC', 'L', iadep1)
        call jeveuo(cartp1//'.VALE', 'L', iavap1)
        call jeexin(cartp1//'.PTMA', iret)
        if (iret .eq. 0) then
            iptmp1 = 0
        else
!            LA CARTE A ETE ETENDUE
            call jeveuo(cartp1//'.PTMA', 'L', iptmp1)
        endif
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgp1), numgp1)
    else
        iadep1 = 0
        iavap1 = 0
        iptmp1 = 1
        numgp1 = 0
    endif
!
    if (cartp2 .ne. ' ') then
        call jeveuo(cartp2//'.DESC', 'L', iadep2)
        call jeveuo(cartp2//'.VALE', 'L', iavap2)
        call jeexin(cartp2//'.PTMA', iret)
        if (iret .eq. 0) then
            iptmp2 = 0
        else
!            LA CARTE A ETE ETENDUE
            call jeveuo(cartp2//'.PTMA', 'L', iptmp2)
        endif
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgp2), numgp2)
    else
        iadep2 = 0
        iavap2 = 0
        iptmp2 = 1
        numgp2 = 0
    endif
!
    iatyma = tabido(1)
    iagd = tabido(2)
    iacmp = tabido(3)
    iconx1 = tabido(4)
    iconx2 = tabido(5)
!
    icmpp(1) = iarepe
    icmpp(2) = jceldp
    icmpp(3) = jcelvp
    icmpp(4) = iatyma
    icmpp(5) = iagd
    icmpp(6) = iacmp
!
    icmpp(7) = iadep1
    icmpp(8) = iavap1
    icmpp(9) = iptmp1
    icmpp(10) = numgp1
!
    icmpp(11) = iconx1
    icmpp(12) = iconx2
!
!
    call mecact(base, '&&'//nompro//'.CH_FORCEP', 'MODELE', ligrel, 'NEUT_I',&
                nbcmp, licmpp, icmpp, r8bid, c16bid,&
                k8bid)
!
    icmpp(2) = -1
    icmpp(3) = -1
!
    icmpp(5) = iadep2
    icmpp(6) = iavap2
    icmpp(7) = iptmp2
    icmpp(8) = numgp2
!
    call mecact(base, '&&'//nompro//'.CH_PRESSP', 'MODELE', ligrel, 'NEUT_I',&
                nbcmp, licmpp, icmpp, r8bid, c16bid,&
                k8bid)
!
! ------- FIN CREATION CARTES PB. PRIMAL--------------------------------
!
! ------- CREATION DE 2 CARTES CONTENANT DES ADRESSES D'OBJETS JEVEUX --
! --------------------------- PROBLEME DUAL ----------------------------
!
    licmpd(1) = 'X1'
    licmpd(2) = 'X2'
    licmpd(3) = 'X3'
    licmpd(4) = 'X4'
    licmpd(5) = 'X5'
    licmpd(6) = 'X6'
    licmpd(7) = 'X7'
    licmpd(8) = 'X8'
    licmpd(9) = 'X9'
    licmpd(10) = 'X10'
    licmpd(11) = 'X11'
    licmpd(12) = 'X12'
!
    call jeveuo(ligrel(1:19)//'.REPE', 'L', iarepe)
    call jeveuo(sigmad(1:19)//'.CELD', 'L', jceldd)
    call jeveuo(sigmad(1:19)//'.CELV', 'L', jcelvd)
!
    if (cartd1 .ne. ' ') then
        call jeveuo(cartd1//'.DESC', 'L', iaded1)
        call jeveuo(cartd1//'.VALE', 'L', iavad1)
        call jeexin(cartd1//'.PTMA', iret)
        if (iret .eq. 0) then
            iptmd1 = 0
        else
!            LA CARTE A ETE ETENDUE
            call jeveuo(cartd1//'.PTMA', 'L', iptmd1)
        endif
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgd1), numgd1)
    else
        iaded1 = 0
        iavad1 = 0
        numgd1 = 0
        iptmd1 = 1
    endif
!
    if (cartd2 .ne. ' ') then
        call jeveuo(cartd2//'.DESC', 'L', iaded2)
        call jeveuo(cartd2//'.VALE', 'L', iavad2)
        call jeexin(cartd2//'.PTMA', iret)
        if (iret .eq. 0) then
            iptmd2 = 0
        else
!            LA CARTE A ETE ETENDUE
            call jeveuo(cartd2//'.PTMA', 'L', iptmd2)
        endif
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgd2), numgd2)
    else
        iaded2 = 0
        iavad2 = 0
        numgd2 = 0
        iptmd2 = 1
    endif
!
    icmpd(1) = iarepe
    icmpd(2) = jceldd
    icmpd(3) = jcelvd
    icmpd(4) = iatyma
    icmpd(5) = iagd
    icmpd(6) = iacmp
    icmpd(7) = iaded1
    icmpd(8) = iavad1
    icmpd(9) = iptmd1
    icmpd(10) = numgd1
    icmpd(11) = iconx1
    icmpd(12) = iconx2
!
!
    call mecact(base, '&&'//nompro//'.CH_FORCED', 'MODELE', ligrel, 'NEUT_I',&
                nbcmp, licmpd, icmpd, r8bid, c16bid,&
                k8bid)
!
!
    icmpd(2) = -1
    icmpd(3) = -1
!
    icmpd(5) = iaded2
    icmpd(6) = iavad2
    icmpd(7) = iptmd2
    icmpd(8) = numgd2
!
    call mecact(base, '&&'//nompro//'.CH_PRESSD', 'MODELE', ligrel, 'NEUT_I',&
                nbcmp, licmpd, icmpd, r8bid, c16bid,&
                k8bid)
!
! ------- FIN CREATION CARTES PB. DUAL----------------------------------
!
!
!
! ------- DEBUT TEST SUR LES CHARGEMENTS VOLUMIQUES POUR LE PB. PRIMAL -
!  CHARGEMENTS VOLUMIQUES : PESANTEUR, ROTATION OU FORCES DE VOLUME
!       ATTENTION : SEULE LA DERNIERE CHARGE EST CONSIDEREE
!
    iret7 = 0
    iret8 = 0
    iret9 = 0
    iret10 = 0
    chfop1 = ' '
    chfop2 = ' '
    chfop3 = ' '
    typcp3 = '        '
    do 20 i = 1, ncharp
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.PESAN', iret7)
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.ROTAT', iret8)
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.F2D2D', iret9)
        call exisd('CHAMP_GD', lcharp(i)//'.CHME.F3D3D', iret10)
        if (iret7 .ne. 0) then
            chfop1 = lcharp(i)//'.CHME.PESAN.DESC'
        endif
        if (iret8 .ne. 0) then
            chfop2 = lcharp(i)//'.CHME.ROTAT.DESC'
        endif
        if (iret9 .ne. 0) then
            chfop3 = lcharp(i)//'.CHME.F2D2D.DESC'
            call jeveuo(lcharp(i)//'.TYPE', 'L', ibid)
            typcp3 = zk8(ibid)
!GN          WRITE(6,*) 'ON A DU F2D2D AVEC '//CHFOP3//' ET '//TYPCP3
        endif
        if (iret10 .ne. 0) then
            chfop3 = lcharp(i)//'.CHME.F3D3D.DESC'
            call jeveuo(lcharp(i)//'.TYPE', 'L', ibid)
            typcp3 = zk8(ibid)
!GN          WRITE(6,*) 'ON A DU F3D3D AVEC '//CHFOP3//' ET '//TYPCP3
        endif
20  end do
!
! ------- FIN TEST SUR LES CHARGEMENTS VOLUMIQUES POUR LE PB. PRIMAL ---
!
! ------- DEBUT TEST SUR LES CHARGEMENTS VOLUMIQUES POUR LE PB. DUAL ---
!  CHARGEMENTS VOLUMIQUES : PESANTEUR, ROTATION OU FORCES DE VOLUME
!       ATTENTION : SEULE LA DERNIERE CHARGE EST CONSIDEREE
!
    iret11 = 0
    iret12 = 0
    iret13 = 0
    iret14 = 0
    chfod1 = ' '
    chfod2 = ' '
    chfod3 = ' '
    typcd3 = '        '
!
    do 21 i = 1, nchard
        call exisd('CHAMP_GD', lchard(i)//'.CHME.PESAN', iret11)
        call exisd('CHAMP_GD', lchard(i)//'.CHME.ROTAT', iret12)
        call exisd('CHAMP_GD', lchard(i)//'.CHME.F2D2D', iret13)
        call exisd('CHAMP_GD', lchard(i)//'.CHME.F3D3D', iret14)
        if (iret11 .ne. 0) then
            chfod1 = lchard(i)//'.CHME.PESAN.DESC'
        endif
        if (iret12 .ne. 0) then
            chfod2 = lchard(i)//'.CHME.ROTAT.DESC'
        endif
        if (iret13 .ne. 0) then
            chfod3 = lchard(i)//'.CHME.F2D2D.DESC'
            call jeveuo(lcharp(i)//'.TYPE', 'L', ibid)
            typcd3 = zk8(ibid)
!GN          WRITE(6,*) 'ON A DU F2D2D AVEC '//CHFOD3//' ET '//TYPCD3
        endif
        if (iret14 .ne. 0) then
            chfod3 = lchard(i)//'.CHME.F3D3D.DESC'
            call jeveuo(lcharp(i)//'.TYPE', 'L', ibid)
            typcd3 = zk8(ibid)
!GN          WRITE(6,*) 'ON A DU F3D3D AVEC '//CHFOD3//' ET '//TYPCD3
        endif
21  end do
!
! ------- FIN TEST SUR LES CHARGEMENTS VOLUMIQUES POUR LE PB. DUAL ---
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PVOISIN'
    lchin(3) = chvois
    lpain(4) = 'PTEMPSR'
    lchin(4) = chtime
    lpain(5) = 'PCONTNOP'
    lchin(5) = sigmap
    lpain(6) = 'PCONTNOD'
    lchin(6) = sigmad
    lpain(7) = 'PPESANRP'
    lchin(7) = chfop1
    lpain(8) = 'PPESANRD'
    lchin(8) = chfod1
    lpain(9) = 'PROTATRP'
    lchin(9) = chfop2
    lpain(10) = 'PROTATRD'
    lchin(10) = chfod2
    lpain(11) = 'PFORCEP'
    lchin(11) = '&&'//nompro//'.CH_FORCEP'
    lpain(12) = 'PFORCED'
    lchin(12) = '&&'//nompro//'.CH_FORCED'
    lpain(13) = 'PPRESSP'
    lchin(13) = '&&'//nompro//'.CH_PRESSP'
    lpain(14) = 'PPRESSD'
    lchin(14) = '&&'//nompro//'.CH_PRESSD'
    lpain(15) = 'PCONSTR'
    lchin(15) = chs
    nbrin = 15
!
    if (typcp3(1:1) .ne. ' ') then
        nbrin = nbrin + 1
        if (typcp3(1:7) .eq. 'MECA_RE') then
            lpain(nbrin) = 'PFRVOLUP'
        else if (typcp3(1:7).eq.'MECA_FO') then
            lpain(nbrin) = 'PFFVOLUP'
        endif
        lchin(nbrin) = chfop3
    endif
!
    if (typcd3(1:1) .ne. ' ') then
        nbrin = nbrin + 1
        if (typcd3(1:7) .eq. 'MECA_RE') then
            lpain(nbrin) = 'PFRVOLUD'
        else if (typcd3(1:7).eq.'MECA_FO') then
            lpain(nbrin) = 'PFFVOLUD'
        endif
        lchin(nbrin) = chfod3
    endif
!
    lpaout(1) = 'PERREUR'
    lchout(1) = chelem
!
    option = 'QIRE_ELEM'
!
!GN      WRITE(6,*) NOMPRO,' APPELLE CALCUL POUR ', OPTION
!GN      WRITE(6,*) '  LPAIN    LCHIN'
!GN      DO 33 , IBID = 1 , NBRIN
!GN        WRITE(6,3000) IBID, LPAIN(IBID), LCHIN(IBID)
!GN   33 CONTINUE
!GN 3000 FORMAT(I2,1X,A8,1X,A24)
!
    call calcul('C', option, ligrel, nbrin, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
    call exisd('CHAMP_GD', lchout(1), iret)
    if (iret .eq. 0) then
        call u2mesk('F', 'CALCULEL2_88', 1, option)
    endif
!
!====
! 4. MENAGE FINAL
!====
!
    call jedetr(cartp1//'.PTMA')
    call jedetr(cartp2//'.PTMA')
    call jedetr(cartd1//'.PTMA')
    call jedetr(cartd2//'.PTMA')
!
    call detrsd('CHAMP_GD', '&&'//nompro//'.CH_FORCEP')
    call detrsd('CHAMP_GD', '&&'//nompro//'.CH_PRESSP')
    call detrsd('CHAMP_GD', '&&'//nompro//'.CH_FORCED')
    call detrsd('CHAMP_GD', '&&'//nompro//'.CH_PRESSD')
!
end subroutine
