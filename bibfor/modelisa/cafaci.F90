subroutine cafaci(fonree, char)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getmjm.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterfort/afddli.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/afrela.h'
    include 'asterfort/assert.h'
    include 'asterfort/canort.h'
    include 'asterfort/celces.h'
    include 'asterfort/cncinv.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/juveca.h'
    include 'asterfort/kndiff.h'
    include 'asterfort/mainoe.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xddlim.h'
    character(len=4) :: fonree
    character(len=8) :: char
! ---------------------------------------------------------------------
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
!
!     BUT: CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
!          ET REMPLIR LIGRCH POUR FACE_IMPO
!
! ARGUMENTS D'ENTREE:
!      FONREE  : TYPE DE LA VALEUR IMPOSEE :
!                REEL OU FONC OU COMP
!      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE
!
! TOLE CRP_20
!
    integer :: nmocl
    integer :: vali(2)
    parameter (nmocl=300)
!
    integer :: i, j, k, n, jlisti, jnoxfl, jnoxfv
    integer :: nbnoeu, jval, nddla, jdirec, nbno, nelim
    integer :: idim, in, jnorm, jtang, jnono, nfaci
    integer :: ibid, jnoma, ier, ndim, nbma2, jcompt
    integer :: n1, n2, ino, jprnm, nbec, nmcl, inor, icmp
    integer :: nbma, nbcmp, inom
    integer :: jlist2, jlist3, nbno1, nbno3
    integer :: ddlimp(nmocl), nbno2, jlino2, jlino1, jlino, jlinu
    real(kind=8) :: valimr(nmocl), coef(3), direct(3)
    complex(kind=8) :: valimc(nmocl), coefc(3)
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=3) :: tymocl(nmocl)
    character(len=4) :: typcoe
    character(len=8) :: k8b, noma, mod, nomg
    character(len=8) :: valimf(nmocl), nomnoe, nomno, ddl(3)
    character(len=16) :: motfac, motcle(nmocl), nomcmd, typmcl(2), moclm(2)
    character(len=16) :: moclm2(2), moclm3(2), typmc3(2)
    character(len=24) :: mesmai, mesma2, lnoeu2, lnoeu1, mesno3
    character(len=19) :: cnxinv
    character(len=19) :: ligrmo
    character(len=19) :: lisrel, noxfem
    logical :: lxfem
    character(len=19) :: ch1, ch2, ch3
    integer :: iarg
!
    call jemarq()
    call getfac('FACE_IMPO', nfaci)
    if (nfaci .eq. 0) goto 999
    call getres(k8b, k8b, nomcmd)
!
    lisrel = '&&CAFACI.RLLISTE'
!
    mesmai = '&&CAFACI.MES_MAILLES'
    mesma2 = '&&CAFACI.MAILLES_2'
    mesno3 = '&&CAFACI.NOEUDS_2'
    motfac = 'FACE_IMPO     '
!
    moclm(1) = 'MAILLE'
    moclm(2) = 'GROUP_MA'
    moclm2(1) = 'SANS_MAILLE'
    moclm2(2) = 'SANS_GROUP_MA'
    typmcl(1) = 'MAILLE'
    typmcl(2) = 'GROUP_MA'
!
    moclm3(1) = 'SANS_NOEUD'
    moclm3(2) = 'SANS_GROUP_NO'
    typmc3(1) = 'NOEUD'
    typmc3(2) = 'GROUP_NO'
!
    typlag = '12'
    ddl(1) = 'DX      '
    ddl(2) = 'DY      '
    ddl(3) = 'DZ      '
!
    coefc(1) = (1.0d0,0.0d0)
    coefc(2) = (1.0d0,0.0d0)
    coefc(3) = (1.0d0,0.0d0)
!
    typcoe = 'REEL'
    if (fonree .eq. 'COMP') typcoe = 'COMP'
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
!
    call dismoi('F', 'NOM_MODELE', char(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! ---  LIGREL DU MODELE ---
!
    ligrmo = mod//'.MODELE'
!
    if (nomcmd(11:14) .eq. 'MECA') then
        nomg='DEPL_R'
    else if (nomcmd(11:14).eq.'THER') then
        nomg='TEMP_R'
    else if (nomcmd(11:14).eq.'ACOU') then
        nomg='PRES_C'
    else
        call assert(.false.)
    endif
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
!
! --- MAILLAGE ASSOCIE AU MODELE ---
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
! ---------------------------------------------------
! *** RECUPERATION DU DESCRIPTEUR GRANDEUR .PRNM
! *** DU MODELE
! ---------------------------------------------------
    call dismoi('F', 'NB_NO_MAILLA', ligrmo, 'LIGREL', n1,&
                k8b, ier)
    call jelira(ligrmo//'.PRNM', 'LONMAX', n2, k1bid)
    nbec = n2/n1
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    else
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
!
!     --------------------------------------------------------
!     RECUPERATION DE LA DIMENSION DE L'ESPACE DES COORDONNEES
!     --------------------------------------------------------
    call dismoi('F', 'DIM_GEOM', mod, 'MODELE', ndim,&
                k8b, ier)
!
!    --------------------------------------------------------
!    MODELE X-FEM
!    --------------------------------------------------------
    call jeexin(mod//'.XFEM_CONT', ier)
    if (ier .eq. 0) then
        lxfem = .false.
        noxfem = ' '
        ch1 = ' '
        ch2 = ' '
        ch3 = ' '
    else
        lxfem = .true.
!       RECUPERATION DE LA CONNECTIVITE INVERSE
        cnxinv='&&CAFACI.CNXINV'
        call cncinv(noma, ibid, 0, 'V', cnxinv)
!
        noxfem = '&&CAFACI.NOXFEM'
        call cnocns(mod//'.NOXFEM', 'V', noxfem)
        call jeveuo(noxfem//'.CNSL', 'L', jnoxfl)
        call jeveuo(noxfem//'.CNSV', 'L', jnoxfv)
!       STATUT DU NOEUD ET LEVEL SETS
        ch1 = '&&CAFACI.CHS1'
        ch2 = '&&CAFACI.CHS2'
        ch3 = '&&CAFACI.CHS3'
        call celces(mod//'.STNO', 'V', ch1)
        call celces(mod//'.LNNO', 'V', ch2)
        call celces(mod//'.LTNO', 'V', ch3)
    endif
!
    do 210 i = 1, nfaci
        icmp = 0
        inor = 0
        nbma2= 0
        nbno3= 0
!
! ----- RECUPERATION DES MAILLES
        call reliem(' ', noma, 'NU_MAILLE', motfac, i,&
                    2, moclm, typmcl, mesmai, nbma)
        if (nbma .eq. 0) goto 210
        call jeveuo(mesmai, 'L', jlisti)
!
! ----- RECUPERATION DES MAILLES (A EXCLURE)
        call reliem(' ', noma, 'NU_MAILLE', motfac, i,&
                    2, moclm2, typmcl, mesma2, nbma2)
!
! ----- RECUPERATION DES NOEUDS (A EXCLURE)
        call reliem(' ', noma, 'NO_NOEUD', motfac, i,&
                    2, moclm3, typmc3, mesno3, nbno3)
!
! ---------------------------------------------------
!     RECUPERATION DES MOTS-CLES DDL SOUS FACE_IMPO
!     MOTCLE(J) : K8 CONTENANT LE J-EME MOT-CLE DDL
!     NDDLA     : NOMBRE DE MOTS CLES DU TYPE DDL
! ---------------------------------------------------
        call getmjm('FACE_IMPO', i, 0, motcle, tymocl,&
                    n)
        nmcl = -n
        if (nmcl .gt. nmocl) then
            vali (1) = nmocl
            vali (2) = nmcl
            call u2mesg('F', 'MODELISA8_31', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
        call getmjm('FACE_IMPO', i, nmcl, motcle, tymocl,&
                    n)
        nddla = 0
        do 50 j = 1, nmcl
            if (motcle(j) .ne. 'MAILLE' .and. motcle(j) .ne. 'GROUP_MA' .and. motcle(j)&
                .ne. 'SANS_MAILLE' .and. motcle(j) .ne. 'SANS_GROUP_MA' .and. motcle(j)&
                .ne. 'SANS_NOEUD' .and. motcle(j) .ne. 'SANS_GROUP_NO' .and. motcle(j) .ne.&
                'DNOR' .and. motcle(j) .ne. 'DTAN') then
                nddla = nddla + 1
                motcle(nddla) = motcle(j)
            endif
50      continue
        motcle(nddla+1) = 'DNOR'
        motcle(nddla+2) = 'DTAN'
        if (fonree .eq. 'REEL') then
            do 60 j = 1, nddla + 2
                call getvr8('FACE_IMPO', motcle(j), i, iarg, 1,&
                            valimr(j), ddlimp(j))
                if (j .le. nddla) then
                    icmp = icmp + ddlimp(j)
                else
                    inor = inor + ddlimp(j)
                endif
60          continue
            if (ndim .eq. 3 .and. ddlimp(nddla+2) .ne. 0) then
                call u2mess('F', 'MODELISA2_63')
            endif
        else
            do 70 j = 1, nddla + 2
                call getvid('FACE_IMPO', motcle(j), i, iarg, 1,&
                            valimf(j), ddlimp(j))
                if (j .le. nddla) then
                    icmp = icmp + ddlimp(j)
                else
                    inor = inor + ddlimp(j)
                endif
70          continue
!
!         -- SI DNOR OU DTAN, IL FAUT SAVOIR SI NDIM=2/3 :
            if (ddlimp(nddla+1) .ne. 0 .or. ddlimp(nddla+2) .ne. 0) then
                if (.not.(ndim.eq.2.or.ndim.eq.3)) call u2mess('F', 'MODELISA2_6')
            endif
!
!
            if (ndim .eq. 3 .and. ddlimp(nddla+2) .ne. 0) then
                call u2mess('F', 'MODELISA2_63')
            endif
        endif
!
!      ***************************************
!      TRAITEMENT DES COMPOSANTES DNOR ET DTAN
!      ***************************************
!    -------------------------------------
!    RECUPERATION DES NOEUDS (A CONSERVER)
!    -------------------------------------
        if ((nbma2.ne.0) .or. (nbno3.ne.0)) then
!
!           LISTE DES NOMS DES NOEUDS
            lnoeu1='&&CAFACI.NOEU_MAIL.TOTAL'
            call jedetr(lnoeu1)
            call mainoe(noma, nbma, zi(jlisti), 'NO', nbno1,&
                        lnoeu1)
            call jeveuo(lnoeu1, 'L', jlino1)
!           LISTE DES NOMS DES NOEUDS DES MAILLES EXCLUES
            lnoeu2='&&CAFACI.NOEU_MAIL.EXCL'
            call jedetr(lnoeu2)
            if (nbma2 .ne. 0) then
!              LISTE DES NUM DES MAILLES EXCLUES
                call jeveuo(mesma2, 'L', jlist2)
!              LISTE DES NOMS DES NOEUDS DES MAILLES EXCLUES
                call mainoe(noma, nbma2, zi(jlist2), 'NO', nbno2,&
                            lnoeu2)
            endif
            if (nbno3 .ne. 0) then
!              LISTE DES NOMS DES NOEUDS EXCLUS : SANS_(NOEUD,GROUP_NO)
                call jeveuo(mesno3, 'L', jlist3)
!              SI NBMA2<>0
!                 AGRANDIR LNOEU2 DE NBNO3 ET AJOUTER MESNO3
!                 EN SUPPRIMANT LES EVENTUELS DOUBLONS
!              SINON
!                 CREER LNOEU2 ET COPIER MESNO3
                if (nbma2 .ne. 0) then
                    call juveca(lnoeu2, nbno2+nbno3)
                    call jeveuo(lnoeu2, 'E', jlino2)
!                 COMPLETER AVEC LES NOEUDS
                    nelim = 0
                    do 300 j = 0, nbno3-1
                        nomno = zk8(jlist3+j)
                        do 301 k = 0, nbno2-1
                            if (nomno .eq. zk8(jlino2+k)) then
                                nelim = nelim + 1
                                goto 300
                            endif
301                      continue
                        zk8(jlino2+nbno2+j-nelim) = nomno
300                  continue
                    nbno2=nbno2+nbno3-nelim
                else
                    call wkvect(lnoeu2, 'V V K8', nbno3, jlino2)
                    do 310 j = 0, nbno3-1
                        zk8(jlino2+j) = zk8(jlist3+j)
310                  continue
                    nbno2=nbno3
                endif
            endif
            call jeveuo(lnoeu2, 'L', jlino2)
!
!           LISTE DES NOMS DES NOEUDS A CONSERVER
            call jedetr('&&CAFACI.NOEU_NOM')
            call wkvect('&&CAFACI.NOEU_NOM', 'V V K8', nbno1, jlino)
            nbno = nbno1
            call kndiff(8, zk8(jlino1), nbno1, zk8(jlino2), nbno2,&
                        zk8(jlino), nbno)
!
!           LISTE DES NUMEROS DES NOEUDS A CONSERVER
            call jedetr('&&CAFACI.NOEU_NUM')
            call wkvect('&&CAFACI.NOEU_NUM', 'V V I', nbno, jlinu)
            do 71 j = 1, nbno
                call jenonu(jexnom(noma//'.NOMNOE', zk8(jlino+j-1)), zi(jlinu+j-1))
71          continue
!
        else
!           LISTE DES NUMEROS DES NOEUDS
            lnoeu1='&&CAFACI.NOEU_MAIL.TOTAL'
            call jedetr(lnoeu1)
            call mainoe(noma, nbma, zi(jlisti), 'NU', nbno,&
                        lnoeu1)
            call jeveuo(lnoeu1, 'L', jlinu)
        endif
!
!   -------------------------------------------
!   CALCUL DES NORMALES ET TANGENTES AUX NOEUDS
!   -------------------------------------------
        if (ddlimp(nddla+1) .ne. 0) then
            call canort(noma, nbma, zi(jlisti), k8b, ndim,&
                        nbno, zi(jlinu), 1)
            call jeveuo('&&CANORT.NORMALE', 'L', jnorm)
        endif
        if (ddlimp(nddla+2) .ne. 0) then
            call canort(noma, nbma, zi(jlisti), k8b, ndim,&
                        nbno, zi(jlinu), 2)
            call jeveuo('&&CANORT.TANGENT', 'L', jtang)
        endif
!   ----------------------
!   AFFECTATION DES COMPOSANTES DE LA RELATION A LA RELATION COURANTE
!   ----------------------
        coef(1) = 1.0d0
        ddl(1) = 'DEPL'
        do 120 ino = 1, nbno
            in = zi(jlinu+ino-1)
!
            call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
!
            if (ddlimp(nddla+1) .ne. 0) then
                do 100 idim = 1, ndim
                    direct(idim) = zr(jnorm-1+ndim* (ino-1)+idim)
100              continue
!
                if (lxfem) then
                    if (zl(jnoxfl-1+2*in)) then
                        call xddlim(mod, ddl, nomnoe, in, valimr(nddla+1),&
                                    valimc(j), valimf(j), fonree, ibid, lisrel,&
                                    ndim, direct, jnoxfv, ch1, ch2,&
                                    ch3, cnxinv)
                        goto 105
                    endif
                endif
!
                call afrela(coef, coefc, ddl, nomnoe, ndim,&
                            direct, 1, valimr(nddla+1), valimc(nddla+1), valimf(nddla+1),&
                            typcoe, fonree, typlag, 0.d0, lisrel)
!
            endif
!
105          continue
!
            if (ddlimp(nddla+2) .ne. 0) then
                do 110 idim = 1, ndim
                    direct(idim) = zr(jtang-1+ndim* (ino-1)+idim)
110              continue
!
                if (lxfem) then
                    if (zl(jnoxfl-1+2*in)) then
                        call xddlim(mod, ddl, nomnoe, in, valimr(nddla+2),&
                                    valimc(j), valimf(j), fonree, ibid, lisrel,&
                                    ndim, direct, jnoxfv, ch1, ch2,&
                                    ch3, cnxinv)
                        goto 120
                    endif
                endif
!
                call afrela(coef, coefc, ddl, nomnoe, ndim,&
                            direct, 1, valimr(nddla+2), valimc(nddla+2), valimf(nddla+2),&
                            typcoe, fonree, typlag, 0.d0, lisrel)
            endif
!
120      continue
!
!      ***************************************************
!      TRAITEMENT DES COMPOSANTES DX DY DZ DRX DRY DRZ ...
!      ***************************************************
!
        if (icmp .eq. 0) goto 200
!
        call jelira(noma//'.NOMNOE', 'NOMMAX', nbnoeu, k1bid)
!
!    ALLOCATION DE 3 OBJETS INTERMEDIAIRES PERMETTANT D'APPLIQUER
!    LA REGLE DE SURCHARGE :
!
!               - VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS
!               - TABLEAU DES VALEURS DES DDLS DES NOEUDS BLOQUES
!                 DIM NBNOEU * NBCOMP
!               - VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR
!                 ASSOCIE AUX DDLS IMPOSES PAR NOEUD
!
        call wkvect('&&CAFACI.NOMS_NOEUDS', 'V V K8', nbnoeu, jnono)
        if (fonree .eq. 'REEL') then
            call wkvect('&&CAFACI.VALDDL', 'V V R', nddla*nbnoeu, jval)
        else
            call wkvect('&&CAFACI.VALDDL', 'V V K8', nddla*nbnoeu, jval)
        endif
!
        call wkvect('&&CAFACI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
!
        call wkvect('&&CAFACI.ICOMPT', 'V V I', nddla, jcompt)
        do 180 ino = 1, nbno
            in = zi(jlinu-1+ino)
            call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
            zk8(jnono-1+in) = nomnoe
            call afddli(zr(jval), zk8(jval), zc(jval), zi(jprnm-1+ (in- 1)*nbec+1), nddla,&
                        fonree, nomnoe, in, ddlimp, valimr,&
                        valimf, valimc, motcle, zr(jdirec+3* (in-1)), 0,&
                        mod, lisrel, zk8( inom), nbcmp, zi(jcompt),&
                        lxfem, jnoxfl, jnoxfv, ch1, ch2,&
                        ch3, cnxinv)
180      continue
        do 181,k=1,nddla
        if (zi(jcompt-1+k) .eq. 0) call u2mesk('F', 'MODELISA2_45', 1, motcle(k))
181      continue
        call jedetr('&&CAFACI.ICOMPT')
!
        call jedetr('&&CAFACI.NOMS_NOEUDS')
        call jedetr('&&CAFACI.VALDDL')
        call jedetr('&&CAFACI.DIRECT')
!
200      continue
!
        call jedetr(mesmai)
        call jedetr(mesma2)
!
210  end do
!        -------------------------------------
!        AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE
!        (I.E. AFFECTATION DES OBJETS .CMULT, .CIMPO,
!        LIGRCH ET .NEMA)
!        -------------------------------------
    call aflrch(lisrel, char)
!
    call jedetr('&&NBNLMA.LN')
    call jedetr('&&NBNLMA.NBN')
    call jedetr('&&CANORT.NORMALE')
    call jedetr('&&CANORT.TANGENT')
    if (lxfem) then
        call jedetr(cnxinv)
        call detrsd('CHAM_NO_S', noxfem)
        call detrsd('CHAM_ELEM_S', ch1)
        call detrsd('CHAM_ELEM_S', ch2)
        call detrsd('CHAM_ELEM_S', ch3)
    endif
!
999  continue
    call jedema()
end subroutine
