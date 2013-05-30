subroutine xpomax(mo, malini, mailx, nbnoc, nbmac,&
                  prefno, nogrfi, maxfem, cns1, cns2,&
                  ces1, ces2, cesvi1, cesvi2, listgr,&
                  dirgrm, nivgrm, resuco, ngfon, comps1,&
                  comps2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
! TOLE CRP_20
! TOLE CRP_21
    implicit none
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterfort/assert.h'
    include 'asterfort/celces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/elref2.h'
    include 'asterfort/exisd.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xismec.h'
    include 'asterfort/xpoajc.h'
    include 'asterfort/xpoajm.h'
    include 'asterfort/xpocmp.h'
    include 'asterfort/xpocox.h'
    integer :: nbnoc, nbmac, ngfon
    character(len=2) :: prefno(4)
    character(len=8) :: mo, malini, maxfem, resuco
    character(len=19) :: cns1, cns2, ces1, ces2, cesvi1, cesvi2
    character(len=19) :: comps1, comps2
    character(len=24) :: mailx, listgr, dirgrm, nivgrm, gpptnn, nogrfi
!
!      TRAITEMENT DES MAILLES DE MAILX
!       - POUR POST_MAIL_XFEM : CREATION DES MAILLES CORRESPONDANTS
!                               AUX SOUS-ELEMENTS ET CREATION DES NOEUDS
!                               CORRESPONDANTS AUX SOMMETS DES SOUS
!                               ELEMENTS
!       - POUR POST_CHAM_XFEM : CALCUL DES DEPLACEMENTS AUX NOUVEAUX
!                               NOEUDS DE MAXFEM
!
!
!   IN
!       MO     : MODELE FISSURE
!       MALINI : MAILLAGE SAIN
!       MAILX  : LISTE DES NUMEROS DES MAILLES SOUS-DECOUPEES
!       NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
!       NBMAC  : NOMBRE DE MAILLES CLASSIQUES DU MAILLAGE FISSURE
!       PREFNO : PREFERENCES POUR LE NOMAGE DES NOUVELLES ENTITES
!       NOGRFI : NOM DU GROUPE DE NOEUDS SUR LA FISSURE A CREER
!       MAXFEM : MAILLAGE FISSURE (SI POST_CHAMP_XFEM)
!       CNS1   : CHAMP_NO_S DU DEPLACEMENT EN ENTREE
!       CES1   : CHAMP_ELEM_S DE CONTRAINTES EN ENTREE
!       LISTGR : LISTE DES GROUPES CONTENANT CHAQUE MAILLE
!       DIRGRM : VECTEUR D'INDIRECTION ENTRE LES GROUP_MA
!       NIVGRM : VECTEUR DE REMPLISSAGE DES GROUP_MA DE MAXFEM
!       RESUCO : NOM DU CONCEPT RESULTAT DONT ON EXTRAIT LES CHAMPS
!       NGFON  : NOMBRE TOTAL DE FOND DE FISSURES
!       COMPS1 : CHAM_ELEM_S DU COMPORTEMENT EN ENTREE
!   OUT
!       MAXFEM : MAILLAGE FISSURE (SI POST_MAIL_XFEM)
!       CNS2   : CHAMP_NO_S DU DEPLACEMENT EN SORTIE
!       CES2   : CHAMP_ELEM_S DE CONTRAINTES EN SORTIE
!       NIVGRM : VECTEUR DE REMPLISSAGE DES GROUP_MA DE MAXFEM
!       COMPS2 : CHAM_ELEM_S DU COMPORTEMENT EN SORTIE
!
!
!
!
!
    integer :: i, ier, jmax, nbmax, ich, ima, nse, ise, in
    integer :: jcesd(11), jcesv(11), jcesl(11), iad, jconx1, jconx2
    integer :: j, ino, n, jdirno, jlsn, inn, nnn, nbnoma, nfiss, ifiss
    integer :: iacoo1, iacoo2, ndim, iad2, inntot, ndime, jtmdim, inm
    integer :: jtypm1, jtypm2, inmtot, itypse(6), iad1, iadc, iadv
    integer :: jcnse, iad4, iad3, jmail, itypel, nbelr, jhea
    integer :: igeom, nfh, ifh, nfe, ddlc, cmp(50), jcnsd1, jlst, jfisno
    integer :: nbcmp, jcnsv1, jcnsv2, nbnofi, inofi
    integer :: jcnsl2, jcesv1, jcesd1, jcesl1, jcesv2, jcesd2, jcesl2
    integer :: jcviv1, jcvid1, jcvil1, jcviv2, jcvid2, jcvil2, ninter
    integer :: jnivgr, iagma, ngrm, jdirgr, iagno, iad10, iad11, npg
    character(len=8) :: k8b, typese(6), elrefp, lirefe(10)
    character(len=8) :: typma, noma
    character(len=16) :: tysd, k16b, nomcmd, notype
    character(len=19) :: chs(11)
    character(len=24) :: dirno, geom, linofi, grpnoe, lsn, lst, fisno, hea
    character(len=24) :: nogno
    logical :: opmail, lmeca
    integer :: iad9, irese, nnose, tabse(6), ncomp
    integer :: iviex, iret, jtypma, jconq1, jconq2, jcnsk1, jxc
    integer :: jresd1, jresv1, jresl1, nbcmpc, jresd2, jresv2, jresl2
!
    data  typese /'SEG2','TRIA3','TETRA4','SEG3','TRIA6','TETRA4'/
    data  tabse  /   2  ,   3   ,   4    ,   3  ,   6   ,   4    /
!
    call jemarq()
!
    call jeexin(mailx, ier)
    if (ier .eq. 0) goto 999
!
!     ------------------------------------------------------------------
!                   RECUPERATION DES OBJETS JEVEUX
!     ------------------------------------------------------------------
!
    call dismoi('F', 'DIM_GEOM', malini, 'MAILLAGE', ndim,&
                k8b, ier)
!
!     NOM DE LA COMMANDE (POST_MAIL_XFEM OU POST_CHAM_XFEM)
    call getres(k8b, k16b, nomcmd)
    if (nomcmd .eq. 'POST_MAIL_XFEM') then
        opmail = .true.
    else if (nomcmd.eq.'POST_CHAM_XFEM') then
        opmail = .false.
    endif
!
    call jeveuo(mailx, 'L', jmax)
    call jelira(mailx, 'LONMAX', nbmax, k8b)
!
    chs(1) = '&&XPOMAX.PINTTO'
    chs(2) = '&&XPOMAX.CNSETO'
    chs(3) = '&&XPOMAX.LONCHA'
    chs(4) = '&&XPOMAX.HEAV'
    chs(6) = '&&XPOMAX.CNSLN'
    chs(7) = '&&XPOMAX.CNSLT'
    chs(8) = '&&XPOMAX.CNSFI'
    chs(9) = '&&XPOMAX.PMILTO'
    chs(10) = '&&XPOMAX.PLONCH'
    chs(11) = '&&XPOMAX.PAIN'
!
!
    call celces(mo//'.TOPOSE.PIN', 'V', chs(1))
    call celces(mo//'.TOPOSE.CNS', 'V', chs(2))
    call celces(mo//'.TOPOSE.LON', 'V', chs(3))
    call celces(mo//'.TOPOSE.HEA', 'V', chs(4))
    call celces(mo//'.LNNO', 'V', chs(6))
    call celces(mo//'.LTNO', 'V', chs(7))
!
    call jeexin(mo//'.FISSNO    .CELD', ier)
    if (ier .ne. 0) then
        call celces(mo//'.FISSNO', 'V', chs(8))
        call jeveuo(chs(8)//'.CESD', 'L', jcesd(8))
        call jeveuo(chs(8)//'.CESV', 'E', jcesv(8))
        call jeveuo(chs(8)//'.CESL', 'L', jcesl(8))
    endif
!
    call jeexin(mo//'.TOPOSE.PMI.CELD', ier)
    if (ier .ne. 0) then
        call celces(mo//'.TOPOSE.PMI', 'V', chs(9))
        call jeveuo(chs(9)//'.CESD', 'L', jcesd(9))
        call jeveuo(chs(9)//'.CESV', 'E', jcesv(9))
        call jeveuo(chs(9)//'.CESL', 'L', jcesl(9))
    endif
!
!      CALL JEEXIN(MO//'.TOPOFAC.LO.CELD',IER)
!      IF (IER.NE.0) THEN
    call celces(mo//'.TOPOFAC.LO', 'V', chs(10))
    call jeveuo(chs(10)//'.CESD', 'L', jcesd(10))
    call jeveuo(chs(10)//'.CESV', 'E', jcesv(10))
    call jeveuo(chs(10)//'.CESL', 'L', jcesl(10))
!      ENDIF
!
!      CALL JEEXIN(MO//'.TOPOFAC.AI.CELD',IER)
!      IF (IER.NE.0) THEN
    call celces(mo//'.TOPOFAC.AI', 'V', chs(11))
    call jeveuo(chs(11)//'.CESD', 'L', jcesd(11))
    call jeveuo(chs(11)//'.CESV', 'E', jcesv(11))
    call jeveuo(chs(11)//'.CESL', 'L', jcesl(11))
!      ENDIF
!
    do 10 ich = 1, 4
        call jeveuo(chs(ich)//'.CESD', 'L', jcesd(ich))
        call jeveuo(chs(ich)//'.CESV', 'E', jcesv(ich))
        call jeveuo(chs(ich)//'.CESL', 'L', jcesl(ich))
10  end do
    do 11 ich = 6, 7
        call jeveuo(chs(ich)//'.CESD', 'L', jcesd(ich))
        call jeveuo(chs(ich)//'.CESV', 'E', jcesv(ich))
        call jeveuo(chs(ich)//'.CESL', 'L', jcesl(ich))
11  end do
!
    call jeveuo(malini//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(malini//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    call jeveuo(malini//'.COORDO    .VALE', 'L', iacoo1)
    call jeveuo(maxfem//'.COORDO    .VALE', 'E', iacoo2)
!
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
    call jeveuo(malini//'.TYPMAIL', 'L', jtypm1)
    call jeveuo(maxfem//'.TYPMAIL', 'E', jtypm2)
    call jeveuo(mo//'.MAILLE', 'L', jmail)
!
    if (.not.opmail) then
        call jeveuo(cns1//'.CNSK', 'L', jcnsk1)
        call jeveuo(cns1//'.CNSD', 'L', jcnsd1)
        call jeveuo(cns1//'.CNSV', 'L', jcnsv1)
        call jeveuo(cns2//'.CNSV', 'E', jcnsv2)
        call jeveuo(cns2//'.CNSL', 'E', jcnsl2)
!
!  -----SI ON N'A PAS UNE MODE_MECA
!
        call gettco(resuco, tysd)
        if (tysd(1:9) .ne. 'MODE_MECA' .and. tysd(1:9) .ne. 'EVOL_THER') then
            call jeveuo(ces1//'.CESV', 'L', jcesv1)
            call jeveuo(ces1//'.CESD', 'L', jcesd1)
            call jeveuo(ces1//'.CESL', 'L', jcesl1)
            call jeveuo(ces2//'.CESV', 'E', jcesv2)
            call jeveuo(ces2//'.CESD', 'L', jcesd2)
            call jeveuo(ces2//'.CESL', 'E', jcesl2)
!
            call jeexin(cesvi1//'.CESV', iret)
            if (iret .ne. 0) then
                call jeveuo(cesvi1//'.CESV', 'L', jcviv1)
                call jeveuo(cesvi1//'.CESD', 'L', jcvid1)
                call jeveuo(cesvi1//'.CESL', 'L', jcvil1)
            else
                jcviv1 = 0
                jcvid1 = 0
                jcvil1 = 0
            endif
            iviex = iret
!
            call jeexin(cesvi2//'.CESV', iret)
            if (iret .ne. 0) then
                call jeveuo(cesvi2//'.CESV', 'E', jcviv2)
                call jeveuo(cesvi2//'.CESD', 'L', jcvid2)
                call jeveuo(cesvi2//'.CESL', 'E', jcvil2)
            else
                jcviv2 = 0
                jcvid2 = 0
                jcvil2 = 0
            endif
            iviex = iviex*iret
!
        endif
!
!       COMPORTEMENT
!       RECUPERATION DU CHAM_ELEM_S DU COMPORTEMENT EN ENTREE
        call exisd('CHAM_ELEM_S', comps1, iret)
        if (iret .ne. 0) then
!
!         RECUP DES INFOS SUR LE CHAM_ELEM_S DU COMPORTEMENT EN ENTREE
            call jeveuo(comps1//'.CESD', 'L', jresd1)
            call jeveuo(comps1//'.CESV', 'L', jresv1)
            call jeveuo(comps1//'.CESL', 'L', jresl1)
!
!         NB CMP DU COMPORTEMENT
            nbcmpc = zi(jresd1-1+2)
!
!         VERIF QUE LE CHAMP DE SORTIE A BIEN ETE CREE
            call exisd('CHAM_ELEM_S', comps2, iret)
            call assert(iret.ne.0)
!
!         RECUP DES INFOS SUR LE CHAM_ELEM_S DU COMPORTEMENT EN SORTIE
            call jeveuo(comps2//'.CESD', 'L', jresd2)
            call jeveuo(comps2//'.CESV', 'E', jresv2)
            call jeveuo(comps2//'.CESL', 'E', jresl2)
        else
!         ON MET A ZERO NB CMP
            nbcmpc = 0
        endif
!
    endif
!
!     RECUP DES NUMEROS DES TYPE DE MAILLES DES SOUS-ELEMENTS
    call jenonu(jexnom('&CATA.TM.NOMTM', typese(1)), itypse(1))
    call jenonu(jexnom('&CATA.TM.NOMTM', typese(2)), itypse(2))
    call jenonu(jexnom('&CATA.TM.NOMTM', typese(3)), itypse(3))
    call jenonu(jexnom('&CATA.TM.NOMTM', typese(4)), itypse(4))
    call jenonu(jexnom('&CATA.TM.NOMTM', typese(5)), itypse(5))
    call jenonu(jexnom('&CATA.TM.NOMTM', typese(6)), itypse(6))
!
!     COMPTEURS DU NOMBRE DE NOUVEAUX NOEUDS ET MAILLES TOTAL
    inntot = 0
    inmtot = 0
!
!     COMPTEUR ET LISTE DE NOEUDS PORTANT DES DDLS DE CONTACT
    linofi='&&XPOMAX.LINOFI'
!      LINOLA='&&XPOMAX.LINOLA'
    inofi=-1
    if (opmail) then
        call dismoi('F', 'NB_NO_MAILLA', maxfem, 'MAILLAGE', nbnofi,&
                    k8b, iret)
        call wkvect(linofi, 'V V I', nbnofi, inofi)
!       CALL WKVECT(LINOLA,'V V I',NBNOFI,INOLA)
    endif
    nbnofi=0
!      NBNOLA=0
!
!     RECUPERATION DU VECTEUR DE REMPLISSAGE DES GROUP_MA
    if (opmail) then
        call jeexin(nivgrm, iret)
        if (iret .ne. 0) call jeveuo(nivgrm, 'E', jnivgr)
    endif
!
!     RECUPERATION DU VECTEUR D'INDIRECTION ENTRE LES GROUP_MA
    if (opmail) then
        call jeexin(dirgrm, iret)
        if (iret .ne. 0) call jeveuo(dirgrm, 'L', jdirgr)
    endif
!
!     RECUPERATION DES INFOS DU MAILLAGE SAIN
    if (.not.opmail) then
        noma=zk8(jcnsk1-1+1)
        call jeveuo(noma//'.TYPMAIL        ', 'L', jtypma)
    endif
!
    call jeveuo(mo//'.XFEM_CONT', 'L', jxc)
!
!     LE RESULTAT EN ENTREE DE POST_CHAM_XFEM EST-IL MECANIQUE
    if (.not.opmail) lmeca = xismec()
!
!     ------------------------------------------------------------------
!                   BOUCLE SUR LES MAILLES DE MAILX
!     ------------------------------------------------------------------
!
    do 100 i = 1, nbmax
!
        ima = zi(jmax-1+i)
!
!       COMPTEUR DES NOUVEAUX NOEUDS ET MAILLES AJOUTES (LOCAL)
        inn = 0
        inm = 0
!
!       RECUPERATION DU NOMBRE DE NOEUDS (2 METHODES)
        call jelira(jexnum(malini//'.CONNEX', ima), 'LONMAX', n, k8b)
        nbnoma=zi(jconx2+ima) - zi(jconx2+ima-1)
        call assert(n.eq.nbnoma)
!
!       RECUPERATION DU NOMBRE TOTAL DE SOUS ELEMENTS
!       CORRESPOD AU NOMBRE DE MAILLES À CREER
        call cesexi('C', jcesd(3), jcesl(3), ima, 1,&
                    1, 1, iad3)
        call assert(iad3.gt.0)
        nse=zi(jcesv(3)-1+iad3)
!
!       RECUPERATION DU NOMBRE DE POINTS D'INTERSECTION
!        NPI=ZI(JCESV(3)-1+IAD3+NIT+1)
!
!       RECUPERATION DU NOMBRE DE POINTS MILIEUX
!        NMI=ZI(JCESV(3)-1+IAD3+NIT+3)
!
!       RECUPERATION DU NOMBRE DE NOUVEAUX NOEUDS A CREER
        nnn=zi(jcesv(3)-1+iad3+2)
        if (nnn .eq. 0) goto 100
!        CALL ASSERT(NNN.NE.0)
!
!       NOMBRE DE FISSURES "VUES" DANS LA MAILLE
        nfiss = zi(jcesd(6)-1+5+4*(ima-1)+2)
!       NOMBRE DE COMPOSANTE DE LA TOPOSE.HEA
        ncomp = zi(jcesd(4)-1+5+4*(ima-1)+3)
!
!       CREATION DU VECTEUR D'INDIRECTION DES NOEUDS (LOCAL A IMA)
        dirno='&&XPOMAX.DIRNO'
        call wkvect(dirno, 'V V I', nnn*(2+nfiss), jdirno)
!        CALL WKVECT(DIRNO,'V V I',(N+NPI+NMI)*3,JDIRNO)
!
!       DIMENSION TOPOLOGIQUE DE LA MAILLE
        ndime= zi(jtmdim-1+zi(jtypm1-1+ima))
!
!       1ER ELEMENT DE REFERENCE ASSOCIE A LA MAILLE
        itypel = zi(jmail-1+ima)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), notype)
        call elref2(notype, 10, lirefe, nbelr)
        elrefp= lirefe(1)
!       NOMBRE DE POINT DE GAUSS
        if (ndim .eq. 2) then
            if (nfiss .eq. 1) then
                npg = 12
            else
                npg = 4
            endif
        else if (ndim.eq.3) then
            if (nfiss .eq. 1) then
                npg = 15
            else
                npg = 5
            endif
        endif
!
!
!       CREATION DE VECTEUR DES COORDONNÉES DE LA MAILLE IMA
!       AVEC DES VALEURS CONTIGUES
        geom = '&&XPOAJD.GEOM'
        call wkvect(geom, 'V V R', ndim*n, igeom)
        do 20 in = 1, n
            ino=zi(jconx1-1+zi(jconx2+ima-1)+in-1)
            do 21 j = 1, ndim
                zr(igeom-1+ndim*(in-1)+j)=zr(iacoo1-1+3*(ino-1)+j)
21          continue
20      continue
!
        if (.not.opmail) then
!         TYPE DE LA MAILLE PARENT
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypma-1+ima)), typma)
!         CONNECTIVITES DU MAILLAGE QUADRATIQUE
!         POUR RECUPERER LES LAGRANGES
            call jeveuo(noma//'.CONNEX', 'L', jconq1)
            call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconq2)
        endif
!
!       COMPOSANTES DU CHAMP DE DEPLACEMENT 1 POUR LA MAILLE IMA
!       ET RECUPERATION DES CONTRAINTES 1
        if (.not.opmail) then
            nbcmp = zi(jcnsd1-1+2)
            call xpocmp(elrefp, cns1, ima, n, jconx1,&
                        jconx2, ndim, nfh, nfe, ddlc,&
                        nbcmp, cmp, lmeca)
!
            if (tysd(1:9) .ne. 'MODE_MECA' .and. tysd(1:9) .ne. 'EVOL_THER') then
                call cesexi('C', jcesd1, jcesl1, ima, 1,&
                            1, 1, iadc)
                if (iviex .ne. 0) call cesexi('C', jcvid1, jcvil1, ima, 1,&
                                              1, 1, iadv)
            endif
        endif
!
!       RECUPERATION DES COORDONNEES DES POINTS D'INTERSECTION
        call cesexi('C', jcesd(1), jcesl(1), ima, 1,&
                    1, 1, iad1)
        call assert(iad1.gt.0)
!
!       RECUPERATION DES COORDONNEES DES POINTS MILIEUX
        if (.not.iselli(elrefp) .and. ndim .le. 2) then
            call cesexi('C', jcesd(9), jcesl(9), ima, 1,&
                        1, 1, iad9)
            call assert(iad9.gt.0)
        else
            iad9=0
        endif
!
!       RECUPERATION DU NOMBRE DE POINTS D'INTERSECTION
!       SUR LES MAILLES PORTEUSES DE DDL DE CONTACT
!
        call cesexi('C', jcesd(10), jcesl(10), ima, 1,&
                    1, 1, iad10)
!        IF (IAD10.GT.0) THEN
        ninter=zi(jcesv(10)-1+iad10)
        call cesexi('C', jcesd(11), jcesl(11), ima, 1,&
                    1, 1, iad11)
!        ELSE
!          NINTER =0
!        ENDIF
!
!       RECUPERATION DE LA CONNECTIVITE DES SOUS-ELEMENTS
        call cesexi('C', jcesd(2), jcesl(2), ima, 1,&
                    1, 1, iad2)
        call assert(iad2.gt.0)
!
!       RECUPERATION DE LA FONCTION HEAVISIDE
        call cesexi('C', jcesd(4), jcesl(4), ima, 1,&
                    1, 1, iad4)
        call assert(iad4.gt.0)
!
!       RECUPERATION DES INFOS CONCERNANT LES GROUP_MA CONTENANT IMA
        if (opmail) then
            call jelira(jexnum(listgr, ima), 'LONMAX', ngrm, k8b)
            if (ngrm .gt. 0) call jeveuo(jexnum(listgr, ima), 'L', iagma)
        endif
!
!       RECUPERATION DES LEVELS SET AUX NOEUDS
        lst = '&&XPOAJD.LST'
        lsn = '&&XPOAJD.LSN'
        hea = '&&XPOAJD.HEA'
        call wkvect(lst, 'V V R', n*nfiss, jlst)
        call wkvect(lsn, 'V V R', n*nfiss, jlsn)
        call wkvect(hea, 'V V I', nfiss, jhea)
        do 30 ifiss = 1, nfiss
            do 40 j = 1, n
                call cesexi('C', jcesd(6), jcesl(6), ima, j,&
                            ifiss, 1, iad)
                call assert(iad.gt.0)
                zr(jlsn-1+(j-1)*nfiss+ifiss) = zr(jcesv(6)-1+iad)
                call cesexi('C', jcesd(7), jcesl(7), ima, j,&
                            ifiss, 1, iad)
                call assert(iad.gt.0)
                zr(jlst-1+(j-1)*nfiss+ifiss) = zr(jcesv(7)-1+iad)
40          continue
30      continue
!
!       RECUPERATION DE LA CONNECTIVITÉ DES FISSURES
        if (.not.opmail .and. nfh .gt. 0) then
!         CORRECTION DE NFH SI ON SE TROMPE DANS XPOCMP
            if (nfh .gt. nfiss) nfh = nfiss
            fisno = '&&XPOAJD.FISNO'
            call wkvect(fisno, 'V V I', n*nfh, jfisno)
            do 60 j = 1, n
                if (nfiss .eq. 1) then
                    zi(jfisno-1+j) = 1
                else if (nfiss.gt.1) then
                    nfh = zi(jcesd(8)-1+5+4*(ima-1)+2)
                    do 50 ifh = 1, nfh
                        call cesexi('C', jcesd(8), jcesl(8), ima, j,&
                                    ifh, 1, iad)
                        call assert(iad.gt.0)
                        zi(jfisno-1+(j-1)*nfh+ifh) = zi(jcesv(8)-1+ iad)
50                  continue
                endif
60          continue
        endif
!
! ----- ON AJOUTE LES NOUVELLES MAILLES ET LES NOUVEAUX NOEUDS
!
        if (.not.iselli(elrefp) .and. ndim .le. 2) then
            irese = 3
        else
            irese = 0
        endif
        nnose = tabse(ndime+irese)
!
!         BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
        do 140 ise = 1, nse
            do 150 ifiss = 1, nfiss
                zi(jhea-1+ifiss) = zi(jcesv(4)-1+iad4-1+ncomp*(ifiss- 1)+ise)
150          continue
            jcnse = jcesv(2)-1+iad2
            call xpoajm(maxfem, jtypm2, itypse(ndime+irese), jcnse, ise,&
                        n, nnose, prefno, jdirno, nse,&
                        inm, inmtot, nbmac, zi(jhea), jnivgr,&
                        iagma, ngrm, jdirgr, opmail, nfiss,&
                        ndim, ndime, jconx1, jconx2, jconq1,&
                        jconq2, ima, iad1+jcesv(1)-1, nnn, inn,&
                        inntot, nbnoc, nbnofi, inofi, iacoo1,&
                        iacoo2, iad9+jcesv(9)-1, ninter, jcesv( 11)+iad11-1, elrefp,&
                        jlsn, jlst, typma, igeom, jfisno,&
                        zi(jxc), cmp, nbcmp, nfh, nfe,&
                        ddlc, jcnsv1, jcnsv2, jcnsl2, lmeca)
            if (.not.opmail) then
                if (tysd(1:9) .ne. 'MODE_MECA' .and. tysd(1:9) .ne. 'EVOL_THER') then
!
!             ON AJOUTE DES CONTRAINTES
                    call xpoajc(nse, inm, inmtot, nbmac, ise,&
                                npg, jcesd1, jcesd2, jcvid1, jcvid2,&
                                ima, ndim, ndime, iadc, iadv,&
                                jcesv1, jcesl2, jcesv2, jcviv1, jcvil2,&
                                jcviv2)
!
                    call xpocox(nbmac, ima, inmtot, nbcmpc, jresd1,&
                                jresv1, jresl1, jresd2, jresv2, jresl2)
!
!
                endif
            endif
140      continue
!
        if (opmail) call assert(inn.eq.nnn)
!
        call jedetr(geom)
        call jedetr(dirno)
        call jedetr(lsn)
        call jedetr(lst)
        call jedetr(hea)
        if (.not.opmail .and. nfh .gt. 0) call jedetr(fisno)
!
100  end do
!
!     CREATION DU GROUPE DES NOEUDS SITUES SUR LA FISSURE
!     PORTANT DES DDLS DE CONTACT
    grpnoe=maxfem//'.GROUPENO'
    if (opmail .and. nbnofi .gt. 0) then
        nogno=nogrfi
!       ON SAIT QUE LE .GROUPENO N'EXISTE PAS
        gpptnn = maxfem//'.PTRNOMNOE'
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', 1+ngfon, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                    1+ngfon)
        call jecroc(jexnom(grpnoe, nogno))
        call jeecra(jexnom(grpnoe, nogno), 'LONMAX', max(1, nbnofi), k8b)
        call jeecra(jexnom(grpnoe, nogno), 'LONUTI', nbnofi, k8b)
        call jeveuo(jexnom(grpnoe, nogno), 'E', iagno)
        do 210 j = 1, nbnofi
            zi(iagno-1+j) = zi(inofi-1+j)
210      continue
    endif
!      IF (OPMAIL.AND.NBNOLA.GT.0) THEN
!        NOGNO=NOGRLA
!        CALL JECROC(JEXNOM(GRPNOE,NOGNO))
!        CALL JEECRA(JEXNOM(GRPNOE,NOGNO),'LONMAX',NBNOLA,K8B)
!        CALL JEVEUO(JEXNOM(GRPNOE,NOGNO),'E',IAGNOL)
!        DO 211 J = 1,NBNOLA
!          ZI(IAGNOL-1+J) = ZI(INOLA-1+J)
! 211    CONTINUE
!      ENDIF
    do 300 ich = 1, 4
        call detrsd('CHAM_ELEM_S', chs(ich))
300  end do
    do 310 ich = 6, 11
        call detrsd('CHAM_ELEM_S', chs(ich))
310  end do
!
    if (opmail) call jedetr(mailx)
    if (opmail) call jedetr(linofi)
!
999  continue
!
!      IF (.NOT.OPMAIL) CALL IMPRSD('CHAMP',COMPS2,6,'COMPS2')
!
    call jedema()
end subroutine
