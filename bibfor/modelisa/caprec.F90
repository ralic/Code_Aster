subroutine caprec(charge, mailla)
    implicit none
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
! TOLE CRP_20
! ----------------------------------------------------------------------
!  DESCRIPTION : OPERATEUR AFFE_CHAR_MECA
!  -----------   TRAITEMENT DU MOT-CLE FACTEUR RELA_CINE_BP
!                APPELANT : CHARME
!
!  IN     : CHARGE : CHARACTER*8  SCALAIRE
!                    NOM DU CONCEPT CHAR_MECA
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/indik8.h'
    include 'asterc/r8gaem.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/drz02d.h'
    include 'asterfort/drz03d.h'
    include 'asterfort/drz12d.h'
    include 'asterfort/drz13d.h'
    include 'asterfort/etenca.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/nocart.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/tbexve.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!
! ARGUMENTS
! ---------
    character(len=8) :: charge, mailla
!
! VARIABLES LOCALES
! -----------------
    integer :: nmocl
    integer :: vali(2)
    parameter (nmocl=300)
    integer :: ias, ias1, iasm, iasm1, ibid, icmp, icode, icode1, icodok, iocc
    integer :: irann, iret, jdesc, jdesc1, jdnbre, jncmp1, jptma, jptma1, jsief
    integer :: jvale, jvale1, jvalv1, nbec, nbmama, nbocc, nbrela, nsief, numail
    logical :: lcart1, lrela, lsigm, lpara
    character(len=1) :: k1b
    character(len=2) :: typlag
    character(len=3) :: k3b
    character(len=8) :: cablpr, k8b, k8vide, modele, npara
    character(len=19) :: ligrmo, lirela, sigcab, sigcha, lisnom, lisan1, lisan2
    character(len=24) :: grnoma, lisnoe, ligrno, noeuma, nomanc, noman1, noman2
    character(len=24) :: k24vid
    integer :: nddla
    integer :: idimax, jlist, ino, jgro, in, indnoe, lonlis
    integer :: jind, in1, indlis, jprnm, nanc, jadd
    real(kind=8) :: dmin, armin
    complex(kind=8) :: c16b
    character(len=1) :: k1bid
    character(len=8) :: nomnoe, nomg
    character(len=8) :: cmp, nomcmp(nmocl)
    character(len=8) :: cmp4, cmp5, cmp6, k8bid
    character(len=9) :: nomte
    integer :: ntypel(nmocl)
    integer :: icmp4, icmp5, icmp6, idrxyz, i, idrz, ndimmo
    integer :: ilisno, ier, inom, nbcmp, ierd
    integer :: nbnom, nban1, nban2, jlsnom, jlsan1, jlsan2, inomc
    integer :: numcab, numca0, n1
    character(len=8) :: effnor
    character(len=19) :: lrltmp, lisrel
    character(len=24) :: table
    integer :: iarg
!
    data effnor/'N       '/
    data lrltmp/'&&CAPREC.LIRELA    '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    k8vide = ' '
    k24vid = ' '
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     DECOMPTE DES OCCURRENCES DU MOT-CLE FACTEUR 'RELA_CINE_BP'
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call getfac('RELA_CINE_BP', nbocc)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     TRAITEMENT SI AU MOINS UNE OCCURRENCE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (nbocc .gt. 0) then
!
! ---  RECUPERATION DES INFORMATIONS UTILES - INITIALISATIONS
        call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                    modele, iret)
        ligrmo = modele//'.MODELE    '
!
        call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbmama,&
                    k8b, iret)
!
!     RECUPERATION DE L'ARETE MIN : ARMIN
        call ltnotb(mailla, 'CARA_GEOM', table)
        call tbliva(table, 1, 'APPLAT_Z', ibid, 0.d0,&
                    c16b, k1bid, 'ABSO', r8gaem(), 'AR_MIN',&
                    k1bid, ibid, armin, c16b, k1bid,&
                    ier)
        call assert(armin.gt.0.d0)
!
!
! --- NOM DE LA LISTE DE RELATIONS
        lisrel = '&&CAPREC.RLLISTE'
!
! --- DIMENSION ASSOCIEE AU MODELE
        call dismoi('F', 'DIM_GEOM', modele, 'MODELE', ndimmo,&
                    k8b, ier)
        if (.not.(ndimmo.eq.2.or.ndimmo.eq.3)) call u2mess('F', 'MODELISA2_6')
!
! --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
! --- D'ELEMENTS DE LAGRANGE ASSOCIES
!
        nomg = 'DEPL_R'
        nomte = 'D_DEPL_R_'
!
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
        call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
        nddla = nbcmp - 1
        if (nddla .gt. nmocl) then
            vali (1) = nmocl
            vali (2) = nddla
            call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
        do 10 i = 1, nddla
            nomcmp(i) = zk8(inom-1+i)
            call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i) (1:7) ), ntypel(i))
10      continue
        call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                    k8bid, ierd)
!
! --- ACCES A L'OBJET .PRNM
!
        if (nbec .gt. 10) then
            call u2mess('F', 'MODELISA_94')
        else
            call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
        endif
!
!
!....... DETERMINATION DU RANG DE LA COMPOSANTE <N>
!....... DE LA GRANDEUR <SIEF_R>
!
        call jelira(jexnom('&CATA.GD.NOMCMP', 'SIEF_R'), 'LONMAX', nsief, k1b)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', 'SIEF_R'), 'L', jsief)
        irann = 0
        do 20 icmp = 1, nsief
            if (zk8(jsief+icmp-1) .eq. effnor) then
                irann = icmp
                goto 30
            endif
20      continue
30      continue
        if (irann .eq. 0) call u2mess('F', 'MODELISA3_35')
!
        call assert(irann.le.30)
        icodok = 2**irann
!
        lcart1 = .true.
        sigcha = charge//'.CHME.SIGIN'
        call dismoi('F', 'NB_EC', 'SIEF_R', 'GRANDEUR', nbec,&
                    k8b, iret)
!
!        BOUCLE SUR LE NOMBRE D'OCCURRENCES DE RELA_CINE_BP
! ---
        do 160 iocc = 1, nbocc
            call getvr8('RELA_CINE_BP', 'DIST_MIN', iocc, iarg, 0,&
                        dmin, n1)
            if (n1 .eq. 0) dmin=armin*1.d-3
!
! ---  LECTURE DES ARGUMENTS DES MOT-CLES 'SIGM_BPEL' ET
! ---  'RELA_CINE' POUR DEFINITION DES DONNEES A RECUPERER
!
            call getvtx('RELA_CINE_BP', 'SIGM_BPEL', iocc, iarg, 1,&
                        k3b, ibid)
            lsigm = (k3b.eq.'OUI')
            call getvtx('RELA_CINE_BP', 'RELA_CINE', iocc, iarg, 1,&
                        k3b, ibid)
            lrela = (k3b.eq.'OUI')
!
! ---  RECUPERATION DES DONNEES LE CAS ECHEANT
            if (lsigm .or. lrela) then
!
! ---  RECUPERATION DU NOM DU CONCEPT DE TYPE CABL_PRECONT
!
                call getvid('RELA_CINE_BP', 'CABLE_BP', iocc, iarg, 1,&
                            cablpr, ibid)
!
! ---  RECUPERATION D'UNE CARTE DE CONTRAINTES INITIALES LE CAS ECHEANT
!
                if (lsigm) then
                    call dismoi('F', 'NB_EC', 'SIEF_R', 'GRANDEUR', nbec,&
                                k8b, iret)
                    sigcab = cablpr//'.CHME.SIGIN'
                    call jeexin(sigcab//'.DESC', iret)
                    if (iret .eq. 0) call u2mesk('F', 'MODELISA3_36', 1, cablpr)
!
! ---  RECOPIAGE DE LA PREMIERE CARTE
!
                    if (lcart1) then
!
                        call copisd('CHAMP_GD', 'G', sigcab, sigcha)
                        lcart1 = .false.
                        call etenca(sigcha, ligrmo, iret)
                        if (iret .ne. 0) call u2mesk('F', 'MODELISA3_37', 1, sigcha)
                        call jeveuo(sigcha//'.DESC', 'L', jdesc1)
                        call jeveuo(sigcha//'.VALE', 'L', jvale1)
                        call jeveuo(sigcha//'.PTMA', 'L', jptma1)
                        call jecreo(sigcha//'.NCMP', 'V V K8')
                        call jeecra(sigcha//'.NCMP', 'LONMAX', nsief, ' ')
                        call jeveuo(sigcha//'.NCMP', 'E', jncmp1)
                        do 40 icmp = 1, nsief
!                  ZK8(JNCMP1+ICMP-1) = '        '
                            zk8(jncmp1+icmp-1) = k8vide
40                      continue
                        zk8(jncmp1) = effnor
                        call jecreo(sigcha//'.VALV', 'V V R')
                        call jeecra(sigcha//'.VALV', 'LONMAX', nsief, ' ')
                        call jeveuo(sigcha//'.VALV', 'E', jvalv1)
!
! ---  RECUPERATION DES DONNEES APPORTEES PAR LES CARTES SUIVANTES
!
                    else
!
                        call etenca(sigcab, ligrmo, iret)
                        if (iret .ne. 0) call u2mesk('F', 'MODELISA3_37', 1, sigcab)
                        call jeveuo(sigcab//'.DESC', 'L', jdesc)
                        call jeveuo(sigcab//'.VALE', 'L', jvale)
                        call jeveuo(sigcab//'.PTMA', 'L', jptma)
                        iasm1 = zi(jdesc1+1)
                        iasm = zi(jdesc +1)
                        do 50 numail = 1, nbmama
                            ias1 = zi(jptma1+numail-1)
                            ias = zi(jptma +numail-1)
                            icode1 = zi(jdesc1+3+2*iasm1+nbec* (ias1- 1))
                            icode = zi(jdesc +3+2*iasm +nbec* (ias -1) )
                            if (icode .eq. icodok) then
                                zr(jvalv1) = zr(jvale+nsief* (ias-1))
                                if (icode1 .eq. icodok) zr(jvalv1) = zr( jvalv1) + zr(jvale1+nsie&
                                                                     &f* (ias1-1))
                                call nocart(sigcha, 3, k1b, 'NUM', 1,&
                                            k1b, numail, ' ', 1)
! ---  NOCART PEUT AGRANDIR LA CARTE IL FAUT DONC ACTUALISER LES ADR.
                                call jeveuo(sigcha//'.DESC', 'L', jdesc1)
                                call jeveuo(sigcha//'.VALE', 'L', jvale1)
                            endif
50                      continue
                        call jelibe(sigcab//'.DESC')
                        call jelibe(sigcab//'.VALE')
                        call jelibe(sigcab//'.PTMA')
                        call jedetc('V', sigcab, 1)
!
                    endif
!
                endif
!
! ---  RECUPERATION D'UNE LISTE DE RELATIONS CINEMATIQUES
! ---  LE CAS ECHEANT
!
                if (lrela) then
                    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                                k8b, iret)
                    lirela = cablpr//'.LIRELA    '
                    call jeexin(lirela//'.RLNR', iret)
                    if (iret .eq. 0) call u2mesk('F', 'MODELISA3_36', 1, cablpr)
                    call jeveuo(lirela//'.RLNR', 'L', jdnbre)
                    nbrela = zi(jdnbre)
                    call jelibe(lirela//'.RLNR')
                    if (nbrela .gt. 0) then
                        call copisd(' ', 'V', lirela, lrltmp)
                        call aflrch(lrltmp, charge)
                    endif
!
!
! ---  ACQUISITION DE LA LISTE DES NOEUDS A LIER PAR UN EQUIVALENT
! ---  A LIAISON_SOLIDE (CETTE LISTE EST NON REDONDANTE)
!
                    call jeveuo(cablpr//'           .LTNS', 'L', jadd)
                    table = zk24(jadd)
!
                    lisnom = '&&CAPREC.NUME_CABLE'
                    lisan1 = '&&CAPREC.NOM_ANCRA1'
                    lisan2 = '&&CAPREC.NOM_ANCRA2'
!
                    call tbexip(table, 'NUME_CABLE', lpara, npara)
                    call tbexip(table, 'NOM_ANCRAGE1', lpara, npara)
!
                    call tbexve(table, 'NUME_CABLE', lisnom, 'V', nbnom,&
                                k8b)
                    call jeveuo(lisnom, 'L', jlsnom)
!
                    call tbexve(table, 'NOM_ANCRAGE1', lisan1, 'V', nban1,&
                                k8b)
                    call jeveuo(lisan1, 'L', jlsan1)
!
                    call tbexve(table, 'NOM_ANCRAGE2', lisan2, 'V', nban2,&
                                k8b)
                    call jeveuo(lisan2, 'L', jlsan2)
!
                    numca0 = 0
!
!         NOMANC = ''
                    nomanc = k24vid
!
                    do 145 inomc = 1, nbnom
                        numcab = zi(jlsnom-1+inomc)
                        noman1 = zk24(jlsan1-1+inomc)
                        noman2 = zk24(jlsan2-1+inomc)
!
                        if (numcab .ne. numca0) then
                            numca0 = numcab
!
                            do 150 nanc = 1, 2
!               NOMANC = '        '
                                nomanc = k24vid
!
                                if (nanc .eq. 1) then
                                    nomanc = noman1
                                endif
!
                                if (nanc .eq. 2) then
                                    nomanc = noman2
                                endif
!
!                IF (NOMANC.NE.'        ') THEN
                                if (nomanc .ne. k24vid) then
!
                                    lisnoe = '&&CAPREC.LISTNOE'
                                    typlag = '12'
                                    ligrno = cablpr//'           '//'.LTNT'
                                    call jeexin(ligrno, iret)
!
                                    grnoma = mailla//'.GROUPENO'
                                    noeuma = mailla//'.NOMNOE'
!
                                    call jeveuo(jexnom(grnoma, nomanc), 'L', jgro)
                                    call jelira(jexnom(grnoma, nomanc), 'LONUTI', idimax, k1b)
                                    call wkvect(lisnoe, 'V V K8', idimax, jlist)
!
                                    indnoe = 0
                                    do 60 ino = 1, idimax
                                        in = zi(jgro+ino-1)
                                        indnoe = indnoe + 1
                                        call jenuno(jexnum(noeuma, in), nomnoe)
!
                                        zk8(jlist+indnoe-1) = nomnoe
60                                  continue
!
! ---  ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS DE LA LISTE
                                    call wkvect('&&CAPREC.INDICE', 'V V I', idimax, jind)
!
                                    do 80 ino = 1, idimax
                                        do 70 in1 = ino + 1, idimax
                                            if (zk8(jlist+in1-1) .eq. zk8( jlist+ino-1)) then
                                                zi(jind+in1-1) = 1
                                            endif
70                                      continue
80                                  continue
!
                                    indlis = 0
                                    do 90 ino = 1, idimax
                                        if (zi(jind+ino-1) .eq. 0) then
                                            indlis = indlis + 1
                                            zk8(jlist+indlis-1) = zk8( jlist+ino-1)
                                        endif
90                                  continue
!
                                    lonlis = indlis
!
                                    call jeveuo(lisnoe, 'L', ilisno)
!
! ---  CAS OU LA LISTE DES NOEUDS A LIER EST UN SINGLETON
!
                                    if (lonlis .eq. 1) then
                                        call u2mess('I', 'MODELISA3_17')
                                        goto 140
                                    endif
!
! ---  CAS OU LA DIMENSION DU MODELE EST EGALE A 2
!
                                    if (ndimmo .eq. 2) then
!
! ---  ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ
!
                                        cmp = 'DRZ'
                                        icmp = indik8(nomcmp,cmp,1, nddla)
                                        idrz = 0
                                        do 100 i = 1, lonlis
! ---  NUMERO DU NOEUD COURANT DE LA LISTE
                                            call jenonu(jexnom(&
                                                        mailla// '.NOMNOE', zk8(ilisno+i-1)),&
                                                        in)
!
                                            if (exisdg(zi( jprnm-1+ (in-1)* nbec+1), icmp)) then
                                                idrz = 1
                                                goto 110
                                            endif
100                                      continue
!
110                                      continue
!
! ---  CAS OU L'ON A UN NOEUD DE LA LISTE PORTANT LE DDL DRZ
!
                                        if (idrz .eq. 1) then
                                            call drz12d(lisnoe, lonlis, charge, typlag, lisrel)
!
! ---  CAS OU AUCUN NOEUD DE LA LISTE NE PORTE LE DDL DRZ
!
                                        else if (idrz.eq.0) then
                                            call drz02d(lisnoe, lonlis, charge, typlag, lisrel,&
                                                        dmin)
!
! ---  FIN DU CAS 2D SANS DDL DE ROTATION
                                        endif
!
! ---  CAS OU LA DIMENSION DU MODELE EST EGALE A 3
!
                                    else if (ndimmo.eq.3) then
!
! ---  ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
! ---  DE ROTATION
!
                                        cmp4 = 'DRX'
                                        cmp5 = 'DRY'
                                        cmp6 = 'DRZ'
                                        icmp4 = indik8(nomcmp,cmp4,1, nddla)
                                        icmp5 = indik8(nomcmp,cmp5,1, nddla)
                                        icmp6 = indik8(nomcmp,cmp6,1, nddla)
                                        idrxyz = 0
                                        do 120 i = 1, lonlis
! ---  NUMERO DU NOEUD COURANT DE LA LISTE
                                            call jenonu(jexnom(&
                                                        mailla// '.NOMNOE', zk8(ilisno+i-1)),&
                                                        in)
!
                                            if ((&
                                                exisdg(zi( jprnm-1+ (in-1) *nbec+1), icmp4)&
                                                )&
                                                .and.&
                                                (&
                                                exisdg(zi( jprnm-1+ (in-1)* nbec+ 1), icmp5&
                                                )&
                                                )&
                                                .and.&
                                                (&
                                                exisdg(zi( jprnm-1+ (in- 1)*nbec+1), icmp6)&
                                                )) then
                                                idrxyz = 1
                                                goto 130
                                            endif
120                                      continue
!
130                                      continue
!
! ---  CAS OU L'ON A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
! ---  DE ROTATION
                                        if (idrxyz .eq. 1) then
                                            call drz13d(lisnoe, lonlis, charge, typlag, lisrel)
!
! ---  CAS MASSIF (PAS DE COMPOSANTES DE ROTATION)
                                        else if (idrxyz.eq.0) then
                                            call drz03d(lisnoe, lonlis, charge, typlag, lisrel,&
                                                        dmin)
!
! ---  FIN DU CAS 3D MASSIF (IDRXYZ=0)
                                        endif
! ---  FIN DU CAS 3D
                                    endif
140                                  continue
! ---  DESTRUCTION DE LA LISTE DES NOEUDS A LIER
                                    call jedetr(lisnoe)
                                    call jedetr('&&CAPREC.INDICE')
!
! ---  AFFECTATION DE LA LISTE_RELA A LA CHARGE :
                                    call aflrch(lisrel, charge)
!
! ---  FIN IF NOMANC = NOM_ANCRAGE1 OU NOM_ANCRAGE2
                                endif
!
! ---  FIN BCL SUR NUMERO ANCRAGE (NANC = 1 OU 2)
150                          continue
!
!
! ---  FIN BCL SUR NUM_CABLE DANS LA TABLE
                        endif
145                  continue
!
                    call jedetr(lisnom)
                    call jedetr(lisan1)
                    call jedetr(lisan2)
!
! ---  FIN IF : LRELA
                endif
!
! ---  FIN IF : LSIGM .OR. LRELA
            endif
!
! ---  FIN BOUCLE SUR LE NOMBRE D'OCCURENCES DE RELA_CINE_BP
160      continue
!
!
! ---  DESTRUCTION DES OBJETS DE TRAVAIL ATTACHES A LA CARTE
! ---  DES CONTRAINTES INITIALES
!
        if (.not.lcart1) call jedetc('V', sigcha, 1)
!
    endif
!
    call jedema()
!
! ---  FIN DE CAPREC.
end subroutine
