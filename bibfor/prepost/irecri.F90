subroutine irecri(nomcon, form, ifi, titre, lgmsh,&
                  nbcham, cham, partie, nbpara, para,&
                  nbordr, ordr, lresu, motfac, iocc,&
                  cecr, tycha, lcor, nbnot, numnoe,&
                  nbmat, nummai, nbcmp, nomcmp, lsup,&
                  borsup, linf, borinf, lmax, lmin,&
                  formr, nive, versio)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/irch19.h'
    include 'asterfort/irgmsh.h'
    include 'asterfort/irpaca.h'
    include 'asterfort/irpara.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jerecu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxcaps.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnopa.h'
    include 'asterfort/rsutrg.h'
    include 'asterfort/titre2.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nomcon
    character(len=*) :: form, titre, cham(*), para(*)
    character(len=*) :: motfac, cecr
    character(len=*) :: nomcmp(*), formr, partie
    character(len=8) :: tycha
    real(kind=8) :: borsup, borinf
    integer :: nive, versio, nbcham, nbpara
    integer :: nbordr, ordr(*), nbcmp, iocc
    integer :: nbnot, numnoe(*), nbmat, nummai(*)
    logical :: lresu, lcor
    logical :: lsup, linf, lmax, lmin, lgmsh
!-----------------------------------------------------------------------
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
! TOLE CRP_21
!
!-----------------------------------------------------------------------
!     ECRITURE D'UN CONCEPT SUR FICHIER RESULTAT
!
! IN  NOMCON : K8  : NOM DU CONCEPT A IMPRIMER
! IN  FORM   : K8  : FORMAT D'ECRITURE
! IN  IFI    : IS  : UNITE LOGIQUE D'ECRITURE
! IN  TITRE  : K80 : TITRE POUR ALI_BABA ET SUPERTAB
! IN  NBCHAM : I   : NOMBRE DE CHAMP DANS LE TABLEAU CHAM
! IN  CHAM   : K16 : NOM DES CHAMPS A IMPRIMER ( EX 'DEPL', ....
! IN  PARTIE : K4  : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
! IN  NBPARA : I   : NOMBRE DE PARAMETRES LE TABLEAU PARA
! IN  PARA   : K16 : NOM DES PARAMETRES A IMPRIMER ( EX 'OMEGA2', ...
! IN  NBORDR : I   : NOMBRE DE NUMEROS D'ORDRE DANS LE TABLEAU ORDR
! IN  ORDR   : I   : LISTE DES NUMEROS D'ORDRE A IMPRIMER
! IN  LRESU  : L   : INDIQUE SI NOMCON EST UN CHAMP OU UN RESULTAT
! IN  MOTFAC : K   : NOM DU MOT CLE FACTEUR
! IN  IOCC   : I   : NUMERO D'OCCURENCE DU MOT CLE FACTEUR
! IN  MODELE : K   : NOM DU MODELE
! IN  CECR   : K1  : CODE D'ECRITURE DES PARAMETRES
!                    'T' TABLEAU 'L' LISTE
! IN  LCOR   : L   : INDIQUE SI IMPRESSION DES COORDONNEES DES NOEUDS
!                    .TRUE.  IMPRESSION
! IN  TYCHA  : K8  : TYPE DE CHAMP (SCALAIRE,VECT_2D,VECT_3D,TENS_2D,
!                    TENS_3D) POUR LE FORMAT GMSH (VERSION >= 1.2)
! IN  NBNOT  : I   : NOMBRE DE NOEUDS A IMPRIMER
! IN  NUMNOE : I   : NUMEROS DES NOEUDS A IMPRIMER
! IN  NBMAT  : I   : NOMBRE DE MAILLES A IMPRIMER
! IN  NUMMAI : I   : NUMEROS DES MAILLES A IMPRIMER
! IN  NBCMP  : I   : NOMBRE DE COMPOSANTES A IMPRIMER
! IN  NOMCMP : K8  : NOMS DES COMPOSANTES A IMPRIMER
! IN  LSUP   : L   : =.TRUE. INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
! IN  BORSUP : R   : VALEUR DE LA BORNE SUPERIEURE
! IN  LINF   : L   : =.TRUE. INDIQUE PRESENCE D'UNE BORNE INFERIEURE
! IN  BORINF : R   : VALEUR DE LA BORNE INFERIEURE
! IN  LMAX   : L   : =.TRUE. INDIQUE IMPRESSION VALEUR MAXIMALE
! IN  LMIN   : L   : =.TRUE. INDIQUE IMPRESSION VALEUR MINIMALE
! IN  FORMR  : K   : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
! IN  NIVE   : I   : NIVEAU IMPRESSION CASTEM 3 OU 10
! IN  VERSIO : I   : NIVEAU VERSION GMSH 1 OU 2
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    character(len=1) :: k1bid
    character(len=4) :: tych
    character(len=6) :: chnumo
    character(len=8) :: nomco
    character(len=19) :: noch19, knacc
    character(len=24) :: nomst
    logical :: lordr
    integer :: nbchca, nbacc, nbcara
    integer :: nbrk16, nbk16, ierd, ibid
    integer :: i, ibib, icha, ifi, isy, itype
    integer :: iun, ideu
    integer :: iord, iordr, ivsi
    integer :: iret
    integer :: jcham, jlast, jpara, jtabl, jtitr, jtot
    integer :: nbobj, nbtitr
!     ------------------------------------------------------------------
!     --- IMPRESSION D'UN TABLEAU SYNTHETIQUE DES PARAMETRES-----
!         (UNIQUEMENT FORMAT 'RESULTAT')
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call irpara(nomcon, form, ifi, nbordr, ordr,&
                nbpara, para, cecr)
!
    nomst = '&&IRECRI.SOUS_TITRE.TITR'
    nomco = nomcon
!
!     --- RECHERCHE DES OBJETS COMPOSANT LA TABLE CASTEM
!         (UNIQUEMENT FORMAT 'CASTEM')
    if (form .eq. 'CASTEM' .and. lresu .and. nbordr .ne. 0) then
        call jeexin('&&IRECRI.TABLE.TOT', iret)
        if (iret .eq. 0) then
            call wkvect('&&IRECRI.TABLE.TOT', 'V V I', nbordr*4, jtot)
        endif
        nbchca = 0
        call wkvect('&&IRECRI.CHAM.CASTEM', 'V V K16', nbcham, jcham)
        do 50 icha = 1, nbcham
            do 51 iord = 1, nbordr
                call rsexch(' ', nomco, cham(icha), ordr(iord), noch19,&
                            iret)
                if (iret .eq. 0) then
                    call dismoi('C', 'TYPE_CHAMP', noch19, 'CHAMP', ibib,&
                                tych, ierd)
                    if (tych(1:4) .eq. 'NOEU' .or. tych(1:4) .eq. 'ELNO') then
                        nbchca = nbchca + 1
                        zk16(jcham-1+nbchca) = cham(icha)
                        goto 50
                    endif
                endif
51          continue
50      continue
        knacc = '&&IRECRI.NOM_ACCES '
        call rsnopa(nomco, 0, knacc, nbacc, ibid)
        call jeexin(knacc, iret)
        if (iret .gt. 0) call jeveuo(knacc, 'E', jpara)
        nbobj = nbchca + nbacc + 1
    endif
!
!
!     --------------------------
!     TRAITEMENT DU FORMAT GMSH
!     -------------------------
!
    if (form .eq. 'GMSH') then
!
        call irgmsh(nomcon, partie, ifi, nbcham, cham,&
                    lresu, nbordr, ordr, nbcmp, nomcmp,&
                    nbmat, nummai, versio, lgmsh, tycha)
!
!     -----------------------------
!     TRAITEMENT DES AUTRES FORMATS
!     -----------------------------
!
    else
!
!     *******************************************
!     --- BOUCLE SUR LA LISTE DES NUMEROS D'ORDRE
!     *******************************************
        nbrk16 = 0
!
        do 21 iordr = 1, nbordr
            call jemarq()
            call jerecu('V')
!       --- FORMAT 'CASTEM'
            if (form .eq. 'CASTEM' .and. lresu) then
                call irpaca(nomcon, ifi, nbordr, iordr, ordr,&
                            nbacc, zk16(jpara), nbchca, zk16(jcham), nbk16,&
                            nive)
                nbrk16 = nbrk16 + nbk16
            endif
!
!       --- SI VARIABLE DE TYPE RESULTAT = RESULTAT COMPOSE :
!           VERIFICATION CORRESPONDANCE ENTRE NUMERO D'ORDRE
!           UTILISATEUR ORDR(IORDR) ET NUMERO DE RANGEMENT IRET
! AU CAS OU ON NE PASSE PAS EN DESSOUS ON INITIALISE LORDR A FALSE
            lordr=.false.
            if (lresu) then
                call rsutrg(nomcon, ordr(iordr), iret, ibid)
                if (iret .eq. 0) then
!           - MESSAGE NUMERO D'ORDRE NON LICITE
                    call codent(ordr(iordr), 'G', chnumo)
                    call u2mesk('A', 'PREPOST2_46', 1, chnumo)
                    goto 22
                endif
                lordr=.true.
            endif
!
!       --- BOUCLE SUR LE NOMBRE DE CHAMPS A IMPRIMER
            if (nbcham .ne. 0) then
                do 20 isy = 1, nbcham
                    if (lresu) then
!           * RESULTAT COMPOSE
!             - VERIFICATION EXISTENCE DANS LA SD RESULTAT NOMCON
!               DU CHAMP CHAM(ISY) POUR LE NO. D'ORDRE ORDR(IORDR)
!               ET RECUPERATION DANS NOCH19 DU NOM SE LE CHAM_GD EXISTE
                        call rsexch(' ', nomcon, cham(isy), ordr(iordr), noch19,&
                                    iret)
                        if (iret .ne. 0) goto 20
                    else
!           * CHAM_GD
                        noch19 = nomcon
                    endif
!
!           * IMPRESSION DES PARAMETRES (FORMAT 'RESULTAT')
                    if (lordr .and. form .eq. 'RESULTAT') then
!             - SEPARATION DES DIVERS NUMEROS D'ORDRE PUIS IMPRESSION
                        write(ifi,'(/,1X,A)') '======>'
                        call irpara(nomcon, form, ifi, 1, ordr(iordr),&
                                    nbpara, para, cecr)
                        lordr=.false.
                    endif
!           * CREATION D'UN SOUS-TITRE
                    if (form .eq. 'RESULTAT' .or. form .eq. 'IDEAS') then
                        call titre2(nomcon, noch19, nomst, motfac, iocc,&
                                    formr)
                    endif
!
!           * IMPRESSION DU SOUS-TITRE SI FORMAT 'RESULTAT'
                    if (form .eq. 'RESULTAT') then
!              ---- SEPARATION DES DIVERS CHAMPS -----
                        write(ifi,'(/,1X,A)') '------>'
                        call jeveuo(nomst, 'L', jtitr)
                        call jelira(nomst, 'LONMAX', nbtitr, k1bid)
                        write(ifi,'(1X,A)') (zk80(jtitr+i-1),i=1,&
                        nbtitr)
                    endif
!
!           ********************************************************
!           * IMPRESSION DU CHAMP (CHAM_NO OU CHAM_ELEM) AU FORMAT
!             'RESULTAT' OU 'SUPERTAB'
!                LE CHAMP EST UN CHAM_GD SIMPLE SI LRESU=.FALSE. OU
!                LE CHAMP EST LE CHAM_GD CHAM(ISY) DE NUMERO D'ORDRE
!                ORDR(IORDR) ISSU DE LA SD_RESULTAT NOMCON
                    call irch19(noch19, partie, form, ifi, titre,&
                                nomcon, cham(isy), ordr(iordr), lcor, nbnot,&
                                numnoe, nbmat, nummai, nbcmp, nomcmp,&
                                lsup, borsup, linf, borinf, lmax,&
                                lmin, lresu, formr, nive)
20              continue
            endif
!
!       --- IMPRESSION  DE LA TABLE SI FORMAT 'CASTEM'
            if (form .eq. 'CASTEM' .and. lresu .and. nbordr .ne. 0) then
                call jeveuo('&&IRPACA.TABL.CASTEM', 'L', jtabl)
                call jeveuo('&&OP0039.LAST', 'E', jlast)
                nbcara = 4*nbobj
                itype = 10
                ivsi = 26
                zi(jtot-1+(iordr-1)*4+1)= ivsi
                zi(jtot-1+(iordr-1)*4+2)= zi(jlast-1+7)+iordr*2-1
                zi(jtot-1+(iordr-1)*4+3)= itype
                zi(jtot-1+(iordr-1)*4+4)= zi(jlast-1+6)*2+1
                iun = 1
                ideu = 2
                itype = 10
                write (ifi,'(A,I4)') ' ENREGISTREMENT DE TYPE',ideu
                if (nive .eq. 3) then
                    write (ifi,'(A,I4,A,I4,A,I4)') ' PILE NUMERO',&
                    itype, 'NBRE OBJETS NOMMES ',iun,'NBRE OBJETS ',&
                    ideu
                else if (nive.eq.10) then
                    write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',&
                    itype, 'NBRE OBJETS NOMMES',iun,'NBRE OBJETS',&
                    ideu
                endif
                call lxcaps(nomco)
                write(ifi,'(1X,A8)') nomco
                if (nive .eq. 3) then
                    write(ifi,'(I5)') zi(jlast-1+6)*2+2
                    write(ifi,'(I5)') nbcara
                    write(ifi,'(16I5)') (zi(jtabl-1+i),i=1,nbobj*4)
                    write(ifi,'(I5)') 4*iordr
                    write(ifi,'(16I5)') (zi(jtot-1+i),i=1,iordr*4)
                else if (nive.eq.10) then
                    write(ifi,'(I8)') zi(jlast-1+6)*2+2
                    write(ifi,'(I8)') nbcara
                    write(ifi,'(10I8)') (zi(jtabl-1+i),i=1,nbobj*4)
                    write(ifi,'(I8)') 4*iordr
                    write(ifi,'(10I8)') (zi(jtot-1+i),i=1,iordr*4)
                endif
                zi(jlast-1+6) = zi(jlast-1+6) + 1
                call jedetr('&&IRPACA.TABL.CASTEM')
            endif
22          continue
            call jedema()
21      end do
!
        if (lresu .and. form .eq. 'CASTEM' .and. nbordr .ne. 0) then
            zi(jlast-1+7) = zi(jlast-1+1)
            zi(jlast-1+8) = zi(jlast-1+8) + nbobj + nbrk16
            zi(jlast-1+3) = zi(jlast-1+3) + nbobj
        endif
!
    endif
!
!     --- DESTRUCTION OBJETS DE TRAVAIL
    call jedetr('&&IRECRI.CHPRES')
    call jedetr('&&IRECRI.FVIDAV')
    call jedetr('&&IRECRI.FVIDAP')
    call jedetr('&&IRECRI.NOM_ACC')
    call jedetr('&&IRPACA.TABL.CASTEM')
    call jedetr('&&IRECRI.CHAM.CASTEM')
    call jedetr('&&IRECRI.TABLE.TOT')
    call jeexin(nomst, iret)
    if (iret .ne. 0) call jedetr(nomst)
!
    call jedema()
end subroutine
