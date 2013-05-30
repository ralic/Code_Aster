subroutine promor(nuz, base)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infbav.h'
    include 'asterfort/infmue.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/juveca.h'
    include 'asterfort/moinip.h'
    include 'asterfort/moinsr.h'
    include 'asterfort/mpicm0.h'
    include 'asterfort/nbec.h'
    include 'asterfort/teattr.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/uttrii.h'
    include 'asterfort/voiuti.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nuz
    character(len=1) :: base
!     ------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
! TOLE CRP_20
!     CALCUL DE LA STRUCTURE COMPACTE D'UNE MATRICE
!     ------------------------------------------------------------------
! IN  K*14 NU     : NOM DE LA SD_UME_DDL A COMPLETER.
! OUT K*14 NU     : L'OBJET NU EST COMPLETE DES OBJETS .SMOS.XXXX
! IN  K1  BASE    : BASE DE CREATION DU STOCKAGE
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    character(len=8) :: ma, mo, kbid, exiele, exivf
!----------------------------------------------------------------------
    character(len=14) :: nu
    logical :: lfeti, ldist, ldgrel, lmadis
    character(len=19) :: nomlig
    integer :: imail, j, iconx1, iconx2, ili, igrel, iadlie, iel, iadnem
    integer :: zzconx, zznbne, zzliel, zzngel, zznsup, zznelg, nunoel, l, idprn1
    integer :: zznema, zzprno, idprn2, ifm, niv, iret, ibid, ier, nnoe, jnueq
    integer :: vali(3), neqx, iilib, igr, numa, k1, n1, iad1, nddl1
    integer :: iddl, jddl, iamail, jsmhc, ncoef, jsmde, igd, nbss
    integer :: iasssa, ierd, iadequ, nlili, nequ, iimax, jnoip, jsuiv, mxddlt
    integer :: ima, nddlt, jalm, jsmdi, nel, nec, nbsma, itypel, jnvge
    integer :: nnov, numav, kvois, rang, jnumsd, imd, jsmh1, jprtk
!
    character(len=8) :: partit
    real(kind=8) :: valr(2), rcoef, requ
    character(len=16) :: codvoi, nomte
    character(len=12) :: vge
    integer :: nvoima, nscoma, jrepe, jptvoi, jelvoi, nbvois
    parameter(nvoima=100,nscoma=4)
    integer :: livois(1:nvoima), tyvois(1:nvoima), nbnovo(1:nvoima)
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2), nbproc
!
!-----------------------------------------------------------------------
!     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
!     S.D. MANIPULEES DANS LE SOUS PROGRAMME
!-----------------------------------------------------------------------
!---- FONCTION D ACCES AU CHAMP CONNEX DE LA S.D. MAILLA DE TYPE
!     MAILLAGE
!     ZZCONX(IMAIL,J) = NUMERO DANS LA NUMEROTATION DU MAILLAGE
!         DU NOEUD J DE LA MAILLE IMAIL
    zzconx(imail,j)=zi(iconx1-1+zi(iconx2+imail-1)+j-1)
!
!---- NBRE DE NOEUDS DE LA MAILLE IMAIL DU MAILLAGE
!
    zznbne(imail)=zi(iconx2+imail)-zi(iconx2+imail-1)
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
!     ZZLIEL(ILI,IGREL,J) =
!      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
!          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
!          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA
!
    zzliel(ili,igrel,j)=zi(zi(iadlie+3*(ili-1)+1)-1+&
     &                    zi(zi(iadlie+3*(ili-1)+2)+igrel-1)+j-1)
!
!---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI
!
    zzngel(ili)=zi(iadlie+3*(ili-1))
!
!---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
!     DU LIGREL ILI REPERTOIRE .LILI
!     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )
!
    zznsup(ili,iel)=zi(zi(iadnem+3*(ili-1)+2)+iel)-&
     &                zi(zi(iadnem+3*(ili-1)+2)+iel-1)-1
!
!---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
!     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )
!
    zznelg(ili,igrel)=zi(zi(iadlie+3*(ili-1)+2)+igrel)-&
     &                  zi(zi(iadlie+3*(ili-1)+2)+igrel-1)-1
!
!---- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DU
!     REPERTOIRE TEMPORAIRE .MATAS.LILI
!
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
!
    zznema(ili,iel,j)=zi(zi(iadnem+3*(ili-1)+1)-1+&
     &                  zi(zi(iadnem+3*(ili-1)+2)+iel-1)+j-1)
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES S.D. LIGREL
!     REPERTORIEES DANS NU.LILI DE LA S.D. NUME_DDL ET A LEURS ADRESSES
!     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
!                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
!                            AU LIGREL ILI DE .LILI
!     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
!     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
!     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE
!
    zzprno(ili,nunoel,l)=zi(idprn1-1+zi(idprn2+ili-1)+&
     &                     (nunoel-1)*(nec+2)+l-1)
!----------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
    nu=nuz
!
!     -- FETI OR NOT FETI ?
    call jeexin('&FETI.MAILLE.NUMSD', iret)
    if (iret .ne. 0) then
        call infmue()
        call infniv(ifm, niv)
        lfeti=.true.
    else
        lfeti=.false.
    endif
!
!
    call dismoi('F', 'NOM_MODELE', nu, 'NUME_DDL', ibid,&
                mo, ier)
    call dismoi('F', 'NUM_GD_SI', nu, 'NUME_DDL', igd,&
                kbid, ier)
    call dismoi('F', 'NOM_MAILLA', nu, 'NUME_DDL', ibid,&
                ma, ier)
!
!---- QUEL TYPE DE PARTITION ?
!     LDIST=.TRUE.  : LES CALCULS ELEMENTAIRES SONT DISTRIBUES
!     LDGREL=.TRUE. : PARTITION DE TYPE 'GROUP_ELEM'
    call dismoi('F', 'NOM_LIGREL', mo, 'MODELE', ibid,&
                nomlig, ier)
    call dismoi('F', 'PARTITION', nomlig, 'LIGREL', ibid,&
                partit, ier)
    call mpicm0(rang, nbproc)
    ldist=.false.
    ldgrel=.false.
    if (partit .ne. ' ') then
        ldist=.true.
        call jeveuo(partit//'.PRTK', 'L', jprtk)
        ldgrel=zk24(jprtk-1+1).eq.'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif
!
    call jeexin(ma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', iconx1)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', iconx2)
    endif
!
    if (mo .eq. ' ') then
        nbss=0
    else
        call dismoi('F', 'NB_SS_ACTI', mo, 'MODELE', nbss,&
                    kbid, ierd)
        if (nbss .gt. 0) then
            call dismoi('F', 'NB_SM_MAILLA', mo, 'MODELE', nbsma,&
                        kbid, ierd)
            call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
        endif
    endif
!
!
    call jeveuo(nu//'     .ADNE', 'E', iadnem)
    call jeveuo(nu//'     .ADLI', 'E', iadlie)
!
!     -- CAS MATR_DISTRIBUE='OUI' => LMADIS=.TRUE.
    call jeexin(nu//'.NUML.DELG', imd)
    lmadis=(imd.ne.0)
    if (.not.lmadis) then
        call jeveuo(nu//'.NUME.NEQU', 'L', iadequ)
        call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili, kbid)
        call jeveuo(nu//'.NUME.PRNO', 'L', idprn1)
        call jeveuo(jexatr(nu//'.NUME.PRNO', 'LONCUM'), 'L', idprn2)
        call jeveuo(nu//'.NUME.NUEQ', 'L', jnueq)
    else
        call jeveuo(nu//'.NUML.NEQU', 'L', iadequ)
        call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili, kbid)
        call jeveuo(nu//'.NUML.PRNO', 'L', idprn1)
        call jeveuo(jexatr(nu//'.NUML.PRNO', 'LONCUM'), 'L', idprn2)
        call jeveuo(nu//'.NUML.NUEQ', 'L', jnueq)
    endif
!
    nec=nbec(igd)
    nequ=zi(iadequ)
!
!
!
!---- CREATION DE 2 TABLEAUX DE TRAVAIL :
!      .NOIP   :   TABLE DES NUMEROS DE LIGNE
!      .NOSUIV :   TABLE DES CHAINAGES DE LA STRUCTURE CHAINEE
!                  QUI EST CONTRUITE AVANT D'OBTENIR LA
!                  STRUCTURE COMPACTE (SMDI,SMHC) DE LA MATRICE
!     CES 2 OBJETS SONT AGRANDIS SI NECESSAIRE DANS MOINSR.F
!     ON COMMENCE AVEC IIMAX=10
    iimax=10
    call wkvect('&&PROMOR.NOIP', 'V V I', iimax, jnoip)
    call wkvect('&&PROMOR.NOSUIV', 'V V I', iimax, jsuiv)
!
!
!
!     -- ALLOCATION DU VECTEUR &&PROMOR.ANCIEN.LM
!     CE VECTEUR SERA AGRANDI SI NECESSAIRE
    mxddlt=100
    call wkvect('&&PROMOR.ANCIEN.LM', 'V V I', mxddlt, jalm)
!
!
!
!
!     -- ALLOCATION DE .SMOS.SMDI
    call wkvect(nu//'.SMOS.SMDI', base//' V I', nequ, jsmdi)
!
!
!---- INITIALISATION DES TABLEAUX POUR LE STOCKAGE MORSE
!     ATTENTION:   PENDANT LA CONSTRUCTION DE LA STRUCTURE CHAINEE
!                  (SMDI,SMHC,ISUIV) DE LA MATRICE ON A
!     ZI(JSMDI+.): POINTEUR DEBUT DE CHAINE
!     ZI(JSMHC-1+II) : MAILLON QUI CONTIENT L'INDICE COLONNE
!                       DE LA CHAINE II
!     ISUIV(II)     : MAILLON SUIVANT DE LA MEME CHAINE.
!
!     NEQX   : COMPTEUR DU NOMBRE D'EQUATION (CONTROLE)
    neqx=0
!
!     IILIB  : 1-ERE PLACE LIBRE
    iilib=1
!
!
!     -- BOUCLE SUR LES LIGREL DE NU//'.NUME.LILI' :
!     -----------------------------------------------
    do 140 ili = 2, nlili
        call jenuno(jexnum(nu//'.NUME.LILI', ili), nomlig)
        call dismoi('F', 'EXI_ELEM', nomlig, 'LIGREL', ibid,&
                    exiele, ierd)
        call dismoi('F', 'EXI_VF', nomlig, 'LIGREL', ibid,&
                    exivf, ierd)
!
        if (nomlig(1:8) .eq. mo) then
            call dismoi('F', 'NB_SS_ACTI', mo, 'MODELE', nbss,&
                        kbid, ierd)
        else
            nbss=0
        endif
        if (exiele .eq. 'NON') goto 90
!
!
!       1. TRAITEMENT DES ELEMENTS FINIS CLASSIQUES:
!       --------------------------------------------
        if (exivf .eq. 'NON') then
            do 80 igr = 1, zzngel(ili)
                if (lmadis) then
                    if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 80
                endif
                nel=zznelg(ili,igr)
                do 70 iel = 1, nel
                    nddlt=0
                    numa=zzliel(ili,igr,iel)
!
                    if (numa .gt. 0) then
!             -- MAILLES DU MAILLAGE :
                        if (lmadis .and. ldist .and. .not.ldgrel) then
                            if (zi(jnumsd+numa-1) .ne. rang) goto 70
                        endif
!
                        nnoe=zznbne(numa)
                        do 30 k1 = 1, nnoe
                            n1=zzconx(numa,k1)
                            iad1=zzprno(1,n1,1)
                            nddl1=zzprno(1,n1,2)
                            if (mxddlt .lt. (nddlt+nddl1)) then
                                mxddlt=2*(nddlt+nddl1)
                                call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                                call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                            endif
                            do 20 iddl = 1, nddl1
                                zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+&
                                iddl-1)
20                          continue
                            nddlt=nddlt+nddl1
30                      continue
!
                    else
!             -- MAILLES TARDIVES :
                        if (lmadis .and. ldist .and. .not.ldgrel) then
                            if (rang .ne. 0) goto 70
                        endif
!
                        numa=-numa
                        nnoe=zznsup(ili,numa)
                        do 50 k1 = 1, nnoe
                            n1=zznema(ili,numa,k1)
                            if (n1 .lt. 0) then
                                n1=-n1
                                iad1=zzprno(ili,n1,1)
                                nddl1=zzprno(ili,n1,2)
                            else
                                iad1=zzprno(1,n1,1)
                                nddl1=zzprno(1,n1,2)
                            endif
                            if (mxddlt .lt. (nddlt+nddl1)) then
                                mxddlt=2*(nddlt+nddl1)
                                call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                                call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                            endif
                            do 40 iddl = 1, nddl1
                                zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+&
                                iddl-1)
40                          continue
                            nddlt=nddlt+nddl1
50                      continue
                    endif
!
!           -- TRI EN ORDRE CROISSANT POUR L'INSERTION DES COLONNES
                    call assert(nddlt.le.mxddlt)
                    call uttrii(zi(jalm), nddlt)
!
!           -- INSERTION DES COLONNES DE L'ELEMENT DANS
!               LA STRUCTURE CHAINEE
                    do 60 iddl = 0, nddlt-1
                        jddl=jsmdi+zi(jalm+iddl)-1
                        if (zi(jddl) .eq. 0) neqx=neqx+1
                        call moinsr(zi(jalm+iddl), iddl+1, jalm, jsmdi, jsuiv,&
                                    '&&PROMOR.NOSUIV', jnoip, '&&PROMOR.NOIP', iilib, iimax)
60                  continue
70              continue
80          continue
!
!
!
!       2. TRAITEMENT DES ELEMENTS FINIS DE TYPE VOISIN_VF :
!       ----------------------------------------------------
        else if (exivf.eq.'OUI') then
            call jeveuo(nomlig//'.REPE', 'L', jrepe)
            call jeveuo(nomlig//'.NVGE', 'L', jnvge)
            vge=zk16(jnvge-1+1)
            call jeveuo(vge//'.PTVOIS', 'L', jptvoi)
            call jeveuo(vge//'.ELVOIS', 'L', jelvoi)
            do 81 igr = 1, zzngel(ili)
                nel=zznelg(ili,igr)
                itypel=zzliel(ili,igr,nel+1)
                call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
                call teattr(nomte, 'S', 'TYPE_VOISIN', codvoi, ibid)
                do 71 iel = 1, nel
                    nddlt=0
                    numa=zzliel(ili,igr,iel)
                    call assert(numa.gt.0)
                    nnoe=zznbne(numa)
                    call voiuti(numa, codvoi, nvoima, nscoma, jrepe,&
                                jptvoi, jelvoi, nbvois, livois, tyvois,&
                                nbnovo, nbsoco, lisoco)
                    do 32,kvois=1,nbvois
                    numav=livois(kvois)
                    nnoe=zznbne(numa)
                    do 31 k1 = 1, nnoe
                        n1=zzconx(numa,k1)
                        iad1=zzprno(1,n1,1)
                        nddl1=zzprno(1,n1,2)
                        if (mxddlt .lt. (nddlt+nddl1)) then
                            mxddlt=2*(nddlt+nddl1)
                            call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                            call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                        endif
                        do 21 iddl = 1, nddl1
                            zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+&
                                iddl-1)
21                      continue
                        nddlt=nddlt+nddl1
31                  continue
                    call assert(nddlt.le.mxddlt)
!
                    nnov=zznbne(numav)
                    do 33 k1 = 1, nnov
                        n1=zzconx(numav,k1)
                        iad1=zzprno(1,n1,1)
                        nddl1=zzprno(1,n1,2)
                        if (mxddlt .lt. (nddlt+nddl1)) then
                            mxddlt=2*(nddlt+nddl1)
                            call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                            call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                        endif
                        do 23 iddl = 1, nddl1
                            zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+&
                                iddl-1)
23                      continue
                        nddlt=nddlt+nddl1
33                  continue
                    call assert(nddlt.le.mxddlt)
!
                    call uttrii(zi(jalm), nddlt)
                    do 61 iddl = 0, nddlt-1
                        jddl=jsmdi+zi(jalm+iddl)-1
                        if (zi(jddl) .eq. 0) neqx=neqx+1
                        call moinsr(zi(jalm+iddl), iddl+1, jalm, jsmdi, jsuiv,&
                                    '&&PROMOR.NOSUIV', jnoip, '&&PROMOR.NOIP', iilib, iimax)
61                  continue
32                  continue
71              continue
81          continue
!
        else
            call assert(.false.)
        endif
!
!
!
!       3. TRAITEMENT DES SOUS-STRUCTURES STATIQUES :
!       ---------------------------------------------
90      continue
        if (nbss .gt. 0) then
            do 130,ima=1,nbsma
            if (zi(iasssa-1+ima) .eq. 0) goto 130
            nddlt=0
            call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
            call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nnoe, kbid)
            do 110 k1 = 1, nnoe
                n1=zi(iamail-1+k1)
                call assert(n1.ne.0)
                iad1=zzprno(1,n1,1)
                nddl1=zzprno(1,n1,2)
                if (mxddlt .lt. (nddlt+nddl1)) then
                    mxddlt=2*(nddlt+nddl1)
                    call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                    call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                endif
                do 100 iddl = 1, nddl1
                    zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+iddl-1)
100              continue
                nddlt=nddlt+nddl1
110          continue
!
            call assert(nddlt.le.mxddlt)
            call uttrii(zi(jalm), nddlt)
            do 120 iddl = 0, nddlt-1
                jddl=jsmdi+zi(jalm+iddl)-1
                if (zi(jddl) .eq. 0) neqx=neqx+1
                call moinsr(zi(jalm+iddl), iddl+1, jalm, jsmdi, jsuiv,&
                            '&&PROMOR.NOSUIV', jnoip, '&&PROMOR.NOIP', iilib, iimax)
120          continue
130          continue
        endif
140  end do
!
    if ((neqx.ne.nequ) .and. (.not.lmadis)) then
        vali(1)=nequ
        vali(2)=neqx
        call u2mesg('F', 'ASSEMBLA_65', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
!
!
!
!     DESIMBRIQUATION DE CHAINES POUR OBTENIR LA STRUCTURE COMPACTE
!     (SMDI,SMHC) DE LA MATRICE
    call wkvect(nu//'.SMOS.SMH1', base//' V S', iimax, jsmh1)
    call moinip(neqx, ncoef, zi(jsmdi), zi(jsuiv), zi(jnoip),&
                zi4(jsmh1))
    call wkvect(nu//'.SMOS.SMHC', base//' V S', ncoef, jsmhc)
    do 150 iddl = 1, ncoef
        zi4(jsmhc+iddl-1)=zi4(jsmh1+iddl-1)
150  end do
    call jedetr(nu//'.SMOS.SMH1')
!
!
!     -- CREATION ET REMPLISSAGE DE .SMDE
    call wkvect(nu//'.SMOS.SMDE', base//' V I', 6, jsmde)
    zi(jsmde-1+1)=nequ
    zi(jsmde-1+2)=ncoef
    zi(jsmde-1+3)=1
!
!
    call jedetr('&&PROMOR.NOIP')
    call jedetr('&&PROMOR.NOSUIV')
    call jedetr('&&PROMOR.ANCIEN.LM   ')
!
    if (niv .ge. 1) then
        vali(1) = nequ
        vali(2) = ncoef
        vali(3) = 2*ncoef-nequ
        rcoef = ncoef
        requ = nequ
        valr(1) = (100.d0*(2.d0*rcoef-requ)) / (requ*requ)
        call u2mesg('I', 'FACTOR_2', 0, ' ', 3,&
                    vali, 1, valr)
    endif
!
    if (lfeti) call infbav()
    call jedema()
!
end subroutine
