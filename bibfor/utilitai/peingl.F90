subroutine peingl(resu, modele, mate, cara, nchar,&
                  lchar, nh, nbocc, motfaz)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8prem.h'
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/etenca.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/exlim3.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jerecu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mecham.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vrcins.h'
    include 'asterfort/vrcref.h'
    include 'asterfort/wkvect.h'
    integer :: nchar, nh, nbocc
    character(len=*) :: resu, modele, mate, cara, lchar(1), motfaz
!.======================================================================
! TOLE CRP_20
!
!      PEINGL  --  OPERATEUR POST_ELEM
!                  TRAITEMENT DU MOT-FACTEUR "INDIC_ENER"
!                          ET DU MOT-FACTEUR "INDIC_SEUIL"
!
!                  CALCUL DES INDICATEURS GLOBAUX DE
!                  DE PERTE DE PROPORTIONNALITE DU CHARGEMENT.
!
! -----------------------------------------------------------------
!
!           -POUR LE MOT-CLE INDIC_ENER, ON CALCULE L'INDICATEUR
!            GLOBAL ENERGETIQUE DETERMINE PAR L'EXPRESSION
!            SUIVANTE :
!            IE = (SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV)/V
!
!        OU  .OMEGA EST LA DENSITE D'ENERGIE TOTALE
!            (I.E. OMEGA = SOMME_0->T(SIGMA:D(EPS)/DT).DTAU
!            .PSI EST LA DENSITE D'ENERGIE ELASTIQUE 'TOTALE'
!            (I.E. ASSOCIEE A LA COURBE DE TRACTION SI ON
!                  CONSIDERAIT LE MATERIAU ELASTIQUE NON-LINEAIRE)
!            .V EST LE VOLUME DU GROUPE DE MAILLES TRAITE
!
! -----------------------------------------------------------------
!
!           -POUR LE MOT-CLE INDIC_SEUIL, ON CALCULE L'INDICATEUR
!            GLOBAL  DETERMINE PAR L'EXPRESSION SUIVANTE :
!
!   IS = (SOMME_DOMAINE(1 - ((SIG-X):EPS_PLAST)/((SIG_Y+R)*P)).DV)/V
!
!        OU  .SIG       EST LE TENSEUR DES CONTRAINTES
!            .X         EST LE TENSEUR DE RAPPEL
!            .EPS_PLAST EST LE TENSEUR DES DEFORMATIONS PLASTIQUES
!            .SIG_Y     EST LA LIMITE D'ELASTICITE
!            .R         EST LA FONCTION D'ECROUISSAGE
!            .P         EST LA DEFORMATION PLASTIQUE CUMULEE
!            .V EST LE VOLUME DU GROUPE DE MAILLES TRAITE
! -----------------------------------------------------------------
!
!  MOT-CLE ENER_ELAS : CALCUL DE L'ENERGIE DE DEFORMATION ELASTIQUE
!  =================   DETERMINEE PAR L'EXPRESSION SUIVANTE :
!
!   ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)
!
!        OU  .SIG       EST LE TENSEUR DES CONTRAINTES
!            .D         EST LE TENSEUR DE HOOKE
!
! -----------------------------------------------------------------
!
!  MOT-CLE ENER_TOTALE : CALCUL DE L'ENERGIE DE DEFORMATION TOTALE
!  ===================   DETERMINEE PAR L'EXPRESSION SUIVANTE :
!
!   ENER_TOTALE =  ENELAS + EPLAS
!
!          AVEC : ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)
!                 ENELAS EST L'ENERGIE DE DEFORMATION ELASTIQUE
!
!           OU  .SIG       EST LE TENSEUR DES CONTRAINTES
!               .D         EST LE TENSEUR DE HOOKE
!
!          ET   : EPLAS = SOMME_VOLUME((R(P))*D(P))
!                 EPLAS EST L'ENERGIE DE DEFORMATION PLASTIQUE
!
!           OU  .P         EST LA DEFORMATION PLASTIQUE CUMULEE
!           ET   R(P) EST CALCULE POUR LES COMPORTEMENTS SUIVANTS :
!                      .VMIS_ISOT_LINE
!                      .VMIS_ISOT_TRAC
!                      .VMIS_ECMI_LINE
!                      .VMIS_ECMI_TRAC
!                      .VMIS_CINE_LINE
!                      .VISC_CIN1_CHAB
!                      .VISC_CIN2_CHAB
!
!          POUR LES AUTRES COMPORTEMENTS ON S'ARRETE EN ERREUR FATALE
!
! -----------------------------------------------------------------
!
!   ARGUMENT        E/S  TYPE         ROLE
!    RESU           VAR    K*      TABLE EN SORTIE DE LA COMMANDE
!    MODELE         IN     K*      NOM DU MODELE SUR-LEQUEL ON FAIT
!                                  LE CALCUL
!    MATE           IN     K*      NOM DU CHAMP MATERIAU
!    CARA           IN     K*      NOM DU CHAMP DES CARA_ELEM
!    NCHAR          IN     I       NOMBRE DE  CHARGES
!    LCHAR(1)       IN     K*      LISTE  DES CHARGES
!    NH             IN     I       NUMERO D'HARMONIQUE DE FOURIER
!    NBOCC          IN     I       NOMBRE D'OCCURENCES DU MOT-FACTEUR
!                                  INDIC_ENER
!    MOTFAZ         IN     K*      NOM DU MOT-CLE FACTEUR "INDIC_ENER"
!                                                     OU  "INDIC_SEUIL"
!                                                     OU  "ENER_ELAS"
!                                                     OU  "ENER_TOTALE"
!                                                     OU  "ENER_DISS"
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: nbparr, nr, np, nc, iret, jord, nbordr, jins, iord, iainst
    integer :: numord, nbin, nt, nm, ng, ibid, nbgrma, jgr, ig, nbma, jad
    integer :: nbmail, jma, im, iocc, nume, nbout, numorm, idesc, ngdmax, ncmpmx
    integer :: ivale, iptma, igd, idebgd, dg, ima, iconex, nbno, nec, ivari, i
    real(kind=8) :: work(5), indic1, volume, inst, valr(6), zero, prec, energi
    complex(kind=8) :: c16b
    character(len=2) :: codret
    character(len=8) :: resul, crit, noma, nommai, vk8(2), kiordm
    character(len=8) :: kiord, k8b, lpain(10), lpaout(2), typarr(9)
    character(len=8) :: nomgd
    character(len=16) :: typres, motfac, noparr(9), ligrmo, compt, option
    character(len=19) :: knum, ligrel, kins, compor
    character(len=19) :: chvarc, chvref
    character(len=24) :: chgeom, chcara(18), chharm, chvari, chdepl
    character(len=24) :: vk24(2), nomgrm
    character(len=24) :: chsig, lchin(10), lchout(2)
    character(len=24) :: mlggma, mlgnma
    character(len=24) :: chsigm, chdepm, chbid
    logical :: evol
    integer :: iarg
!
    data typarr/'I','R','K24','K8','R','R','R','R','R'/
    data chvarc,chvref /'&&PEINGL.CHVARC','&&PEINGL.CHVARC.REF'/
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    ivari=0
    chbid='&&PEINGL.VARINUL'
    compor='&&PEINGL.COMPNUL'
    compt='XXXXXXXXXXXXXXXX'
    evol=.false.
    zero = 0.0d0
    motfac = motfaz
    option = motfaz
    noparr(1) = 'NUME_ORDRE'
    noparr(2) = 'INST'
    noparr(3) = 'LIEU'
    noparr(4) = 'ENTITE'
    noparr(5) = motfac
    nbparr = 5
    if (motfac(1:4) .eq. 'ENER') then
        noparr(5) = 'TOTALE'
        if (motfac .eq. 'ENER_DISS') then
            option='DISS_ELEM'
            noparr(6) = 'ENDO'
            noparr(7) = 'PLAS'
            nbparr = 7
        else if (motfac.eq.'ENER_ELAS') then
            option='ENEL_ELEM'
            noparr(6) = 'MEMBRANE'
            noparr(7) = 'FLEXION'
            noparr(8) = 'CISAILLE'
            noparr(9) = 'COUPL_MF'
            nbparr = 9
        else if (motfac.eq.'ENER_TOTALE') then
            option='ENER_TOTALE'
            noparr(6) = 'MEMBRANE'
            noparr(7) = 'FLEXION'
            nbparr = 7
        endif
    endif
    energi = zero
    do 8 i = 1, 5
        work(i)=0.d0
 8  end do
    do 9 i = 1, 6
        valr(i)=0.d0
 9  end do
!
! --- RECUPERATION DU RESULTAT A TRAITER :
!     ----------------------------------
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                resul, nr)
!
    if (nr .eq. 0) then
        call u2mess('F', 'UTILITAI3_76')
    endif
!
! --- ON VERIFIE QUE LE  RESULTAT A TRAITER EST DE TYPE EVOL_NOLI :
!     -----------------------------------------------------------
    call gettco(resul, typres)
    evol=(typres(1:9).eq.'EVOL_NOLI').or.(typres(1:9).eq.'EVOL_ELAS')
    if (.not.evol) then
        call u2mess('F', 'UTILITAI3_77')
    endif
!
! --- RECUPERATION DE LA PRECISION POUR LE TRAITEMENT DES NUMEROS
! --- D'ORDRE :
!     -------
    call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                prec, np)
!
! --- RECUPERATION DU CRITERE POUR LE TRAITEMENT DES NUMEROS D'ORDRE :
!     --------------------------------------------------------------
    call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                crit, nc)
!
! --- RECUPERATION DES NUMEROS D'ORDRE A TRAITER :
!     ------------------------------------------
    knum = '&&PEINGL.NUME_ORDRE'
    call rsutnu(resul, ' ', 0, knum, nbordr,&
                prec, crit, iret)
    if (iret .ne. 0) goto 70
    call jeveuo(knum, 'L', jord)
!
! --- RECUPERATION DES INSTANTS CORRESPONDANT AUX NUMEROS D'ORDRE :
!     -----------------------------------------------------------
    kins = '&&PEINGL.INSTANT'
    call wkvect(kins, 'V V R', nbordr, jins)
    call jenonu(jexnom(resul//'           .NOVA', 'INST'), iret)
    if (iret .ne. 0) then
        do 10 iord = 1, nbordr
            numord = zi(jord+iord-1)
            call rsadpa(resul, 'L', 1, 'INST', numord,&
                        0, iainst, k8b)
            zr(jins+iord-1) = zr(iainst)
10      continue
    endif
!
! --- VERIFICATIONS ET RECUPERATION DU NOM DU MAILLAGE :
!     ------------------------------------------------
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 80
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
!
    call exlim3(motfaz, 'V', modele, ligrel)
!
! ---  CREATION DE LA TABLE 'GLOBALE' :
!      -----------------------------
    call tbcrsd(resu, 'G')
    call tbajpa(resu, nbparr, noparr, typarr)
!
! --- BOUCLE SUR LES NUMEROS D'ORDRE DU RESULTAT :
!     ------------------------------------------
    do 60 iord = 1, nbordr
        call jemarq()
        call jerecu('V')
!
! ---  RECUPERATION DU NUMERO D'ORDRE :
!      ------------------------------
        numord = zi(jord+iord-1)
        call codent(numord, 'G', kiord)
!
! ---  RECUPERATION DE L'INSTANT :
!      -------------------------
        inst = zr(jins+iord-1)
        valr(1) = inst
!
! ---  RECUPERATION OU CONSTITUTION DU CHAMP DE VARIABLE DE COMMANDE
! ---  ET DE LA VARIABLE DE COMMANDE DE REFERENCE :
!      --------------------------------------------
!
        call vrcins(modele, mate, cara, inst, chvarc,&
                    codret)
        call vrcref(modele(1:8), mate(1:8), cara(1:8), chvref(1:19))
!
        if (typres(1:9) .eq. 'EVOL_NOLI') then
!
! ---    RECUPERATION DE LA RELATION DE COMPORTEMENT ASSOCIEE AU
! ---    NUMERO D'ORDRE COURANT :
!        ----------------------
            call rsexch('F', resul, 'COMPORTEMENT', numord, compor,&
                        iret)
!
!CC---   RECUPERATION DU COMPOR PAR ETENCA
!
            ligrmo = modele//'.MODELE'
!
! ---    CREATION DU TABLEAU DESCRIPTEUR DE LA CARTE COMPOR ---
!
            call etenca(compor, ligrmo, iret)
            if (iret .ne. 0) then
                call u2mess('F', 'UTILITAI2_62')
            endif
! ---    RECUPERATION DE LA GRANDEUR (ICI COMPOR)  ---
! ---    REFERENCEE PAR LA CARTE COMPO             ---
!
            call jeveuo(compor//'.DESC', 'L', idesc)
            ngdmax = zi(idesc+2-1)
!
            nomgd = 'COMPOR  '
!
            call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                        k8b, iret)
!
            if (nec .gt. 1) then
                call u2mess('F', 'UTILITAI2_61')
            endif
!
!
! ---    NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR  ---
!
            call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx, k8b)
!
! ---    TABLEAU DE VALEURS DE LA CARTE COMPO     ---
! ---    (CONTENANT LES VALEURS DU COMPORTEMENT)  ---
!
            call jeveuo(compor//'.VALE', 'L', ivale)
!
! ---    RECUPERATION DU VECTEUR D'ADRESSAGE DANS LA CARTE  ---
! ---    CREE PAR ETENCA                                    ---
!
            call jeveuo(compor//'.PTMA', 'L', iptma)
!
! ---    AFFECTATION DU TABLEAU DES NOEUDS  ---
!
! ---    NOMBRE DE MAILLES DU MAILLAGE ---
!
            call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                        k8b, iret)
!
            do 20 ima = 1, nbma
                if (zi(iptma+ima-1) .ne. 0) then
                    igd = zi(iptma+ima-1)
                    idebgd = (igd-1)*ncmpmx
                    dg = zi(idesc+3+2*ngdmax+zi(iptma+ima-1)-1)
!
! ---        ON S'ASSURE QUE LA PREMIERE COMPOSANTE DE LA GRANDEUR
! ---        QUI EST RELCOM A BIEN ETE AFFECTEE
!
                    if (.not.exisdg(dg,1)) then
                        call u2mess('F', 'UTILITAI2_63')
                    endif
! ---        RECUPERATION DU COMPORTEMENT AFFECTE A LA MAILLE
                    compt = zk16(ivale+idebgd+1-1)
!
! ---        RECUPERATION DES NUMEROS DES NOEUDS DE LA MAILLE
                    call jeveuo(jexnum(noma//'.CONNEX', ima), 'L', iconex)
                    call jelira(jexnum(noma//'.CONNEX', ima), 'LONMAX', nbno, k8b)
!
                endif
20          continue
!
!CC---FIN DE RECUPERATION DU COMPOR
!
        endif
!
! ---  RECUPERATION DU CHAMP DE CONTRAINTES ASSOCIE AU
! ---  NUMERO D'ORDRE COURANT :
!      ----------------------
        call rsexch('F', resul, 'SIEF_ELGA', numord, chsig,&
                    iret)
!
! --- SI LE NUMERO COURANT EST INFERIEUR A NBORDR ON RECUPERE LES
! --- CONTRAINTES DE L INSTANT PRECEDENT
!
        if (iord .gt. 1) then
            numorm = zi(jord+iord-1-1)
            call codent(numorm, 'G', kiordm)
            call rsexch('F', resul, 'SIEF_ELGA', numorm, chsigm,&
                        iret)
        endif
!
! ---  RECUPERATION DU CHAMP DES VARIABLES INTERNES ASSOCIE AU
! ---  NUMERO D'ORDRE COURANT DANS LE CAS DES EVOL_NOLI
!      ----------------------
        if (typres(1:9) .eq. 'EVOL_NOLI') then
            call rsexch(' ', resul, 'VARI_ELGA', numord, chvari,&
                        iret)
            ivari=1
            if (iret .gt. 0) then
                if (motfac .ne. 'ENER_ELAS') then
                    vk24(1) = resul
                    vk24(2) = kiord
                    call u2mesk('F', 'UTILITAI3_79', 2, vk24)
                else
! CREATION D'UN CHAMP DE VARIABLES INTERNES NUL
                    ivari=0
                    chbid='&&PEINGL.VARINUL'
                    call calcul('S', 'TOU_INI_ELGA', ligrmo, 1, chgeom,&
                                'PGEOMER', 1, chbid, 'PVARI_R', 'V',&
                                'OUI')
                endif
            endif
        endif
!
! ---  RECUPERATION DU CHAMP DES DEPLACEMENTS ASSOCIE AU
! ---  NUMERO D'ORDRE COURANT :
!      ----------------------
        call rsexch('F', resul, 'DEPL', numord, chdepl,&
                    iret)
!
! ---  RECUPERATION DU CHAMP DES DEPLACEMENTS ASSOCIE AU
! ---  NUMERO D'ORDRE PRECEDENT :
!      ----------------------
        if (iord .gt. 1) then
            call rsexch('F', resul, 'DEPL', numorm, chdepm,&
                        iret)
        endif
!
! ---  CALCUL DE L'INDICATEUR GLOBAL DE PERTE DE RADIALITE
! ---  SUR TOUTES LES MAILLES DU MODELE :
!      --------------------------------
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PDEPLR'
        lchin(3) = chdepl
        lpain(4) = 'PCONTPR'
        lchin(4) = chsig
        lpain(5) = 'PVARIPR'
        if (ivari .eq. 1) then
            lchin(5) = chvari
        else
            lchin(5) = chbid
        endif
        lpain(6) = 'PCOMPOR'
        lchin(6) = compor
        lpain(7) = 'PVARCPR'
        lchin(7) = chvarc
        lpain(8) = 'PVARCRR'
        lchin(8) = chvref
        lpain(9) = 'PCACOQU'
        lchin(9) = chcara(7)
        lpain(10) = 'PNBSP_I'
        lchin(10) = cara//'.CANBSP'
        nbin = 10
        if (option .eq. 'ENER_TOTALE') then
            if (iord .gt. 1) then
                lpain(9) = 'PCONTMR'
                lchin(9) = chsigm
                lpain(10) = 'PDEPLM'
                lchin(10) = chdepm
                nbin = 10
            endif
        endif
        if (option .eq. 'INDIC_ENER' .or. option .eq. 'INDIC_SEUIL') then
            nbout = 2
            lpaout(1) = 'PENERD1'
            lchout(1) = '&&PEINGL.INDIC'
            lpaout(2) = 'PENERD2'
            lchout(2) = '&&PEINGL.VOLUME'
            else if (option.eq.'ENEL_ELEM' .or. option.eq.'ENER_TOTALE')&
        then
            nbout = 1
            lpaout(1) = 'PENERD1'
            lchout(1) = '&&PEINGL.INDIC'
        else if (option.eq.'DISS_ELEM') then
            nbout = 1
            lpaout(1) = 'PDISSD1'
            lchout(1) = '&&PEINGL.INDIC'
        endif
!
        call calcul('S', option, ligrel, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
!
! ---  BOUCLE SUR LES OCCURENCES DU MOT-CLE INDIC_ENER :
!      -----------------------------------------------
        do 50 iocc = 1, nbocc
!
! ---   RECUPERATION DES MAILLES POUR LESQUELLES ON VA CALCULER
! ---   L'INDICATEUR :
!       ------------
            call getvtx(motfac, 'TOUT', iocc, iarg, 0,&
                        k8b, nt)
            call getvem(noma, 'MAILLE', motfac, 'MAILLE', iocc,&
                        iarg, 0, k8b, nm)
            call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA', iocc,&
                        iarg, 0, k8b, ng)
!
! ---   TRAITEMENT DU MOT CLE "TOUT" ,LA QUANTITE EST CALCULEE
! ---   SUR TOUT LE MODELE :
!       ------------------
            if (nt .ne. 0) then
                if (motfac .eq. 'INDIC_ENER' .or. motfac .eq. 'INDIC_SEUIL') then
!
! ---     SOMMATION DES INTEGRALES SUIVANTES SUR LE MODELE
! ---     LA PREMIERE INTEGRALE CALCULEE EST :
! ---     SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV
! ---     LA SECONDE INTEGRALE CALCULEE EST LE VOLUME :
!         -------------------------------------------
                    call mesomm(lchout(1), 1, ibid, work(1), c16b,&
                                0, ibid)
                    call mesomm(lchout(2), 1, ibid, work(2), c16b,&
                                0, ibid)
!
                    indic1 = work(1)
                    volume = work(2)
!
                    if (indic1 .le. 1.0d4*r8prem()) then
                        indic1 = zero
                    endif
!
                    if (volume .le. r8prem()) then
                        call u2mess('F', 'UTILITAI3_80')
                    endif
!
                    valr(2) = indic1/volume
                    vk8(1) = noma
                    vk8(2) = 'TOUT'
!
                    else if (motfac.eq.'ENER_ELAS'  .or.&
     &               motfac.eq.'ENER_TOTALE'.or.&
     &               motfac.eq.'ENER_DISS') then
!
! ---          SOMMATION DE L'ENERGIE ( ELASTIQUE OU TOTALE)
! ---          SUR LE MODELE :
!              -------------
                    if (motfac .eq. 'ENER_TOTALE') then
                        call mesomm(lchout(1), 1, ibid, work(1), c16b,&
                                    0, ibid)
                    else if (motfac.eq.'ENER_DISS') then
                        call mesomm(lchout(1), 3, ibid, work(1), c16b,&
                                    0, ibid)
                    else
                        call mesomm(lchout(1), 5, ibid, work(1), c16b,&
                                    0, ibid)
                    endif
! ---  BOUCLE SUR LES PAS DE TEMPS ON SOMME LES TERMES DE
! ---  L ENERGIE TOTAL
                    if ((compt(1:9).ne.'VMIS_ISOT') .and. (compt(1:4) .ne.'ELAS') .and.&
                        (motfac.ne.'ENER_ELAS') .and. (motfac.ne.'ENER_DISS')) then
                        energi = energi + work(1)
                    else
                        energi = work(1)
                    endif
!
                    valr(2) = energi
                    valr(3) = work(2)
                    valr(4) = work(3)
                    vk8(1) = noma
                    vk8(2) = 'TOUT'
                    if (motfac .eq. 'ENER_ELAS') then
! ---    AJOUT INUTILE POUR L INSTANT PUISQUE WORK(4) ET WORK(5)
!        SONT NULS. EN PREVISION DU CALCUL DE L ENERGIE ELASTIQUE
!        DE CISAILLEMENT ET DE COUPLAGE MEMBRANE FLEXION POUR LES
!        PLAQUES EN MECA STATIQUE UNIQUEMENT, SI ON L AUTORISE
!        UN JOUR.
                        valr(5) = work(4)
                        valr(6) = work(5)
                    endif
!
                endif
!
! ---    ECRITURE DE L'INDICATEUR OU DE L'ENERGIE DANS LA TABLE :
!        ------------------------------------------------------
                call tbajli(resu, nbparr, noparr, numord, valr,&
                            c16b, vk8, 0)
            endif
!
! ---   TRAITEMENT DU MOT CLE "GROUP_MA" ,LA QUANTITE EST CALCULEE
! ---   SUR LE GROUP_MA COURANT :
!       -----------------------
            if (ng .ne. 0) then
                nbgrma = -ng
                call wkvect('&&PEINGL_GROUPM', 'V V K24', nbgrma, jgr)
                call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA', iocc,&
                            iarg, nbgrma, zk24(jgr), ng)
!
! ---     BOUCLE SUR LES GROUPES DE MAILLES :
!         ---------------------------------
                vk24(2) = 'GROUP_MA'
                do 30 ig = 1, nbgrma
                    nomgrm = zk24(jgr+ig-1)
                    call jeexin(jexnom(mlggma, nomgrm), iret)
                    if (iret .eq. 0) then
                        call u2mesk('F', 'UTILITAI3_46', 1, nomgrm)
                    endif
                    call jelira(jexnom(mlggma, nomgrm), 'LONUTI', nbma, k8b)
                    if (nbma .eq. 0) then
                        call u2mesk('F', 'UTILITAI3_47', 1, nomgrm)
                    endif
                    call jeveuo(jexnom(mlggma, nomgrm), 'L', jad)
!
                    if (motfac .eq. 'INDIC_ENER' .or. motfac .eq. 'INDIC_SEUIL') then
!
! ---      SOMMATION DES INTEGRALES SUIVANTES SUR LES
! ---      MAILLES DU GROUP_ MA
! ---      LA PREMIERE INTEGRALE CALCULEE EST :
! ---      SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV
! ---      LA SECONDE INTEGRALE CALCULEE EST LE VOLUME :
!          -------------------------------------------
                        call mesomm(lchout(1), 1, ibid, work(1), c16b,&
                                    nbma, zi(jad))
                        call mesomm(lchout(2), 1, ibid, work(2), c16b,&
                                    nbma, zi(jad))
!
                        indic1 = work(1)
                        volume = work(2)
!
                        if (indic1 .le. 1.0d4*r8prem()) then
                            indic1 = zero
                        endif
!
                        if (volume .le. r8prem()) then
                            call u2mesk('F', 'UTILITAI3_81', 1, nomgrm)
                        endif
!
                        valr(2) = indic1/volume
                        vk24(1) = nomgrm
!
                        else if (motfac.eq.'ENER_ELAS' .or.&
     &                 motfac.eq.'ENER_TOTALE' .or.&
     &                 motfac.eq.'ENER_DISS') then
!
! ---          SOMMATION DE L'ENERGIE ( ELASTIQUE OU TOTALE)
! ---          SUR LE MODELE :
!              -------------
                        if (motfac .eq. 'ENER_TOTALE') then
                            call mesomm(lchout(1), 1, ibid, work(1), c16b,&
                                        nbma, zi(jad))
                        else if (motfac.eq.'ENER_DISS') then
                            call mesomm(lchout(1), 3, ibid, work(1), c16b,&
                                        nbma, zi(jad))
                        else
                            call mesomm(lchout(1), 5, ibid, work, c16b,&
                                        nbma, zi(jad))
                        endif
!
! ---  BOUCLE SUR LES PAS DE TEMPS ON SOMME LES TERMES DE
! ---  L ENERGIE TOTAL
!
                        if ((compt(1:9).ne.'VMIS_ISOT') .and. (compt( 1:4).ne.'ELAS') .and.&
                            (motfac.ne.'ENER_ELAS') .and. (motfac.ne.'ENER_DISS')) then
!
!
                            energi = energi + work(1)
                        else
                            energi = work(1)
                        endif
!
                        valr(2) = energi
                        valr(3) = work(2)
                        valr(4) = work(3)
                        vk24(1) = nomgrm
                        if (motfac .eq. 'ENER_ELAS') then
! ---    AJOUT INUTILE POUR L INSTANT PUISQUE WORK(4) ET WORK(5)
!        SONT NULS. EN PREVISION DU CALCUL DE L ENERGIE ELASTIQUE
!        DE CISAILLEMENT ET DE COUPLAGE MEMBRANE FLEXION POUR LES
!        PLAQUES EN MECA STATIQUE UNIQUEMENT, SI ON L AUTORISE
!        UN JOUR.
                            valr(5) = work(4)
                            valr(6) = work(5)
                        endif
!
                    endif
!
!
! ---    ECRITURE DE L'INDICATEUR OU DE L'ENERGIE DANS LA TABLE :
!        ------------------------------------------------------
!
! ---      ECRITURE DE L'INDICATEUR DANS LA TABLE :
!          --------------------------------------
                    call tbajli(resu, nbparr, noparr, numord, valr,&
                                c16b, vk24, 0)
30              continue
!
                call jedetr('&&PEINGL_GROUPM')
            endif
!
! ---   TRAITEMENT DU MOT CLE "MAILLE" ,L'INDICATEUR EST CALCULE
! ---   SUR LA MAILLE COURANTE :
!       ----------------------
            if (nm .ne. 0) then
                nbmail = -nm
                call wkvect('&&PEINGL_MAILLE', 'V V K8', nbmail, jma)
                call getvem(noma, 'MAILLE', motfac, 'MAILLE', iocc,&
                            iarg, nbmail, zk8(jma), nm)
!
! ---    BOUCLE SUR LES MAILLES :
!        ----------------------
                vk8(2) = 'MAILLE'
                do 40 im = 1, nbmail
                    nommai = zk8(jma+im-1)
                    call jeexin(jexnom(mlgnma, nommai), iret)
                    if (iret .eq. 0) then
                        call u2mesk('F', 'UTILITAI3_49', 1, nommai)
                    endif
                    call jenonu(jexnom(mlgnma, nommai), nume)
!
                    if (motfac .eq. 'INDIC_ENER' .or. motfac .eq. 'INDIC_SEUIL') then
!
! ---      LES INTEGRALES SONT CALCULEES SUR LA MAILLE COURANTE
! ---      LA PREMIERE INTEGRALE CALCULEE EST :
! ---      SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV
! ---      LA SECONDE INTEGRALE CALCULEE EST LE VOLUME :
!          -------------------------------------------
                        call mesomm(lchout(1), 1, ibid, work(1), c16b,&
                                    1, nume)
                        call mesomm(lchout(2), 1, ibid, work(2), c16b,&
                                    1, nume)
!
                        indic1 = work(1)
                        volume = work(2)
!
                        if (indic1 .le. 1.0d4*r8prem()) then
                            indic1 = zero
                        endif
!
                        if (volume .le. r8prem()) then
                            call u2mesk('F', 'UTILITAI3_82', 1, nommai)
                        endif
!
                        valr(2) = indic1/volume
                        vk8(1) = nommai
!
                        else if (motfac.eq.'ENER_ELAS' .or.&
     &                 motfac.eq.'ENER_TOTALE' .or.&
     &                 motfac.eq.'ENER_DISS') then
!
! ---          SOMMATION DE L'ENERGIE ( ELASTIQUE OU TOTALE)
! ---          SUR LE MODELE :
!              -------------
                        if (motfac .eq. 'ENER_TOTALE') then
                            call mesomm(lchout(1), 1, ibid, work(1), c16b,&
                                        1, nume)
                        else if (motfac.eq.'ENER_DISS') then
                            call mesomm(lchout(1), 3, ibid, work(1), c16b,&
                                        1, nume)
                        else
                            call mesomm(lchout(1), 5, ibid, work, c16b,&
                                        1, nume)
                        endif
!
                        if ((compt(1:9).ne.'VMIS_ISOT') .and. (compt( 1:4).ne.'ELAS') .and.&
                            (motfac.ne.'ENER_ELAS')) then
!
                            energi = energi + work(1)
                        else
                            energi = work(1)
                        endif
!
                        valr(2) = energi
                        valr(3) = work(2)
                        valr(4) = work(3)
                        vk8(1) = nommai
                        if (motfac .eq. 'ENER_ELAS') then
! ---    AJOUT INUTILE POUR L INSTANT PUISQUE WORK(4) ET WORK(5)
!        SONT NULS. EN PREVISION DU CALCUL DE L ENERGIE ELASTIQUE
!        DE CISAILLEMENT ET DE COUPLAGE MEMBRANE FLEXION POUR LES
!        PLAQUES EN MECA STATIQUE UNIQUEMENT, SI ON L AUTORISE
!        UN JOUR.
                            valr(5) = work(4)
                            valr(6) = work(5)
                        endif
!
                    endif
!
! ---      ECRITURE DE L'INDICATEUR DANS LA TABLE :
!          --------------------------------------
                    call tbajli(resu, nbparr, noparr, numord, valr,&
                                c16b, vk8, 0)
40              continue
!
                call jedetr('&&PEINGL_MAILLE')
            endif
50      continue
        call jedetr('&&MECHTI.CH_INST_R')
        call detrsd('CHAM_ELEM', chvarc)
        call detrsd('CHAM_ELEM', chvref)
        call jedetr(compor//'.PTMA')
        call jedema()
60  end do
70  continue
    call jedetr(knum)
    call jedetr(kins)
    call jedetr('&&PEINGL.INDIC')
    call jedetr('&&PEINGL.VOLUME')
    call jedetr('&&MEHARM.NUME_HARM')
    if (ivari .eq. 0) then
        call jedetr(chbid)
    endif
!
80  continue
    call jedema()
end subroutine
