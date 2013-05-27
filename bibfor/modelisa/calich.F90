subroutine calich(chargz)
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
!.======================================================================
    implicit none
!
!       CALICH -- TRAITEMENT DU MOT FACTEUR LIAISON_CHAMNO
!
!      TRAITEMENT DU MOT FACTEUR LIAISON_CHAMNO DE AFFE_CHAR_MECA
!      CE MOT FACTEUR PERMET DE DEFINIR UNE RELATION LINEAIRE ENTRE
!      LES DDLS DES NOEUDS D'UN MODELE DONT LES COEFFICIENTS SONT
!      LES VALEURS DES COMPOSANTES DU CHAM_NO DONNE APRES LE MOT CLE :
!      CHAM_NO.
!      LA VALEUR DU SECOND MEMBRE EST DONNEE APRES LE MOT CLE
!      COEF_IMPO (C'EST UN REEL).
!      ON NE PREND EN COMPTE QUE LES COEFFICIENTS NON NULS DU
!      CHAM_NO DANS LA RELATION LINEAIRE.
!
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE EST ENRICHIE
!                                   DE LA RELATION LINEAIRE DECRITE
!                                   CI-DESSUS.
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/afrela.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!
! -----  ARGUMENTS
    character(len=*) :: chargz
! ------ VARIABLES LOCALES
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=4) :: tych, typval, typcoe
    character(len=8) :: k8bid, chamno, noma, nomcmp, nomnoe, betaf
    character(len=8) :: charge, poslag, nomgd
    character(len=16) :: motfac
    character(len=19) :: lisrel, cham19, prchno
    character(len=24) :: noeuma
!
    complex(kind=8) :: betac
    integer :: iarg
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: i, ibid, idcoec, idcoer, idddl, ideeq, idimen
    integer :: idirec, idnoeu, idvale, iequa, ier, ino, inocmp
    integer :: iocc, iret, k, nb, nbcmp, nbec, nbnoeu
    integer :: nbterm, nequa, nliai, nucmp
    real(kind=8) :: beta, vale, zero
!-----------------------------------------------------------------------
    call jemarq()
!
    motfac = 'LIAISON_CHAMNO'
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 40
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
! --- BETA, BETAC ET BETAF SONT LES VALEURS DU SECOND MEMBRE DE LA
! --- RELATION LINEAIRE SUIVANT QUE C'EST UN REEL, UN COMPLEXE OU
! --- UNE FONCTION, DANS NOTRE CAS C'EST UN REEL
!
    beta = zero
    betac = (0.0d0,0.0d0)
    betaf = '&FOZERO'
!
    cham19 = '                   '
    charge = chargz
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DE LA RELATION
!
    typval = 'REEL'
!
! --- TYPE DES VALEURS DES COEFFICIENTS
!
    typcoe = 'REEL'
!
! --- NOM DE LA LISTE_RELA
!
    lisrel = '&CALICH.RLLISTE'
!
! --- BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR LIAISON_CHAMNO :
!     -------------------------------------------------------
    do 30 iocc = 1, nliai
!
! ---   ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! ---   APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! ---   ASSEMBLEE :
! ---   SI OUI TYPLAG = '22'
! ---   SI NON TYPLAG = '12'
!
        call getvtx(motfac, 'NUME_LAGR', iocc, iarg, 1,&
                    poslag, ibid)
        if (poslag .eq. 'APRES') then
            typlag = '22'
        else
            typlag = '12'
        endif
!
! ---   RECUPERATION DU CHAMNO
!       ----------------------
        call getvid(motfac, 'CHAM_NO', iocc, iarg, 1,&
                    chamno, nb)
        if (nb .eq. 0) then
            call u2mess('F', 'MODELISA2_98')
        endif
!
        cham19(1:8) = chamno
!
! ---   VERIFICATION DE L'EXISTENCE DU CHAMNO
!       -------------------------------------
        call jeexin(cham19//'.VALE', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'MODELISA2_99')
        endif
!
! ---   VERIFICATION DU TYPE DU CHAMP
!       -----------------------------
        call dismoi('F', 'TYPE_CHAMP', chamno, 'CHAM_NO', ibid,&
                    tych, ier)
!
        if (tych .ne. 'NOEU') then
            call u2mess('F', 'MODELISA3_1')
        endif
!
! ---   RECUPERATION DE LA VALEUR DU SECOND MEMBRE DE LA RELATION
! ---   LINEAIRE
!       --------
        call getvr8(motfac, 'COEF_IMPO', iocc, iarg, 1,&
                    beta, nb)
        if (nb .eq. 0) then
            call u2mess('F', 'MODELISA3_2')
        endif
!
! ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU CHAMNO :
!       ----------------------------------------------
        call dismoi('F', 'NOM_GD', chamno, 'CHAM_NO', ibid,&
                    nomgd, ier)
!
! ---   RECUPERATION DU NOMBRE DE MOTS SUR-LESQUELS SONT CODEES LES
! ---   LES INCONNUES ASSOCIEES A LA GRANDEUR DE NOM NOMGD
!       --------------------------------------------------
        call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nbec,&
                    k8bid, ier)
        if (nbec .gt. 10) then
            call u2mesk('F', 'MODELISA2_87', 1, nomgd)
        endif
!
! ---   RECUPERATION DU MAILLAGE ASSOCIE AU CHAM_NO
!       -------------------------------------------
        call dismoi('F', 'NOM_MAILLA', chamno, 'CHAM_NO', ibid,&
                    noma, ier)
!
! ---   RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
!       --------------------------------------------
        call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoeu,&
                    k8bid, ier)
!
! ---   RECUPERATION DU NOMBRE DE TERMES DU CHAM_NO
!       -------------------------------------------
        call dismoi('F', 'NB_EQUA', chamno, 'CHAM_NO', nequa,&
                    k8bid, ier)
!
! ---   RECUPERATION DU PROF_CHNO DU CHAM_NO
!       ------------------------------------
        call dismoi('F', 'PROF_CHNO', chamno, 'CHAM_NO', ibid,&
                    prchno, ier)
!
! ---   RECUPERATION DU NOMBRE DE COMPOSANTES ASSOCIEES A LA LA GRANDEUR
!       ----------------------------------------------------------------
        call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nbcmp, k1bid)
!
! ---   RECUPERATION DU NOM DES COMPOSANTES ASSOCIEES A LA LA GRANDEUR
!       --------------------------------------------------------------
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', inocmp)
!
! ---   RECUPERATION DU .VALE DU CHAM_NO
!       --------------------------------
        call jeveuo(cham19//'.VALE', 'E', idvale)
!
! ---   RECUPERATION DU .DEEQ DU PROF_CHNO
!       ----------------------------------
        call jeveuo(prchno//'.DEEQ', 'L', ideeq)
!
! ---   DETERMINATION DU NOMBRE DE COMPOSANTES NON-NULLES DU CHAM_NO
!       ------------------------------------------------------------
        k = 0
        do 10 i = 1, nequa
            if (zr(idvale+i-1) .ne. zero) then
                k = k + 1
            endif
10      continue
!
        nbterm = k
!
! ---   CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! ---   DE LA LISTE_RELA
!       ----------------
! ---     VECTEUR DU NOM DES NOEUDS
        call wkvect('&&CALICH.LISNO', 'V V K8', nbterm, idnoeu)
! ---     VECTEUR DU NOM DES DDLS
        call wkvect('&&CALICH.LISDDL', 'V V K8', nbterm, idddl)
! ---      VECTEUR DES COEFFICIENTS REELS
        call wkvect('&&CALICH.COER', 'V V R', nbterm, idcoer)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
        call wkvect('&&CALICH.COEC', 'V V C', nbterm, idcoec)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
        call wkvect('&&CALICH.DIRECT', 'V V R', 3*nbterm, idirec)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
        call wkvect('&&CALICH.DIME', 'V V I', nbterm, idimen)
!
! ---   COLLECTION DES NOMS DES NOEUDS DU MAILLAGE
!       ------------------------------------------
        noeuma = noma//'.NOMNOE'
!
! ---   AFFECTATION DES TABLEAUX DE TRAVAIL :
!       -----------------------------------
        k = 0
!
! ---   BOUCLE SUR LES TERMES DU CHAM_NO
!
        do 20 iequa = 1, nequa
!
! ---     INO  : NUMERO DU NOEUD INO CORRESPONDANT AU DDL IEQUA
!
            ino = zi(ideeq+2* (iequa-1)+1-1)
!
! ---     NUCMP  : NUMERO DE COMPOSANTE CORRESPONDANTE AU DDL IEQUA
!
            nucmp = zi(ideeq+2* (iequa-1)+2-1)
!
! ---     ON NE PREND PAS EN COMPTE LES MULTIPLICATEURS DE LAGRANGE
! ---     (CAS OU NUCMP < 0)
!
            if (nucmp .gt. 0) then
!
! ---       RECUPERATION DU NOM DU NOEUD INO
!
                call jenuno(jexnum(noeuma, ino), nomnoe)
!
                vale = zr(idvale+iequa-1)
!
                if (vale .ne. zero) then
                    k = k + 1
                    nomcmp = zk8(inocmp+nucmp-1)
                    zk8(idnoeu+k-1) = nomnoe
                    zk8(idddl+k-1) = nomcmp
                    zr(idcoer+k-1) = vale
                endif
            endif
!
20      continue
!
        nbterm = k
!
! ---   AFFECTATION DE LA RELATION A LA LISTE_RELA  :
!       ------------------------------------------
        call afrela(zr(idcoer), zc(idcoec), zk8(idddl), zk8(idnoeu), zi(idimen),&
                    zr(idirec), nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
!
! ---   MENAGE :
!       ------
        call jedetr('&&CALICH.LISNO')
        call jedetr('&&CALICH.LISDDL')
        call jedetr('&&CALICH.COER')
        call jedetr('&&CALICH.COEC')
        call jedetr('&&CALICH.DIRECT')
        call jedetr('&&CALICH.DIME')
!
30  end do
!
! --- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ----------------------------------------
    call aflrch(lisrel, charge)
!
! --- MENAGE :
!     ------
    call jedetr(lisrel)
!
40  continue
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
