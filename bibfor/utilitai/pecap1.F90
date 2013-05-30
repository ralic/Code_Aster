subroutine pecap1(chgeoz, tempez, ngi, lisgma, ct)
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
!.======================================================================
    implicit none
!
!      PECAP1  -- CALCUL DE LA CONSTANTE DE TORSION D'UNE POUTRE
!                 DEFINIE PAR SA SECTION MAILLEE EN ELEMENTS
!                 MASSIFS 2D.
!
!          .LE DOMAINE SUR-LEQUEL ON TRAVAILLE REPRESENTE LA
!           SECTION DE LA POUTRE MAILLEE AVEC DES ELEMENTS 2D
!           ISOPARAMETRIQUES THERMIQUES (THERMIQUES CAR ON
!           DOIT RESOUDRE UNE EQUATION DE LAPLACE).
!
!          .LA CONSTANTE DE TORSION CT EST DETERMINEE EN FAISANT
!           LA RESOLUTION DE L'EQUATION 1 :
!                LAPLACIEN(PHI) = -2     DANS LA SECTION
!       AVEC     PHI = 0                 SUR LE CONTOUR DE LA SECTION
!           ON A ALORS CT = 2*INTEGRALE_S(PHI.DS)
!
!          .DANS LE CAS OU LA SECTION EST TROUEE
!            LE PROBLEME A RESOUDRE DEVIENT :
!                LAPLACIEN(PHI) = -2     DANS LA SECTION
!       AVEC     PHI = 0                 SUR LE CONTOUR EXTERIEUR
!                PHI = T(I)              OU T(I) EST UNE CONSTANTE NE
!                                        DEPENDANT QUE DU CONTOUR
!                                        INTERIEUR I
!
!            ON PEUT MONTRER QUE CE PROBLEME EST EQUIVALENT A :
!                LAPLACIEN(PHI) = -2     DANS LA SECTION
!       AVEC     PHI = 0                 SUR LE CONTOUR EXTERIEUR
!                ET POUR CHAQUE TROU :
!                PHI EST CONSTANT SUR LE CONTOUR INTERIEUR
!                D(PHI)/DN = 2*AIRE(TROU)/L(TROU)
!                   OU D/DN DESIGNE LA DERIVEE PAR RAPPORT A LA
!                   NORMALE ET L DESIGNE LA LONGUEUR DU BORD DU TROU
!
!           ON A ALORS CT =   2*INTEGRALE_S(PHI.DS)
!                           + 2*SOMME(PHI(CONTOUR)*AIRE(TROU))
!                              (I=1,NGI)
!                              OU NGI DESIGNE LE NOMBRE DE TROUS
!
!     OPTION : 'CARA_TORSION'
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    CHGEOZ         IN    K*      COORDONNEES DES CONNECTIVITES
!                                 DANS LE REPERE PRINCIPAL D'INERTIE
!    TEMPEZ         IN    K*      RESULTAT DE TYPE EVOL_THER
!                                 REFERENCANT LE CHAMP DE SCALAIRES
!                                 SOLUTION DE L'EQUATION 1
!    NGI            IN    I       NOMBRE DE TROUS DE LA SECTION
!                                 (= 0 PAR DEFAUT)
!    LISGMA(NGI)    IN    K*      TABLEAU DES NOMS DES GROUP_MA
!                                 DES ELEMENTS DE BORD (SEG2 OU SEG3)
!                                 CONSTITUANT LES CONTOURS DES TROUS
!    CT             OUT   R       CONSTANTE DE TORSION
!
!.========================= DEBUT DES DECLARATIONS ====================
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/nbec.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
! -----  ARGUMENTS
    character(len=*) :: chgeoz, tempez, lisgma(ngi)
! -----  VARIABLES LOCALES
    integer :: gd
    character(len=1) :: k1bid
    character(len=8) :: lpain(2), lpaout(1)
    character(len=8) :: temper, nomail
    character(len=8) :: crit, modele, k8bid, noma
    character(len=19) :: prchno
    character(len=14) :: typres
    character(len=19) :: knum, ligrth, nomt19
    character(len=24) :: lchin(2), lchout(1), chgeom
    character(len=24) :: chtemp, nolili
    real(kind=8) :: work(9)
    complex(kind=8) :: cbid
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
!-----------------------------------------------------------------------
    integer :: i, ianueq, iaprno, ibid, idcoor, iddesc, idnoma
    integer :: idvale, ierd, igr, iret, iret1, iret2, ival
    integer :: jdes, jgro, m, nbmail, nbno, nbordr, nec
    integer :: ngi, numa, numail
    real(kind=8) :: ct, deux, prec, r8b, strap, temp, undemi
    real(kind=8) :: x1, x2, xmin, y1, y2, ymin, zero
!
!-----------------------------------------------------------------------
    zero = 0.0d0
    undemi = 0.5d0
    deux = 2.0d0
    prec = 1.0d-3
    chgeom = chgeoz
    temper = tempez
    knum = '&&PECAP1.NUME_ORD_1'
    crit = 'RELATIF'
    xmin = zero
    ymin = zero
!
    do 10 i = 1, 9
        work(i) = zero
10  end do
!
! --- ON VERIFIE QUE LE RESULTAT EST DE TYPE EVOL_THER :
!     ------------------------------------------------
    call dismoi('F', 'TYPE_RESU', temper, 'RESULTAT', ibid,&
                typres, ierd)
    if (typres .ne. 'EVOL_THER') then
        call u2mess('F', 'UTILITAI3_50')
    endif
!
! --- RECUPERATION DU NOMBRE D'ORDRES DU RESULTAT :
!     -------------------------------------------
    call rsutnu(temper, ' ', 0, knum, nbordr,&
                prec, crit, iret)
    if (nbordr .ne. 1) then
        call u2mesk('F', 'UTILITAI3_51', 1, temper)
    endif
!
! --- RECUPERATION DU CHAMP DE TEMPERATURES DU RESULTAT :
!     -------------------------------------------------
    call rsexch('F', temper, 'TEMP', 0, chtemp,&
                iret)
!
! --- RECUPERATION DU NUME_DDL ASSOCIE AU CHAMP DE TEMPERATURES :
!     ---------------------------------------------------------
    call dismoi('F', 'PROF_CHNO', chtemp, 'CHAM_NO', ibid,&
                prchno, ierd)
!
! --- RECUPERATION DU MODELE ASSOCIE AU NUME_DDL  :
!     ------------------------------------------
!     -- QUESTION "POURRIE" :
    call dismoi('F', 'NOM_MODELE', prchno, 'PROF_CHNO', ibid,&
                modele, ierd)
!
! --- RECUPERATION DU LIGREL DU MODELE  :
!     --------------------------------
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrth, ierd)
!
! --- CALCUL POUR CHAQUE ELEMENT DE LA SECTION DE L'INTEGRALE DU
! --- CHAMP DE SCALAIRES SOLUTION DE L'EQUATION DE LAPLACE DESTINEE
! --- A CALCULER LA CONSTANTE DE TORSION :
!     ----------------------------------
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PTEMPER'
    lchin(2) = chtemp
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&PECAP1.INTEG'
!
    call calcul('S', 'CARA_TORSION', ligrth, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
! --- SOMMATION DES INTEGRALES PRECEDENTES SUR LA SECTION DE LA POUTRE
! --- (I.E. CALCUL DE SOMME_SECTION_POUTRE(PHI.DS)) :
!     ---------------------------------------------
    call mesomm(lchout(1), 9, ibid, work, cbid,&
                0, ibid)
    ct = deux*work(1)
!
    if (ngi .ne. 0) then
!
! ---   RECUPERATION DU NOM DU MAILLAGE :
!       -------------------------------
        call jeveuo(ligrth//'.LGRF', 'L', idnoma)
        noma = zk8(idnoma)
!
! ---   RECUPERATION DES COORDONNEES DES NOEUDS DU MAILLAGE :
!       ---------------------------------------------------
        call jeveuo(noma//'.COORDO    .VALE', 'L', idcoor)
!
! ---   RECUPERATION DES COORDONNEES X_MIN ET Y_MIN DU MAILLAGE :
!       -------------------------------------------------------
        call jeexin(noma//'           .LTNT', iret1)
        if (iret1 .ne. 0) then
            call ltnotb(noma, 'CARA_GEOM', nomt19)
            call tbliva(nomt19, 0, ' ', ibid, r8b,&
                        cbid, k8bid, k8bid, r8b, 'X_MIN',&
                        k8bid, ibid, xmin, cbid, k8bid,&
                        iret2)
            if (iret2 .ne. 0) call u2mess('F', 'MODELISA2_13')
            call tbliva(nomt19, 0, ' ', ibid, r8b,&
                        cbid, k8bid, k8bid, r8b, 'Y_MIN',&
                        k8bid, ibid, ymin, cbid, k8bid,&
                        iret2)
            if (iret2 .ne. 0) call u2mess('F', 'MODELISA2_13')
        else
            call u2mess('F', 'UTILITAI3_53')
        endif
!
! --- RECUPERATION DE LA TEMPERATURE AU PREMIER NOEUD DU GROUP_MA :
!     ===========================================================
!
! ---   RECUPERATION DU TABLEAU DES VALEURS DU CHAMP DE TEMPERATURES :
!       ------------------------------------------------------------
        call jeveuo(chtemp(1:19)//'.VALE', 'L', idvale)
!
! ---   RECUPERATION DU DESCRIPTEUR DU CHAMP DE TEMPERATURES :
!       ----------------------------------------------------
        call jeveuo(chtemp(1:19)//'.DESC', 'L', iddesc)
!
! ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU CHAMP (C'EST TEMP_R) :
!       ------------------------------------------------------------
        gd = zi(iddesc+1-1)
!
! ---   NOMBRE D'ENTIERS CODES ASSOCIE A LA GRANDEUR :
!       --------------------------------------------
        nec = nbec(gd)
        call jenuno(jexnum(prchno//'.LILI', 1), nolili)
        call jelira(jexnum(prchno//'.PRNO', 1), 'LONMAX', ibid, k1bid)
!
! ---   RECUPERATION DU PRNO DU CHAMP DE TEMPERATURES :
!       ---------------------------------------------
        call jeveuo(jexnum(prchno//'.PRNO', 1), 'L', iaprno)
!
! ---   RECUPERATION DU TABLEAU DES NUMEROS D'EQUATION :
!       ----------------------------------------------
        call jeveuo(prchno//'.NUEQ', 'L', ianueq)
!
! ---   CALCUL POUR CHAQUE TROU DE SA SURFACE S
! ---   LA CONSTANTE DE TORSION CALCULEE PRECEDEMENT VA ETRE
! ---   AUGMENTEE DE 2*TEMP(1)*S POUR CHAQUE TROU
! ---   OU TEMP(1) EST LA TEMPERATURE AU PREMIER NOEUD DU BORD
! ---   BOUCLE SUR LES CONTOURS INTERIEURS :
!       ----------------------------------
        do 30 igr = 1, ngi
!
            strap = zero
!
! ---   RECUPERATION DES MAILLES DU GROUP_MA :
!       ------------------------------------
            call jeveuo(jexnom(noma//'.GROUPEMA', lisgma(igr)), 'L', jgro)
!
! ---   RECUPERATION DU NOMBRE DE MAILLES DU GROUP_MA :
!       ---------------------------------------------
            call jelira(jexnom(noma//'.GROUPEMA', lisgma(igr)), 'LONUTI', nbmail, k1bid)
!
! ---   NUMERO DE LA PREMIERE MAILLE  DU BORD :
!       -------------------------------------
            numail = zi(jgro)
!
! ---   NOM DE LA PREMIERE MAILLE  DU BORD :
!       ----------------------------------
            call jenuno(jexnum(noma//'.NOMMAI', numail), nomail)
            call jenonu(jexnom(noma//'.NOMMAI', nomail), numa)
!
! ---   RECUPERATION DES CONNECTIVITES DE LA MAILLE :
!       -------------------------------------------
            call jeveuo(jexnum(noma//'.CONNEX', numa), 'L', jdes)
!
! ---   POINTEUR DANS LE TABLEAU DES NUMEROS D'EQUATIONS ASSOCIE
! ---   AU PREMIER NOEUD :
!       ----------------
            ival = zi(iaprno-1+ (zi(jdes)-1)* (nec+2)+1)
!
! ---   TEMPERATURE AU PREMIER NOEUD DE LA PREMIERE MAILLE DU CONTOUR
! ---   INTERIEUR COURANT :
!       -----------------
            temp = zr(idvale-1+zi(ianueq-1+ival))
!
! ---   BOUCLE SUR LES MAILLES (SEG2 OU SEG3) CONSTITUANT LE CONTOUR
! ---   INTERIEUR COURANT :
!       -----------------
            do 20 m = 1, nbmail
!
! ---     NUMERO DE LA MAILLE :
!         -------------------
                numail = zi(jgro+m-1)
!
! ---     NOM DE LA MAILLE :
!         ----------------
                call jenuno(jexnum(noma//'.NOMMAI', numail), nomail)
                call jenonu(jexnom(noma//'.NOMMAI', nomail), numa)
!
! ---     NOMBRE DE CONNECTIVITES DE LA MAILLE :
!         ------------------------------------
                call jelira(jexnum(noma//'.CONNEX', numa), 'LONMAX', nbno, k1bid)
!
! ---     RECUPERATION DES CONNECTIVITES DE LA MAILLE :
!         -------------------------------------------
                call jeveuo(jexnum(noma//'.CONNEX', numa), 'L', jdes)
!
! ---     COORDONNEEES DES NOEUDS SOMMETS DE LA MAILLE DANS
! ---     UN REPERE OU LES AXES SONT TOUJOURS X ET Y MAIS DONT
! ---     L'ORIGINE SE SITUE AU POINT DE COORDONNEES (XMIN,YMIN)
! ---     OU XMIN ET YMIN SONT LES COORDONNEES LES PLUS 'FAIBLES'
! ---     DE NOEUDS DU MAILLAGE : C'EST POUR POUVOIR APPLIQUER
! ---     SANS ERREUR LA FORMULE DONNANT LA SURFACE DETERMINEE
! ---     PAR LE SEGMENT ET SA PROJECTION SUR L'AXE Y :
!         -------------------------------------------
                x1 = zr(idcoor+3* (zi(jdes)-1)+1-1) - xmin
                y1 = zr(idcoor+3* (zi(jdes)-1)+2-1) - ymin
                x2 = zr(idcoor+3* (zi(jdes+1)-1)+1-1) - xmin
                y2 = zr(idcoor+3* (zi(jdes+1)-1)+2-1) - ymin
!
! ---    AIRE DU TRAPEZE DETERMINE PAR L'ELEMENT SEGMENT COURANT
! ---    ET PAR SA PROJECTION SUR L'AXE Y :
!        --------------------------------
                strap = strap + undemi* (x1+x2)* (y2-y1)
20          continue
!
! ---  MISE A JOUR DE LA CONSTANTE DE TORSION, ELLE EST AUGMENTEE
! ---  DE 2*AIRE(TROU)*TEMP(1) :
!      -----------------------
            ct = ct + deux*temp*abs(strap)
30      continue
!
    endif
!
    call detrsd('CHAMP_GD', '&&PECAP1.INTEG')
!.============================ FIN DE LA ROUTINE ======================
end subroutine
