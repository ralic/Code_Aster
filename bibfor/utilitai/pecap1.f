      SUBROUTINE PECAP1(CHGEOZ,TEMPEZ,NGI,LISGMA,CT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
      IMPLICIT NONE

C      PECAP1  -- CALCUL DE LA CONSTANTE DE TORSION D'UNE POUTRE
C                 DEFINIE PAR SA SECTION MAILLEE EN ELEMENTS
C                 MASSIFS 2D.

C          .LE DOMAINE SUR-LEQUEL ON TRAVAILLE REPRESENTE LA
C           SECTION DE LA POUTRE MAILLEE AVEC DES ELEMENTS 2D
C           ISOPARAMETRIQUES THERMIQUES (THERMIQUES CAR ON
C           DOIT RESOUDRE UNE EQUATION DE LAPLACE).

C          .LA CONSTANTE DE TORSION CT EST DETERMINEE EN FAISANT
C           LA RESOLUTION DE L'EQUATION 1 :
C                LAPLACIEN(PHI) = -2     DANS LA SECTION
C       AVEC     PHI = 0                 SUR LE CONTOUR DE LA SECTION
C           ON A ALORS CT = 2*INTEGRALE_S(PHI.DS)

C          .DANS LE CAS OU LA SECTION EST TROUEE
C            LE PROBLEME A RESOUDRE DEVIENT :
C                LAPLACIEN(PHI) = -2     DANS LA SECTION
C       AVEC     PHI = 0                 SUR LE CONTOUR EXTERIEUR
C                PHI = T(I)              OU T(I) EST UNE CONSTANTE NE
C                                        DEPENDANT QUE DU CONTOUR
C                                        INTERIEUR I

C            ON PEUT MONTRER QUE CE PROBLEME EST EQUIVALENT A :
C                LAPLACIEN(PHI) = -2     DANS LA SECTION
C       AVEC     PHI = 0                 SUR LE CONTOUR EXTERIEUR
C                ET POUR CHAQUE TROU :
C                PHI EST CONSTANT SUR LE CONTOUR INTERIEUR
C                D(PHI)/DN = 2*AIRE(TROU)/L(TROU)
C                   OU D/DN DESIGNE LA DERIVEE PAR RAPPORT A LA
C                   NORMALE ET L DESIGNE LA LONGUEUR DU BORD DU TROU

C           ON A ALORS CT =   2*INTEGRALE_S(PHI.DS)
C                           + 2*SOMME(PHI(CONTOUR)*AIRE(TROU))
C                              (I=1,NGI)
C                              OU NGI DESIGNE LE NOMBRE DE TROUS

C     OPTION : 'CARA_TORSION'


C   ARGUMENT        E/S  TYPE         ROLE
C    CHGEOZ         IN    K*      COORDONNEES DES CONNECTIVITES
C                                 DANS LE REPERE PRINCIPAL D'INERTIE
C    TEMPEZ         IN    K*      RESULTAT DE TYPE EVOL_THER
C                                 REFERENCANT LE CHAMP DE SCALAIRES
C                                 SOLUTION DE L'EQUATION 1
C    NGI            IN    I       NOMBRE DE TROUS DE LA SECTION
C                                 (= 0 PAR DEFAUT)
C    LISGMA(NGI)    IN    K*      TABLEAU DES NOMS DES GROUP_MA
C                                 DES ELEMENTS DE BORD (SEG2 OU SEG3)
C                                 CONSTITUANT LES CONTOURS DES TROUS
C    CT             OUT   R       CONSTANTE DE TORSION

C.========================= DEBUT DES DECLARATIONS ====================
      INCLUDE 'jeveux.h'
C -----  ARGUMENTS
      CHARACTER*(*) CHGEOZ,TEMPEZ,LISGMA(NGI)
C -----  VARIABLES LOCALES
      INTEGER GD,NBEC
      CHARACTER*1 K1BID
      CHARACTER*8 LPAIN(2),LPAOUT(1)
      CHARACTER*8 TEMPER,NOMAIL
      CHARACTER*8 CRIT,MODELE,K8BID,NOMA
      CHARACTER*19 PRCHNO
      CHARACTER*14 TYPRES
      CHARACTER*19 KNUM,LIGRTH,NOMT19
      CHARACTER*24 LCHIN(2),LCHOUT(1),CHGEOM
      CHARACTER*24 CHTEMP,NOLILI
      REAL*8 WORK(9)
      COMPLEX*16 CBID
C.========================= DEBUT DU CODE EXECUTABLE ==================

C ---- INITIALISATIONS
C      ---------------
C-----------------------------------------------------------------------
      INTEGER I ,IANUEQ ,IAPRNO ,IBID ,IDCOOR ,IDDESC ,IDNOMA
      INTEGER IDVALE ,IERD ,IGR ,IRET ,IRET1 ,IRET2 ,IVAL
      INTEGER JDES ,JGRO ,M ,NBMAIL ,NBNO ,NBORDR ,NEC
      INTEGER NGI ,NUMA ,NUMAIL
      REAL*8 CT ,DEUX ,PREC ,R8B ,STRAP ,TEMP ,UNDEMI
      REAL*8 X1 ,X2 ,XMIN ,Y1 ,Y2 ,YMIN ,ZERO

C-----------------------------------------------------------------------
      ZERO = 0.0D0
      UNDEMI = 0.5D0
      DEUX = 2.0D0
      PREC = 1.0D-3
      CHGEOM = CHGEOZ
      TEMPER = TEMPEZ
      KNUM = '&&PECAP1.NUME_ORD_1'
      CRIT = 'RELATIF'
      XMIN = ZERO
      YMIN = ZERO

      DO 10 I = 1,9
        WORK(I) = ZERO
   10 CONTINUE

C --- ON VERIFIE QUE LE RESULTAT EST DE TYPE EVOL_THER :
C     ------------------------------------------------
      CALL DISMOI('F','TYPE_RESU',TEMPER,'RESULTAT',IBID,TYPRES,IERD)
      IF (TYPRES.NE.'EVOL_THER') THEN
        CALL U2MESS('F','UTILITAI3_50')
      END IF

C --- RECUPERATION DU NOMBRE D'ORDRES DU RESULTAT :
C     -------------------------------------------
      CALL RSUTNU(TEMPER,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
      IF (NBORDR.NE.1) THEN
        CALL U2MESK('F','UTILITAI3_51',1,TEMPER)
      END IF

C --- RECUPERATION DU CHAMP DE TEMPERATURES DU RESULTAT :
C     -------------------------------------------------
      CALL RSEXCH('F',TEMPER,'TEMP',0,CHTEMP,IRET)

C --- RECUPERATION DU NUME_DDL ASSOCIE AU CHAMP DE TEMPERATURES :
C     ---------------------------------------------------------
      CALL DISMOI('F','PROF_CHNO',CHTEMP,'CHAM_NO',IBID,PRCHNO,IERD)

C --- RECUPERATION DU MODELE ASSOCIE AU NUME_DDL  :
C     ------------------------------------------
C     -- QUESTION "POURRIE" :
      CALL DISMOI('F','NOM_MODELE',PRCHNO,'PROF_CHNO',IBID,MODELE,IERD)

C --- RECUPERATION DU LIGREL DU MODELE  :
C     --------------------------------
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IBID,LIGRTH,IERD)

C --- CALCUL POUR CHAQUE ELEMENT DE LA SECTION DE L'INTEGRALE DU
C --- CHAMP DE SCALAIRES SOLUTION DE L'EQUATION DE LAPLACE DESTINEE
C --- A CALCULER LA CONSTANTE DE TORSION :
C     ----------------------------------
      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PTEMPER'
      LCHIN(2) = CHTEMP
      LPAOUT(1) = 'PCASECT'
      LCHOUT(1) = '&&PECAP1.INTEG'

      CALL CALCUL('S','CARA_TORSION',LIGRTH,2,LCHIN,LPAIN,1,LCHOUT,
     &            LPAOUT,'V','OUI')

C --- SOMMATION DES INTEGRALES PRECEDENTES SUR LA SECTION DE LA POUTRE
C --- (I.E. CALCUL DE SOMME_SECTION_POUTRE(PHI.DS)) :
C     ---------------------------------------------
      CALL MESOMM(LCHOUT(1),9,IBID,WORK,CBID,0,IBID)
      CT = DEUX*WORK(1)

      IF (NGI.NE.0) THEN

C ---   RECUPERATION DU NOM DU MAILLAGE :
C       -------------------------------
        CALL JEVEUO(LIGRTH//'.LGRF','L',IDNOMA)
        NOMA = ZK8(IDNOMA)

C ---   RECUPERATION DES COORDONNEES DES NOEUDS DU MAILLAGE :
C       ---------------------------------------------------
        CALL JEVEUO(NOMA//'.COORDO    .VALE','L',IDCOOR)

C ---   RECUPERATION DES COORDONNEES X_MIN ET Y_MIN DU MAILLAGE :
C       -------------------------------------------------------
        CALL JEEXIN(NOMA//'           .LTNT',IRET1)
        IF (IRET1.NE.0) THEN
          CALL LTNOTB(NOMA,'CARA_GEOM',NOMT19)
          CALL TBLIVA(NOMT19,0,' ',IBID,R8B,CBID,K8BID,K8BID,R8B,
     &                'X_MIN',K8BID,IBID,XMIN,CBID,K8BID,IRET2)
          IF (IRET2.NE.0) CALL U2MESS('F','MODELISA2_13')
          CALL TBLIVA(NOMT19,0,' ',IBID,R8B,CBID,K8BID,K8BID,R8B,
     &                'Y_MIN',K8BID,IBID,YMIN,CBID,K8BID,IRET2)
          IF (IRET2.NE.0) CALL U2MESS('F','MODELISA2_13')
        ELSE
          CALL U2MESS('F','UTILITAI3_53')
        END IF

C --- RECUPERATION DE LA TEMPERATURE AU PREMIER NOEUD DU GROUP_MA :
C     ===========================================================

C ---   RECUPERATION DU TABLEAU DES VALEURS DU CHAMP DE TEMPERATURES :
C       ------------------------------------------------------------
        CALL JEVEUO(CHTEMP(1:19)//'.VALE','L',IDVALE)

C ---   RECUPERATION DU DESCRIPTEUR DU CHAMP DE TEMPERATURES :
C       ----------------------------------------------------
        CALL JEVEUO(CHTEMP(1:19)//'.DESC','L',IDDESC)

C ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU CHAMP (C'EST TEMP_R) :
C       ------------------------------------------------------------
        GD = ZI(IDDESC+1-1)

C ---   NOMBRE D'ENTIERS CODES ASSOCIE A LA GRANDEUR :
C       --------------------------------------------
        NEC = NBEC(GD)
        CALL JENUNO(JEXNUM(PRCHNO//'.LILI',1),NOLILI)
        CALL JELIRA(JEXNUM(PRCHNO//'.PRNO',1),'LONMAX',IBID,K1BID)

C ---   RECUPERATION DU PRNO DU CHAMP DE TEMPERATURES :
C       ---------------------------------------------
        CALL JEVEUO(JEXNUM(PRCHNO//'.PRNO',1),'L',IAPRNO)

C ---   RECUPERATION DU TABLEAU DES NUMEROS D'EQUATION :
C       ----------------------------------------------
        CALL JEVEUO(PRCHNO//'.NUEQ','L',IANUEQ)

C ---   CALCUL POUR CHAQUE TROU DE SA SURFACE S
C ---   LA CONSTANTE DE TORSION CALCULEE PRECEDEMENT VA ETRE
C ---   AUGMENTEE DE 2*TEMP(1)*S POUR CHAQUE TROU
C ---   OU TEMP(1) EST LA TEMPERATURE AU PREMIER NOEUD DU BORD
C ---   BOUCLE SUR LES CONTOURS INTERIEURS :
C       ----------------------------------
        DO 30 IGR = 1,NGI

          STRAP = ZERO

C ---   RECUPERATION DES MAILLES DU GROUP_MA :
C       ------------------------------------
          CALL JEVEUO(JEXNOM(NOMA//'.GROUPEMA',LISGMA(IGR) (1:8)),'L',
     &                JGRO)

C ---   RECUPERATION DU NOMBRE DE MAILLES DU GROUP_MA :
C       ---------------------------------------------
          CALL JELIRA(JEXNOM(NOMA//'.GROUPEMA',LISGMA(IGR)),'LONUTI',
     &                NBMAIL,K1BID)

C ---   NUMERO DE LA PREMIERE MAILLE  DU BORD :
C       -------------------------------------
          NUMAIL = ZI(JGRO)

C ---   NOM DE LA PREMIERE MAILLE  DU BORD :
C       ----------------------------------
          CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),NOMAIL)
          CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),NUMA)

C ---   RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C       -------------------------------------------
          CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA),'L',JDES)

C ---   POINTEUR DANS LE TABLEAU DES NUMEROS D'EQUATIONS ASSOCIE
C ---   AU PREMIER NOEUD :
C       ----------------
          IVAL = ZI(IAPRNO-1+ (ZI(JDES)-1)* (NEC+2)+1)

C ---   TEMPERATURE AU PREMIER NOEUD DE LA PREMIERE MAILLE DU CONTOUR
C ---   INTERIEUR COURANT :
C       -----------------
          TEMP = ZR(IDVALE-1+ZI(IANUEQ-1+IVAL))

C ---   BOUCLE SUR LES MAILLES (SEG2 OU SEG3) CONSTITUANT LE CONTOUR
C ---   INTERIEUR COURANT :
C       -----------------
          DO 20 M = 1,NBMAIL

C ---     NUMERO DE LA MAILLE :
C         -------------------
            NUMAIL = ZI(JGRO+M-1)

C ---     NOM DE LA MAILLE :
C         ----------------
            CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),NOMAIL)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),NUMA)

C ---     NOMBRE DE CONNECTIVITES DE LA MAILLE :
C         ------------------------------------
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMA),'LONMAX',NBNO,
     &                  K1BID)

C ---     RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C         -------------------------------------------
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA),'L',JDES)

C ---     COORDONNEEES DES NOEUDS SOMMETS DE LA MAILLE DANS
C ---     UN REPERE OU LES AXES SONT TOUJOURS X ET Y MAIS DONT
C ---     L'ORIGINE SE SITUE AU POINT DE COORDONNEES (XMIN,YMIN)
C ---     OU XMIN ET YMIN SONT LES COORDONNEES LES PLUS 'FAIBLES'
C ---     DE NOEUDS DU MAILLAGE : C'EST POUR POUVOIR APPLIQUER
C ---     SANS ERREUR LA FORMULE DONNANT LA SURFACE DETERMINEE
C ---     PAR LE SEGMENT ET SA PROJECTION SUR L'AXE Y :
C         -------------------------------------------
            X1 = ZR(IDCOOR+3* (ZI(JDES)-1)+1-1) - XMIN
            Y1 = ZR(IDCOOR+3* (ZI(JDES)-1)+2-1) - YMIN
            X2 = ZR(IDCOOR+3* (ZI(JDES+1)-1)+1-1) - XMIN
            Y2 = ZR(IDCOOR+3* (ZI(JDES+1)-1)+2-1) - YMIN

C ---    AIRE DU TRAPEZE DETERMINE PAR L'ELEMENT SEGMENT COURANT
C ---    ET PAR SA PROJECTION SUR L'AXE Y :
C        --------------------------------
            STRAP = STRAP + UNDEMI* (X1+X2)* (Y2-Y1)
   20     CONTINUE

C ---  MISE A JOUR DE LA CONSTANTE DE TORSION, ELLE EST AUGMENTEE
C ---  DE 2*AIRE(TROU)*TEMP(1) :
C      -----------------------
          CT = CT + DEUX*TEMP*ABS(STRAP)
   30   CONTINUE

      END IF

      CALL DETRSD('CHAMP_GD','&&PECAP1.INTEG')
C.============================ FIN DE LA ROUTINE ======================
      END
