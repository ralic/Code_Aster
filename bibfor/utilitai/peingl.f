      SUBROUTINE PEINGL(RESU,MODELE,MATE,CARA,NCHAR,LCHAR,NH,NBOCC,
     &                  OPTIOZ)
      IMPLICIT   NONE
      INTEGER NCHAR,NH,NBOCC
      CHARACTER*(*) RESU,MODELE,MATE,CARA,LCHAR(1),OPTIOZ
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20

C      PEINGL  --  OPERATEUR POST_ELEM
C                  TRAITEMENT DU MOT-FACTEUR "INDIC_ENER"
C                          ET DU MOT-FACTEUR "INDIC_SEUIL"

C                  CALCUL DES INDICATEURS GLOBAUX DE
C                  DE PERTE DE PROPORTIONNALITE DU CHARGEMENT.

C -----------------------------------------------------------------

C           -POUR L'OPTION INDIC_ENER, ON CALCULE L'INDICATEUR
C            GLOBAL ENERGETIQUE DETERMINE PAR L'EXPRESSION
C            SUIVANTE :
C            IE = (SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV)/V

C        OU  .OMEGA EST LA DENSITE D'ENERGIE TOTALE
C            (I.E. OMEGA = SOMME_0->T(SIGMA:D(EPS)/DT).DTAU
C            .PSI EST LA DENSITE D'ENERGIE ELASTIQUE 'TOTALE'
C            (I.E. ASSOCIEE A LA COURBE DE TRACTION SI ON
C                  CONSIDERAIT LE MATERIAU ELASTIQUE NON-LINEAIRE)
C            .V EST LE VOLUME DU GROUPE DE MAILLES TRAITE

C -----------------------------------------------------------------

C           -POUR L'OPTION INDIC_SEUIL, ON CALCULE L'INDICATEUR
C            GLOBAL  DETERMINE PAR L'EXPRESSION SUIVANTE :

C   IS = (SOMME_DOMAINE(1 - ((SIG-X):EPS_PLAST)/((SIG_Y+R)*P)).DV)/V

C        OU  .SIG       EST LE TENSEUR DES CONTRAINTES
C            .X         EST LE TENSEUR DE RAPPEL
C            .EPS_PLAST EST LE TENSEUR DES DEFORMATIONS PLASTIQUES
C            .SIG_Y     EST LA LIMITE D'ELASTICITE
C            .R         EST LA FONCTION D'ECROUISSAGE
C            .P         EST LA DEFORMATION PLASTIQUE CUMULEE
C            .V EST LE VOLUME DU GROUPE DE MAILLES TRAITE
C -----------------------------------------------------------------

C  OPTION ENER_ELAS : CALCUL DE L'ENERGIE DE DEFORMATION ELASTIQUE
C  ================   DETERMINEE PAR L'EXPRESSION SUIVANTE :

C   ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)

C        OU  .SIG       EST LE TENSEUR DES CONTRAINTES
C            .D         EST LE TENSEUR DE HOOKE

C -----------------------------------------------------------------

C  OPTION ENER_TOTALE : CALCUL DE L'ENERGIE DE DEFORMATION TOTALE
C  ==================   DETERMINEE PAR L'EXPRESSION SUIVANTE :

C   ENER_TOTALE =  ENELAS + EPLAS

C          AVEC : ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)
C                 ENELAS EST L'ENERGIE DE DEFORMATION ELASTIQUE

C           OU  .SIG       EST LE TENSEUR DES CONTRAINTES
C               .D         EST LE TENSEUR DE HOOKE

C          ET   : EPLAS = SOMME_VOLUME((R(P))*D(P))
C                 EPLAS EST L'ENERGIE DE DEFORMATION PLASTIQUE

C           OU  .P         EST LA DEFORMATION PLASTIQUE CUMULEE
C           ET   R(P) EST CALCULE POUR LES COMPORTEMENTS SUIVANTS :
C                      .VMIS_ISOT_LINE
C                      .VMIS_ISOT_TRAC
C                      .VMIS_ECMI_LINE
C                      .VMIS_ECMI_TRAC
C                      .VMIS_CINE_LINE
C                      .VISC_CIN1_CHAB
C                      .VISC_CIN2_CHAB

C          POUR LES AUTRES COMPORTEMENTS ON S'ARRETE EN ERREUR FATALE

C -----------------------------------------------------------------

C   ARGUMENT        E/S  TYPE         ROLE
C    RESU           VAR    K*      TABLE EN SORTIE DE LA COMMANDE
C    MODELE         IN     K*      NOM DU MODELE SUR-LEQUEL ON FAIT
C                                  LE CALCUL
C    MATE           IN     K*      NOM DU CHAMP MATERIAU
C    CARA           IN     K*      NOM DU CHAMP DES CARA_ELEM
C    NCHAR          IN     I       NOMBRE DE  CHARGES
C    LCHAR(1)       IN     K*      LISTE  DES CHARGES
C    NH             IN     I       NUMERO D'HARMONIQUE DE FOURIER
C    NBOCC          IN     I       NOMBRE D'OCCURENCES DU MOT-FACTEUR
C                                  INDIC_ENER
C    OPTIOZ         IN     K*      NOM DE L'OPTION "INDIC_ENER"
C                                              OU  "INDIC_SEUIL"
C                                              OU  "ENER_ELAS"
C                                              OU  "ENER_TOTALE"
C                                              OU  "ENER_DISS"

C.========================= DEBUT DES DECLARATIONS ====================
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C -----  VARIABLES LOCALES
      INTEGER NBPARR,NR,NP,NC,IRET,JORD,NBORDR,JINS,IORD,IAINST,NUMORD,
     &        NBIN,NT,NM,NG,IBID,NBGRMA,JGR,IG,NBMA,JAD,NBMAIL,JMA,IM,
     &        IOCC,NUME,IFM,NIV,NBOUT,NUMORM,IDESC,NGDMAX,NCMPMX,IVALE,
     &        IPTMA,IGD,IDEBGD,DG,IMA,ICONEX,NBNO,NEC,IVARI,I
      REAL*8 WORK(3),INDIC1,VOLUME,INST,VALER(4),ZERO,PREC,R8PREM,ENERGI
      COMPLEX*16 C16B
      CHARACTER*2 CODRET
      CHARACTER*8 RESUL,CRIT,NOMA,NOMMAI,VALEK(2),KIORDM
      CHARACTER*8 KIORD,K8B,LPAIN(10),LPAOUT(2),TYPARR(7)
      CHARACTER*8 NOMGD
      CHARACTER*16 TYPRES,OPTION,NOPARR(7),LIGRMO,COMPT
      CHARACTER*19 KNUM,LIGREL,KINS,COMPOR
      CHARACTER*19 CHVARC,CHVREF
      CHARACTER*24 CHGEOM,CHCARA(18),CHHARM,CHVARI,CHDEPL
      CHARACTER*24 VALK(2)
      CHARACTER*24 CHSIG,LCHIN(10),LCHOUT(2)
      CHARACTER*24 MLGGMA,MLGNMA
      CHARACTER*24 CHSIGM,CHDEPM,CHBID
      LOGICAL EXISDG

      DATA TYPARR/'I','R','K8','K8','R','R','R'/
      DATA CHVARC,CHVREF /'&&PEINGL.CHVARC','&&PEINGL.CHVARC.REF'/
C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ()

C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0
      OPTION = OPTIOZ
      NOPARR(1) = 'NUME_ORDRE'
      NOPARR(2) = 'INST'
      NOPARR(3) = 'LIEU'
      NOPARR(4) = 'ENTITE'
      NOPARR(5) = OPTION
      NBPARR    = 5
      IF (OPTION(1:4).EQ.'ENER') THEN
        NOPARR(5) = 'TOTALE'
        IF (OPTION(6:9).EQ.'DISS') THEN
          NOPARR(6) = 'ENDO'
          NOPARR(7) = 'PLAS'
        ELSE
          NOPARR(6) = 'MEMBRANE'
          NOPARR(7) = 'FLEXION'
        ENDIF
        NBPARR    = 7
      END IF
      ENERGI = ZERO
      DO 8 I=1,3
         WORK(I)=0.D0
 8    CONTINUE
      DO 9 I=1,4
         VALER(I)=0.D0
 9    CONTINUE

C --- RECUPERATION DU RESULTAT A TRAITER :
C     ----------------------------------
      CALL GETVID(' ','RESULTAT',1,1,1,RESUL,NR)

      IF (NR.EQ.0) THEN
        CALL U2MESS('F','UTILITAI3_76')
      END IF

C --- ON VERIFIE QUE LE  RESULTAT A TRAITER EST DE TYPE EVOL_NOLI :
C     -----------------------------------------------------------
      CALL GETTCO(RESUL,TYPRES)
      IF (TYPRES(1:9).NE.'EVOL_NOLI') THEN
        CALL U2MESS('F','UTILITAI3_77')
      END IF

C --- RECUPERATION DE LA PRECISION POUR LE TRAITEMENT DES NUMEROS
C --- D'ORDRE :
C     -------
      CALL GETVR8(' ','PRECISION',1,1,1,PREC,NP)

C --- RECUPERATION DU CRITERE POUR LE TRAITEMENT DES NUMEROS D'ORDRE :
C     --------------------------------------------------------------
      CALL GETVTX(' ','CRITERE',1,1,1,CRIT,NC)

C --- RECUPERATION DES NUMEROS D'ORDRE A TRAITER :
C     ------------------------------------------
      KNUM = '&&PEINGL.NUME_ORDRE'
      CALL RSUTNU(RESUL,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
      IF (IRET.NE.0) GO TO 70
      CALL JEVEUO(KNUM,'L',JORD)

C --- RECUPERATION DES INSTANTS CORRESPONDANT AUX NUMEROS D'ORDRE :
C     -----------------------------------------------------------
      KINS = '&&PEINGL.INSTANT'
      CALL WKVECT(KINS,'V V R',NBORDR,JINS)
      CALL JENONU(JEXNOM(RESUL//'           .NOVA','INST'),IRET)
      IF (IRET.NE.0) THEN
        DO 10 IORD = 1,NBORDR
          NUMORD = ZI(JORD+IORD-1)
          CALL RSADPA(RESUL,'L',1,'INST',NUMORD,0,IAINST,K8B)
          ZR(JINS+IORD-1) = ZR(IAINST)
   10   CONTINUE
      END IF

C --- VERIFICATIONS ET RECUPERATION DU NOM DU MAILLAGE :
C     ------------------------------------------------
      CALL MECHAM(OPTION,MODELE,NCHAR,LCHAR,CARA,NH,CHGEOM,CHCARA,
     &            CHHARM,IRET)
      IF (IRET.NE.0) GO TO 80
      NOMA = CHGEOM(1:8)
      MLGNMA = NOMA//'.NOMMAI'
      MLGGMA = NOMA//'.GROUPEMA'

      CALL EXLIM3(OPTIOZ,'V',MODELE,LIGREL)

C ---  CREATION DE LA TABLE 'GLOBALE' :
C      -----------------------------
      CALL TBCRSD(RESU,'G')
      CALL TBAJPA(RESU,NBPARR,NOPARR,TYPARR)

C --- BOUCLE SUR LES NUMEROS D'ORDRE DU RESULTAT :
C     ------------------------------------------
      DO 60 IORD = 1,NBORDR
        CALL JEMARQ()
        CALL JERECU('V')

C ---  RECUPERATION DU NUMERO D'ORDRE :
C      ------------------------------
        NUMORD = ZI(JORD+IORD-1)
        CALL CODENT(NUMORD,'G',KIORD)

C ---  RECUPERATION DE L'INSTANT :
C      -------------------------
        INST = ZR(JINS+IORD-1)
        VALER(1) = INST

C ---  RECUPERATION OU CONSTITUTION DU CHAMP DE VARIABLE DE COMMANDE
C ---  ET DE LA VARIABLE DE COMMANDE DE REFERENCE :
C      --------------------------------------------

        CALL VRCINS(MODELE,MATE,CARA,INST,CHVARC,CODRET)
        CALL VRCREF(MODELE(1:8),MATE(1:8),CARA(1:8),CHVREF(1:19))


C ---  RECUPERATION DE LA RELATION DE COMPORTEMENT ASSOCIEE AU
C ---  NUMERO D'ORDRE COURANT :
C      ----------------------
        CALL RSEXCH(RESUL,'COMPORTEMENT',NUMORD,COMPOR,IRET)
        IF (IRET.GT.0) THEN
           VALK(1) = RESUL
           VALK(2) = KIORD
           CALL U2MESK('F','UTILITAI3_78', 2 ,VALK)
        END IF

CCC---RECUPERATION DU COMPOR PAR ETENCA

        LIGRMO = MODELE//'.MODELE'

C --- CREATION DU TABLEAU DESCRIPTEUR DE LA CARTE COMPOR ---

        CALL ETENCA(COMPOR,LIGRMO,IRET)
        IF (IRET.NE.0) THEN
          CALL U2MESS('F','UTILITAI2_62')
        END IF
C --- RECUPERATION DE LA GRANDEUR (ICI COMPOR)  ---
C --- REFERENCEE PAR LA CARTE COMPO             ---

        CALL JEVEUO(COMPOR//'.DESC','L',IDESC)
        NGDMAX = ZI(IDESC+2-1)

        NOMGD = 'COMPOR  '

        CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,K8B,IRET)

        IF (NEC.GT.1) THEN
          CALL U2MESS('F','UTILITAI2_61')
        END IF


C --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR  ---

        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',NCMPMX,K8B)

C --- TABLEAU DE VALEURS DE LA CARTE COMPO     ---
C --- (CONTENANT LES VALEURS DU COMPORTEMENT)  ---

        CALL JEVEUO(COMPOR//'.VALE','L',IVALE)

C --- RECUPERATION DU VECTEUR D'ADRESSAGE DANS LA CARTE  ---
C --- CREE PAR ETENCA                                    ---

        CALL JEVEUO(COMPOR//'.PTMA','L',IPTMA)

C --- AFFECTATION DU TABLEAU DES NOEUDS  ---

C --- NOMBRE DE MAILLES DU MAILLAGE ---

        CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IRET)

        DO 20 IMA = 1,NBMA
          IF (ZI(IPTMA+IMA-1).NE.0) THEN
            IGD = ZI(IPTMA+IMA-1)
            IDEBGD = (IGD-1)*NCMPMX
            DG = ZI(IDESC+3+2*NGDMAX+ZI(IPTMA+IMA-1)-1)

C ---     ON S'ASSURE QUE LA PREMIERE COMPOSANTE DE LA GRANDEUR
C ---     QUI EST RELCOM A BIEN ETE AFFECTEE

            IF (.NOT.EXISDG(DG,1)) THEN
              CALL U2MESS('F','UTILITAI2_63')
            END IF
C ---     RECUPERATION DU COMPORTEMENT AFFECTE A LA MAILLE
            COMPT = ZK16(IVALE+IDEBGD+1-1)

C ---     RECUPERATION DES NUMEROS DES NOEUDS DE LA MAILLE
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',IMA),'L',ICONEX)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IMA),'LONMAX',NBNO,K8B)

          END IF
   20   CONTINUE

CCC---FIN DE RECUPERATION DU COMPOR

C ---  RECUPERATION DU CHAMP DE CONTRAINTES ASSOCIE AU
C ---  NUMERO D'ORDRE COURANT :
C      ----------------------
        CALL RSEXCH(RESUL,'SIEF_ELGA',NUMORD,CHSIG,IRET)
        IF (IRET.GT.0) THEN
           VALK(1) = RESUL
           VALK(2) = KIORD
           CALL U2MESK('F','CALCULEL3_17', 2 ,VALK)
        END IF

C --- SI LE NUMERO COURANT EST INFERIEUR A NBORDR ON RECUPERE LES
C --- CONTRAINTES DE L INSTANT PRECEDENT

        IF (IORD.GT.1) THEN
          NUMORM = ZI(JORD+IORD-1-1)
          CALL CODENT(NUMORM,'G',KIORDM)
          CALL RSEXCH(RESUL,'SIEF_ELGA',NUMORM,CHSIGM,IRET)
          IF (IRET.GT.0) THEN
             VALK(1) = RESUL
             VALK(2) = KIORDM
             CALL U2MESK('F','CALCULEL3_17', 2 ,VALK)
          END IF
        END IF

C ---  RECUPERATION DU CHAMP DES VARIABLES INTERNES ASSOCIE AU
C ---  NUMERO D'ORDRE COURANT :
C      ----------------------
        CALL RSEXCH(RESUL,'VARI_ELGA',NUMORD,CHVARI,IRET)
        IVARI=1
        IF (IRET.GT.0) THEN
           IF (OPTION.NE.'ENER_ELAS') THEN
               VALK(1) = RESUL
               VALK(2) = KIORD
               CALL U2MESK('F','UTILITAI3_79', 2 ,VALK)
           ELSE
C CREATION D'UN CHAMP DE VARIABLES INTERNES NUL
              IVARI=0
              CHBID='&&PEINGL.VARINUL'
              CALL CALCUL('S','TOU_INI_ELGA',LIGRMO,1,CHGEOM,'PGEOMER',
     &                  1,CHBID,'PVARI_R','V')
           END IF
        END IF

C ---  RECUPERATION DU CHAMP DES DEPLACEMENTS ASSOCIE AU
C ---  NUMERO D'ORDRE COURANT :
C      ----------------------
        CALL RSEXCH(RESUL,'DEPL',NUMORD,CHDEPL,IRET)
        IF (IRET.GT.0) THEN
           VALK(1) = RESUL
           VALK(2) = KIORD
           CALL U2MESK('F','CALCULEL3_11', 2 ,VALK)
        END IF

C ---  RECUPERATION DU CHAMP DES DEPLACEMENTS ASSOCIE AU
C ---  NUMERO D'ORDRE PRECEDENT :
C      ----------------------
        IF (IORD.GT.1) THEN
          CALL RSEXCH(RESUL,'DEPL',NUMORM,CHDEPM,IRET)
          IF (IRET.GT.0) THEN
             VALK(1) = RESUL
             VALK(2) = KIORDM
             CALL U2MESK('F','CALCULEL3_11', 2 ,VALK)
          END IF
        END IF

C ---  CALCUL DE L'INDICATEUR GLOBAL DE PERTE DE RADIALITE
C ---  SUR TOUTES LES MAILLES DU MODELE :
C      --------------------------------
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PMATERC'
        LCHIN(2) = MATE
        LPAIN(3) = 'PDEPLR'
        LCHIN(3) = CHDEPL
        LPAIN(4) = 'PCONTPR'
        LCHIN(4) = CHSIG
        LPAIN(5) = 'PVARIPR'
        IF (IVARI.EQ.1) THEN
           LCHIN(5) = CHVARI
        ELSE
           LCHIN(5) = CHBID
        ENDIF
        LPAIN(6) = 'PCOMPOR'
        LCHIN(6) = COMPOR
        LPAIN(7) = 'PVARCPR'
        LCHIN(7) = CHVARC
        LPAIN(8) = 'PVARCRR'
        LCHIN(8) = CHVREF
        LPAIN(9) = 'PCACOQU'
        LCHIN(9) = CHCARA(7)
        LPAIN(10) = 'PNBSP_I'
        LCHIN(10) = CARA//'.CANBSP'
        NBIN = 10
        IF (OPTION.EQ.'ENER_TOTALE') THEN
          IF (IORD.GT.1) THEN
            LPAIN(9) = 'PCONTMR'
            LCHIN(9) = CHSIGM
            LPAIN(10) = 'PDEPLM'
            LCHIN(10) = CHDEPM
            NBIN = 10
          END IF
        END IF
        IF (OPTION.EQ.'INDIC_ENER' .OR. OPTION.EQ.'INDIC_SEUIL') THEN
          NBOUT = 2
          LPAOUT(1) = 'PENERD1'
          LCHOUT(1) = '&&PEINGL.INDIC'
          LPAOUT(2) = 'PENERD2'
          LCHOUT(2) = '&&PEINGL.VOLUME'
        ELSE IF (OPTION.EQ.'ENER_ELAS' .OR.
     &           OPTION.EQ.'ENER_TOTALE') THEN
          NBOUT = 1
          LPAOUT(1) = 'PENERD1'
          LCHOUT(1) = '&&PEINGL.INDIC'
        ELSE IF (OPTION.EQ.'ENER_DISS') THEN
          NBOUT = 1
          LPAOUT(1) = 'PDISSD1'
          LCHOUT(1) = '&&PEINGL.INDIC'
        END IF

        CALL CALCUL('S',OPTION,LIGREL,NBIN,LCHIN,LPAIN,NBOUT,LCHOUT,
     &              LPAOUT,'V')

C ---  BOUCLE SUR LES OCCURENCES DU MOT-CLE INDIC_ENER :
C      -----------------------------------------------
        DO 50 IOCC = 1,NBOCC

C ---   RECUPERATION DES MAILLES POUR LESQUELLES ON VA CALCULER
C ---   L'INDICATEUR :
C       ------------
          CALL GETVTX(OPTION,'TOUT',IOCC,1,0,K8B,NT)
          CALL GETVEM(NOMA,'MAILLE',OPTION,'MAILLE',IOCC,1,0,K8B,NM)
          CALL GETVEM(NOMA,'GROUP_MA',OPTION,'GROUP_MA',IOCC,1,0,K8B,NG)

C ---   TRAITEMENT DU MOT CLE "TOUT" ,LA QUANTITE EST CALCULEE
C ---   SUR TOUT LE MODELE :
C       ------------------
          IF (NT.NE.0) THEN
            IF (OPTION.EQ.'INDIC_ENER' .OR.
     &          OPTION.EQ.'INDIC_SEUIL') THEN

C ---     SOMMATION DES INTEGRALES SUIVANTES SUR LE MODELE
C ---     LA PREMIERE INTEGRALE CALCULEE EST :
C ---     SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV
C ---     LA SECONDE INTEGRALE CALCULEE EST LE VOLUME :
C         -------------------------------------------
              CALL MESOMM(LCHOUT(1),1,IBID,WORK(1),C16B,0,IBID)
              CALL MESOMM(LCHOUT(2),1,IBID,WORK(2),C16B,0,IBID)

              INDIC1 = WORK(1)
              VOLUME = WORK(2)

              IF (INDIC1.LE.1.0D4*R8PREM()) THEN
                INDIC1 = ZERO
              END IF

              IF (VOLUME.LE.R8PREM()) THEN
                CALL U2MESS('F','UTILITAI3_80')
              END IF

              VALER(2) = INDIC1/VOLUME
              VALEK(1) = NOMA
              VALEK(2) = 'TOUT'

            ELSE IF (OPTION.EQ.'ENER_ELAS'  .OR.
     &               OPTION.EQ.'ENER_TOTALE'.OR.
     &               OPTION.EQ.'ENER_DISS') THEN

C ---          SOMMATION DE L'ENERGIE ( ELASTIQUE OU TOTALE)
C ---          SUR LE MODELE :
C              -------------
              IF(OPTION.EQ.'ENER_TOTALE') THEN
                CALL MESOMM(LCHOUT(1),1,IBID,WORK(1),C16B,0,IBID)
              ELSE
                CALL MESOMM(LCHOUT(1),3,IBID,WORK,C16B,0,IBID)
              ENDIF
C ---  BOUCLE SUR LES PAS DE TEMPS ON SOMME LES TERMES DE
C ---  L ENERGIE TOTAL
              IF ((COMPT(1:9).NE.'VMIS_ISOT') .AND.
     &            (COMPT(1:4).NE.'ELAS') .AND.
     &            (OPTION.NE.'ENER_ELAS').AND.
     &            (OPTION.NE.'ENER_DISS')) THEN
                 ENERGI = ENERGI + WORK(1)
              ELSE
                ENERGI = WORK(1)
              END IF

              VALER(2) = ENERGI
              VALER(3) = WORK(2)
              VALER(4) = WORK(3)
              VALEK(1) = NOMA
              VALEK(2) = 'TOUT'

            END IF

C ---    ECRITURE DE L'INDICATEUR OU DE L'ENERGIE DANS LA TABLE :
C        ------------------------------------------------------
            CALL TBAJLI(RESU,NBPARR,NOPARR,NUMORD,VALER,C16B,VALEK,0)
          END IF

C ---   TRAITEMENT DU MOT CLE "GROUP_MA" ,LA QUANTITE EST CALCULEE
C ---   SUR LE GROUP_MA COURANT :
C       -----------------------
          IF (NG.NE.0) THEN
            NBGRMA = -NG
            CALL WKVECT('&&PEINGL_GROUPM','V V K8',NBGRMA,JGR)
            CALL GETVEM(NOMA,'GROUP_MA',OPTION,'GROUP_MA',IOCC,1,NBGRMA,
     &                  ZK8(JGR),NG)

C ---     BOUCLE SUR LES GROUPES DE MAILLES :
C         ---------------------------------
            VALEK(2) = 'GROUP_MA'
            DO 30 IG = 1,NBGRMA
              NOMMAI = ZK8(JGR+IG-1)
              CALL JEEXIN(JEXNOM(MLGGMA,NOMMAI),IRET)
              IF (IRET.EQ.0) THEN
                CALL U2MESK('F','UTILITAI3_46',1,NOMMAI)
              END IF
              CALL JELIRA(JEXNOM(MLGGMA,NOMMAI),'LONUTI',NBMA,K8B)
              IF (NBMA.EQ.0) THEN
                CALL U2MESK('F','UTILITAI3_47',1,NOMMAI)
              END IF
              CALL JEVEUO(JEXNOM(MLGGMA,NOMMAI),'L',JAD)

              IF (OPTION.EQ.'INDIC_ENER' .OR.
     &            OPTION.EQ.'INDIC_SEUIL') THEN

C ---      SOMMATION DES INTEGRALES SUIVANTES SUR LES
C ---      MAILLES DU GROUP_ MA
C ---      LA PREMIERE INTEGRALE CALCULEE EST :
C ---      SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV
C ---      LA SECONDE INTEGRALE CALCULEE EST LE VOLUME :
C          -------------------------------------------
                CALL MESOMM(LCHOUT(1),1,IBID,WORK(1),C16B,NBMA,ZI(JAD))
                CALL MESOMM(LCHOUT(2),1,IBID,WORK(2),C16B,NBMA,ZI(JAD))

                INDIC1 = WORK(1)
                VOLUME = WORK(2)

                IF (INDIC1.LE.1.0D4*R8PREM()) THEN
                  INDIC1 = ZERO
                END IF

                IF (VOLUME.LE.R8PREM()) THEN
                  CALL U2MESK('F','UTILITAI3_81',1,NOMMAI)
                END IF

                VALER(2) = INDIC1/VOLUME
                VALEK(1) = NOMMAI

              ELSE IF (OPTION.EQ.'ENER_ELAS' .OR.
     &                 OPTION.EQ.'ENER_TOTALE' .OR.
     &                 OPTION.EQ.'ENER_DISS') THEN

C ---          SOMMATION DE L'ENERGIE ( ELASTIQUE OU TOTALE)
C ---          SUR LE MODELE :
C              -------------
              IF(OPTION.EQ.'ENER_TOTALE') THEN
                CALL MESOMM(LCHOUT(1),1,IBID,WORK(1),C16B,NBMA,ZI(JAD))
              ELSE
                CALL MESOMM(LCHOUT(1),3,IBID,WORK,C16B,NBMA,ZI(JAD))
              ENDIF

C ---  BOUCLE SUR LES PAS DE TEMPS ON SOMME LES TERMES DE
C ---  L ENERGIE TOTAL

                IF ((COMPT(1:9).NE.'VMIS_ISOT') .AND.
     &             (COMPT(1:4).NE.'ELAS') .AND.
     &             (OPTION.NE.'ENER_ELAS').AND.
     &             (OPTION.NE.'ENER_DISS')) THEN


                  ENERGI = ENERGI + WORK(1)
                ELSE
                  ENERGI = WORK(1)
                END IF

                VALER(2) = ENERGI
                VALER(3) = WORK(2)
                VALER(4) = WORK(3)
                VALEK(1) = NOMMAI

              END IF


C ---    ECRITURE DE L'INDICATEUR OU DE L'ENERGIE DANS LA TABLE :
C        ------------------------------------------------------

C ---      ECRITURE DE L'INDICATEUR DANS LA TABLE :
C          --------------------------------------
              CALL TBAJLI(RESU,NBPARR,NOPARR,NUMORD,VALER,C16B,VALEK,0)
   30       CONTINUE

            CALL JEDETR('&&PEINGL_GROUPM')
          END IF

C ---   TRAITEMENT DU MOT CLE "MAILLE" ,L'INDICATEUR EST CALCULE
C ---   SUR LA MAILLE COURANTE :
C       ----------------------
          IF (NM.NE.0) THEN
            NBMAIL = -NM
            CALL WKVECT('&&PEINGL_MAILLE','V V K8',NBMAIL,JMA)
            CALL GETVEM(NOMA,'MAILLE',OPTION,'MAILLE',IOCC,1,NBMAIL,
     &                  ZK8(JMA),NM)

C ---    BOUCLE SUR LES MAILLES :
C        ----------------------
            VALEK(2) = 'MAILLE'
            DO 40 IM = 1,NBMAIL
              NOMMAI = ZK8(JMA+IM-1)
              CALL JEEXIN(JEXNOM(MLGNMA,NOMMAI),IRET)
              IF (IRET.EQ.0) THEN
                CALL U2MESK('F','UTILITAI3_49',1,NOMMAI)
              END IF
              CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUME)

              IF (OPTION.EQ.'INDIC_ENER' .OR.
     &            OPTION.EQ.'INDIC_SEUIL') THEN

C ---      LES INTEGRALES SONT CALCULEES SUR LA MAILLE COURANTE
C ---      LA PREMIERE INTEGRALE CALCULEE EST :
C ---      SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV
C ---      LA SECONDE INTEGRALE CALCULEE EST LE VOLUME :
C          -------------------------------------------
                CALL MESOMM(LCHOUT(1),1,IBID,WORK(1),C16B,1,NUME)
                CALL MESOMM(LCHOUT(2),1,IBID,WORK(2),C16B,1,NUME)

                INDIC1 = WORK(1)
                VOLUME = WORK(2)

                IF (INDIC1.LE.1.0D4*R8PREM()) THEN
                  INDIC1 = ZERO
                END IF

                IF (VOLUME.LE.R8PREM()) THEN
                  CALL U2MESK('F','UTILITAI3_82',1,NOMMAI)
                END IF

                VALER(2) = INDIC1/VOLUME
                VALEK(1) = NOMMAI

              ELSE IF (OPTION.EQ.'ENER_ELAS' .OR.
     &                 OPTION.EQ.'ENER_TOTALE' .OR.
     &                 OPTION.EQ.'ENER_DISS') THEN

C ---          SOMMATION DE L'ENERGIE ( ELASTIQUE OU TOTALE)
C ---          SUR LE MODELE :
C              -------------
              IF(OPTION.EQ.'ENER_TOTALE') THEN
                CALL MESOMM(LCHOUT(1),1,IBID,WORK(1),C16B,1,NUME)
              ELSE
                CALL MESOMM(LCHOUT(1),3,IBID,WORK,C16B,1,NUME)
              ENDIF

                IF ((COMPT(1:9).NE.'VMIS_ISOT') .AND.
     &            (COMPT(1:4).NE.'ELAS') .AND.
     &            (OPTION.NE.'ENER_ELAS')) THEN

                  ENERGI = ENERGI + WORK(1)
                ELSE
                  ENERGI = WORK(1)
                END IF

                VALER(2) = ENERGI
                VALER(3) = WORK(2)
                VALER(4) = WORK(3)

                VALEK(1) = NOMMAI

              END IF

C ---      ECRITURE DE L'INDICATEUR DANS LA TABLE :
C          --------------------------------------
              CALL TBAJLI(RESU,NBPARR,NOPARR,NUMORD,VALER,C16B,VALEK,0)
   40       CONTINUE

            CALL JEDETR('&&PEINGL_MAILLE')
          END IF
   50   CONTINUE
        CALL JEDETR('&&MECHTI.CH_INST_R')
        CALL JEDETR(CHVARC)
        CALL JEDETR(CHVREF)
        CALL JEDETC('V',COMPOR//'.PTMA',1)
        CALL JEDEMA()
   60 CONTINUE
   70 CONTINUE
      CALL JEDETR(KNUM)
      CALL JEDETR(KINS)
      CALL JEDETR('&&PEINGL.INDIC')
      CALL JEDETR('&&PEINGL.VOLUME')
      CALL JEDETR('&&MEHARM.NUME_HARM')
      IF (IVARI.EQ.0) THEN
         CALL JEDETR(CHBID)
      ENDIF

   80 CONTINUE
      CALL JEDEMA()

C.============================ FIN DE LA ROUTINE ======================
      END
