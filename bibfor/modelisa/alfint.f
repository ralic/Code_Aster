      SUBROUTINE ALFINT (CHMATZ, IMATE, NOMMAZ, TDEF, NOPARZ, NUMMAT,
     &                    PREC, CH19,EOUN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
      IMPLICIT NONE
C
C      ALFINT   -- INTERPOLATION DES COEFFICIENTS DE DILATATION
C                  ALPHA PERMETTANT LA PRISE EN COMPTE DU FAIT
C                  QUE LA TEMPERATURE DE REFERENCE ( A LAQUELLE
C                  LES DEFORMATIONS SONT NULLES) EST DIFFERENTE
C                  DE LA TEMPERATURE AMBIANTE.
C
C   ARGUMENT        E/S  TYPE         ROLE
C    CHMATZ         IN     K*       NOM DU CHAM_MATER COURANT
C    NOMMAZ         IN     K*       NOM DU MATERIAU COURANT
C    TDEF           IN     R        TEMPERATURE DE DEFINITION DU
C                                   MATERIAU
C    NOPARZ         IN     K*       NOM DU PARAMETRE A INTERPOLER :
C                                   = 'ALPHA'
C                                   = 'FBM_ALPH'
C                                   = 'A_ALPHA'
C    NUMMAT         IN     I        INDICE DU MATERIAU
C    PREC           IN     R        PRECISION AVEC LAQUELLE ON COMPARE
C                                   LA TEMPERATURE COURANTE ET LA
C                                   LA TEMPERATURE DE REFERENCE ET
C                                   SELON LAQUELLE ON DECIDE SI ON
C                                   FAIT UN DEVELOPPEMENT DE T AU
C                                   PREMIER ORDRE AUTOUR DE TREF.
C    CH19Z          VAR    K*       NOM DE LA FONCTION ALPHA DU
C                                   MATERIAU EN ENTREE
C                                   NOM DE LA FONCTION CONTENANT
C                                   LES VALEURS DE ALPHA MODIFIEES EN
C                                   SORTIE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*)     CHMATZ, NOMMAZ, NOPARZ
      CHARACTER*19       CH19
      CHARACTER*1       EOUN
C -----  VARIABLES LOCALES
      CHARACTER*2       CODRET
      CHARACTER*8       K8B, CHMAT, NOMMAT, NOPARA,KTREF,NOMGD,VALK(2)
      CHARACTER*10      PHENOM
      CHARACTER*16      TYPRES,NOMCMD
      CHARACTER*19      CHWORK
      INTEGER NUMMAT,NCMP,IDTMPR,JNOMRC,IDVALE,IDVALW,I,NBPTS,IMATE
      INTEGER NBEC,K,IER,EC1,KK,IGD,NGDMAX,JDESC,JVALE,ITREF
      REAL*8 PREC,UNDEMI,TREF,ALFREF,ALPHAI,TI,TIM1,TIP1
      REAL*8 ALFIM1,ALFIP1,DALREF,TDEF,R8VIDE
      LOGICAL EXISDG

C.========================= DEBUT DECLARATIONS NORMALISEES  JEVEUX ====
      CHARACTER*32       JEXNOM,JEXNUM
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C.========================= FIN DECLARATIONS NORMALISEES  JEVEUX ====
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      CHMAT  = CHMATZ
      NOMMAT = NOMMAZ
      NOPARA = NOPARZ
      UNDEMI = 0.5D0

      CALL GETRES(K8B,K8B,NOMCMD)
C     EN THERMIQUE ON N A PAS BESOIN DE CALCULER ALPHA=F(T)
      IF (NOMCMD(1:5).EQ.'THER_') GOTO 9999
C
C --- RECUPERATION DE LA TEMPERATURE DE REFERENCE (TREF):
C     ---------------------------------------------------
      CALL ASSERT(EOUN.EQ.'E'.OR.EOUN.EQ.'N')
      IF (EOUN.EQ.'E') THEN
        CALL JEEXIN(CHMAT//'.TEMPE_REF .VALE',ITREF)

        IF (ITREF.GT.0) THEN
C         -- CAS DE L'ANCIENNE SYNTAXE : AFFE_CHAR_MECA/TEMP_CALCULEE :
          CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','TEMP_R'),'LONMAX',NCMP,
     &                K8B)
          CALL JEVEUT(CHMAT//'.TEMPE_REF .VALE','L',IDTMPR)
          TREF = ZR(IDTMPR+NCMP*(IMATE-1)+1-1)
          IF (TREF.EQ.R8VIDE()) GOTO 9998

        ELSE
C         -- CAS DE LA NOUVELLE SYNTAXE : AFFE_MATERIAU/AFFE_VARC/TEMP
          CALL JEVEUO (CHMAT//'.CHAMP_MAT .DESC', 'L', JDESC )
          CALL JEVEUO (CHMAT//'.CHAMP_MAT .VALE', 'L', JVALE )
          IGD = ZI(JDESC-1+1)
          CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',IGD),NOMGD)
          CALL ASSERT(NOMGD.EQ.'NEUT_F')
          CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','NEUT_F'),'LONMAX',NCMP,
     &                K8B)
          CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NBEC,K8B,IER)
          NGDMAX=ZI(JDESC-1+2)
C         TREF EST SUR LE 1ER ENTIER CODE :
          EC1=ZI(JDESC-1+3+2*NGDMAX+NBEC*(IMATE-1)+1)
          K=0
          DO 777, KK=1,30
            IF (EXISDG(EC1,KK)) K=K+1
 777      CONTINUE
          IF (ZK8(JVALE+NCMP*(IMATE-1)+K-2).NE.'TREF=>')
     &        CALL U2MESK('F','CALCULEL6_56',1,CHMAT)
          KTREF = ZK8(JVALE+NCMP*(IMATE-1)+K-1)
          IF (KTREF.EQ.'NAN') GOTO 9998

          READ (KTREF,'(F8.2)') TREF
        ENDIF


      ELSE
C        -- LE 12/6/2006 : CC ET SG SOUHAITENT QUE POUR LES MATERIAUX
C        AUX NOEUDS, ALPHA RECUPERE DANS LES TE00IJ SOIT CELUI DE
C        LA FONCTION TEL QUEL (SANS MODIF. PAR ALFINT / TDEF / TREF) :
         TREF = TDEF
      ENDIF

C
C --- RECUPERATION DU NOM DU PHENOMENE ASSOCIE AU MATERIAU :
C     ----------------------------------------------------
      CALL JEVEUT(NOMMAT//'.MATERIAU.NOMRC','L',JNOMRC)
      PHENOM = ZK16(JNOMRC+NUMMAT-1)(1:10)
C
C --- CALCUL DE ALPHA A LA TEMPERATURE DE REFERENCE :
C     ---------------------------------------------
      CALL RCVALE(NOMMAT, PHENOM, 1, 'TEMP    ', TREF, 1, NOPARA,
     &            ALFREF, CODRET, 'F ')
C
C --- CREATION DE LA NOUVELLE FONCTION DEVANT CONTENIR LES VALEURS
C --- INTERPOLEES DE ALPHA :
C --- LE NOM DE CETTE NOUVELLE FONCTION EST DEFINI DE LA MANIERE
C --- SUIVANTE : NOM_NOUVEAU = &&NOM_ANCIEN(1:2)//00NUMMAT :
C     ----------------------------------------------------
      CALL GCNCON ( '.' , CHWORK )
      CHWORK = '&&'//CHWORK(3:8)
C
C --- CREATION DE LA NOUVELLE FONCTION CHWORK PAR RECOPIE DE CH19
C --- SUR LA VOLATILE :
C     ---------------
      CALL GETTCO(CH19,TYPRES)
      IF (TYPRES.EQ.'FORMULE') THEN
          CALL U2MESS('F','MODELISA2_1')
      ENDIF
      CALL COPISD ( 'FONCTION', 'V', CH19, CHWORK )
C
C --- RECUPERATION DU NOMBRE DE TERMES DEFINISSANT LA FONCTION :
C     --------------------------------------------------------
      CALL JELIRA(CHWORK(1:19)//'.VALE', 'LONMAX', NBPTS, K8B)
C
C --- NOMBRE DE POINTS DE LA FONCTION :
C     -------------------------------
      NBPTS = NBPTS/2
C
C --- RECUPERATION DU .VALE DE LA FONCTION DESTINEE A CONTENIR LES
C --- VALEURS DE ALPHA INTERPOLEES :
C     ----------------------------
      CALL JEVEUO(CHWORK(1:19)//'.VALE', 'E', IDVALW)
C
C --- RECUPERATION DU .VALE DE LA FONCTION CONTENANT LES
C --- VALEURS INITIALES DE ALPHA  :
C     --------------------------
      CALL JEVEUO(CH19(1:19)//'.VALE', 'L', IDVALE)
C
C --- CALCUL DES ALPHA INTERPOLES :
C     ---------------------------
      DO 10 I = 1, NBPTS
C
        ALPHAI = ZR(IDVALE+I+NBPTS-1)
        TI     = ZR(IDVALE+I-1)
C
C --- DANS LE CAS OU ABS(TI-TREF) > PREC :
C --- ALPHA_NEW(TI) = (ALPHA(TI)*(TI-TDEF) - ALPHA(TREF)*(TREF-TDEF))
C ---                 /(TI-TREF)   :
C                    -----------
        IF (ABS(TI-TREF).GE.PREC) THEN
C
           ZR(IDVALW+I+NBPTS-1) = (ALPHAI*(TI-TDEF)- ALFREF*(TREF-TDEF))
     &                           /(TI-TREF)
C
C --- DANS LE CAS OU ABS(TI-TREF) < PREC :
C --- IL FAUT D'ABORD CALCULER LA DERIVEE DE ALPHA PAR RAPPORT
C --- A LA TEMPERATURE EN TREF : D(ALPHA)/DT( TREF) :
C     --------------------------------------------
        ELSE
C
C ---   DANS LE CAS OU I > 1 ET I < NBPTS :
C ---   D(ALPHA)/DT( TREF) = 0.5*((ALPHA(TI+1)-ALPHA(TREF))/(TI+1-TREF)
C ---                            +(ALPHA(TREF)-ALPHA(TI-1))/(TREF-TI-1))
C                                 -------------------------------------
          IF (I.GT.1.AND.I.LT.NBPTS) THEN
C
             TIM1     = ZR(IDVALE+I-1-1)
             TIP1     = ZR(IDVALE+I+1-1)
             ALFIM1   = ZR(IDVALE+I+NBPTS-1-1)
             ALFIP1   = ZR(IDVALE+I+NBPTS+1-1)
             IF ( TIP1 .EQ. TREF ) CALL U2MESS('F','MODELISA2_2')
             IF ( TIM1 .EQ. TREF ) CALL U2MESS('F','MODELISA2_2')
C
             DALREF = UNDEMI*((ALFIP1-ALFREF)/(TIP1-TREF)
     &                       +(ALFREF-ALFIM1)/(TREF-TIM1))
C
C ---   DANS LE CAS OU I = NBPTS :
C ---   D(ALPHA)/DT( TREF) = (ALPHA(TREF)-ALPHA(TI-1))/(TREF-TI-1) :
C       ----------------------------------------------------------
          ELSEIF (I.EQ.NBPTS) THEN
C
             TIM1     = ZR(IDVALE+I-1-1)
             ALFIM1   = ZR(IDVALE+I+NBPTS-1-1)
             IF ( TIM1 .EQ. TREF ) CALL U2MESS('F','MODELISA2_2')
C
             DALREF   = (ALFREF-ALFIM1)/(TREF-TIM1)
C
C ---   DANS LE CAS OU I = 1 :
C ---   D(ALPHA)/DT( TREF) = (ALPHA(TI+1)-ALPHA(TREF))/(TI+1-TREF) :
C       ----------------------------------------------------------
          ELSEIF (I.EQ.1) THEN
C
             TIP1     = ZR(IDVALE+I+1-1)
             ALFIP1   = ZR(IDVALE+I+NBPTS+1-1)
             IF ( TIP1 .EQ. TREF ) CALL U2MESS('F','MODELISA2_2')
C
             DALREF   = (ALFIP1-ALFREF)/(TIP1-TREF)
C
          ENDIF
C
C ---   DANS CE CAS OU ABS(TI-TREF) < PREC , ON A :
C ---   ALPHA_NEW(TI) = ALPHA_NEW(TREF)
C ---   ET ALPHA_NEW(TREF) = D(ALPHA)/DT (TREF)*(TREF-TDEF)+ALPHA(TREF):
C       --------------------------------------------------------------
           ZR(IDVALW+I+NBPTS-1) = DALREF*(TREF-TDEF) + ALFREF
C
        ENDIF
C
  10  CONTINUE
C
C --- ON REMPLACE LA FONCTION EN ENTREE CH19 PAR LA FONCTION
C --- DE TRAVAIL CONTENANT LES VALEURS DE ALPHA INTERPOLEES CHWORK :
C     ------------------------------------------------------------
      CH19 = CHWORK

      GO TO 9999
C     -- SECTION "ERREUR":
9998  CONTINUE
      VALK(1)=CHMAT
      VALK(2)=NOMMAT
      CALL U2MESK('F','CALCULEL6_1',2,VALK)


9999  CONTINUE
      END
