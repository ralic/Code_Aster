      SUBROUTINE FONLEV ( RESU, NOMA, NBNOFF )
      IMPLICIT NONE
      CHARACTER*8         RESU, NOMA
      INTEGER             NBNOFF
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     VERIFICATION DES LEVRES ET DE LEUR CONNEXITE  
C
C     ENTREES:
C        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMA       : NOM DU MAILLAGE
C        NBNOFF     : NOMBRE DE NOEUDS EN FOND DE FISSURE
C        
C     -----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       JMAI1,JADR,JNOE1,JMAI2,JMAII, JJJ, IATYMA
      INTEGER       JINF, JSUP, IAMASE, ITYP, JVALE, JFINF, JFSUP
      INTEGER       JUFINF, JUFSUP, JUINF2
      INTEGER       IGR, NGR, INO, I, J, K, IBID, K2, J2
      INTEGER       NBMAI, NENT, INDICE
      INTEGER       NN,COMPTA,NBMAS1,NBMAS2, NBMAL
      INTEGER       IRET, IRET1,IRET2
      REAL*8        D, PREC, PRECR
      CHARACTER*4   TYPMA
      CHARACTER*6   NOMPRO
      CHARACTER*8   K8B, MAILLE, TYPE, NOEUG, TYPMCL(2),MOTCLE(2) 
      CHARACTER*9   TYPLEV(2),MOTFAC, VALK(2)
      CHARACTER*24  NOMOBJ, GROUMA, NOMMAI, CONEC, TRAV
      PARAMETER(PREC=1.D-1)
C     -----------------------------------------------------------------
C
      CALL JEMARQ()
      
      NOMPRO = 'FONLEV'
C
C     ------------------------------------------------------------------
C     INITIALISATION DE VARIABLES
C     ------------------------------------------------------------------
      GROUMA = NOMA//'.GROUPEMA'
      NOMMAI = NOMA//'.NOMMAI'
      CONEC = NOMA//'.CONNEX'
      CALL JEVEUO (NOMA//'.TYPMAIL','L',IATYMA)

C     ------------------------------------------------------------------
C     BOUCLE SUR LES DEUX LEVRES 
C     CELLES-CI SONT TRAITEES DE LA MEME MANIERE
C     ------------------------------------------------------------------
      TYPLEV(1) = 'LEVRE_SUP'
      TYPLEV(2) = 'LEVRE_INF'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'        
       
      DO 10 INDICE=1,2
        MOTFAC=TYPLEV(INDICE)
C        
C       EVALUATION DU NOMBRE DE MAILLES ET CONSTRUCTION DU VECTEUR DE
C       MAILLES DE TRAVAIL
C
        TRAV = '&&'//NOMPRO//'.'//MOTFAC
        CALL RELIEM ( ' ', NOMA, 'NO_MAILLE', MOTFAC, 1, 2,
     &                  MOTCLE, TYPMCL, TRAV, NBMAL )
        IF (NBMAL .EQ. 0) GOTO 9999
        CALL JEVEUO(TRAV,'L',JJJ)
C
C     ------------------------------------------------------------------
C     VERIFICATION DE L'EXISTENCE DES GROUPES DE MAILLES RENSEIGNEES
C     ET CALCUL DU NOMBRE TOTAL DE MAILLES
C     ------------------------------------------------------------------
C
        CALL GETVTX(MOTFAC,'GROUP_MA',1,1,NBMAL,ZK8(JJJ),NGR)
        CALL GETVTX(MOTFAC,'MAILLE',1,1,0,K8B,NENT)

C
C ---   ALLOCATION D'UN PREMIER OBJET DE TRAVAIL
C       DANS LEQUEL SERONT STOCKES LES MAILLES AVANT DE S'ASSURER QU'IL 
C       N'Y A PAS DUPPLICATION
C
        CALL WKVECT('&&'//NOMPRO//'.MAIL','V V K8',NBMAL,JMAI1)
        JMAII = JMAI1
C       ----------------------------------------------------------------
C      VERIFICATION POUR LES MAILLES DE LA LEVRE COURANTE
C      SI ON A UN SEUL NOEUD ALORS ELLES SONT DE TYPE SEG
C      SI ON A PLUSIEURS NOEUDS ALORS ELLES SONT DE TYPE QUAD OU TRIA
C      ET CALCUL DU NOMBRE TOTAL DE MAILLES DE LA LEVRE COURANTE
C      ----------------------------------------------------------------
C       SI GROUP_MA
        IF (NENT.EQ.0) THEN
          DO 110 IGR = 1, NGR

            CALL JELIRA (JEXNOM(GROUMA,ZK8(JJJ-1 + IGR)),'LONMAX',
     &                             NBMAI,K8B)
            CALL JEVEUO (JEXNOM(GROUMA,ZK8(JJJ-1 + IGR)),'L',JADR)
C
C
            DO 105 I = 1, NBMAI
              CALL JENUNO(JEXNUM(NOMMAI,ZI(JADR-1 + I)),MAILLE)
              CALL JENONU(JEXNOM(NOMMAI,MAILLE),IBID)
              ITYP=IATYMA-1+IBID
              CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
              TYPMA = TYPE(1:4)
              IF (((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA'))
     &               .AND.(NBNOFF.GT.1)) THEN
                 VALK(1) = TYPE
                 VALK(2) = MOTFAC
                 CALL U2MESK('F','RUPTURE0_65',2,VALK)
              ELSEIF ((TYPMA(1:3).NE.'SEG').AND.(NBNOFF.EQ.1)) THEN
                 VALK(1) = TYPE
                 VALK(2) = MOTFAC
                 CALL U2MESK('F','RUPTURE0_75',2,VALK)
              ELSE
                 ZK8(JMAI1) = MAILLE
                 JMAI1 = JMAI1 + 1
              ENDIF
C
 105        CONTINUE
C
 110      CONTINUE
        ELSE
C SI MAILLE
          DO 230 INO = 1, NBMAL
            CALL JENONU(JEXNOM(NOMMAI,ZK8(JJJ-1 + INO)),IBID)
            ITYP=IATYMA-1+IBID
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
            TYPMA = TYPE(1:4)
            IF (((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA'))
     &                                .AND.(NBNOFF.GT.1)) THEN
              VALK(1) = TYPE
              VALK(2) = MOTFAC
              CALL U2MESK('F','RUPTURE0_65',2,VALK)
            ELSEIF ((TYPMA(1:3).NE.'SEG').AND.(NBNOFF.EQ.1)) THEN
              VALK(1) = TYPE
              VALK(2) = MOTFAC
              CALL U2MESK('F','RUPTURE0_75',2,VALK)
            ELSE
              ZK8(JMAI1) = ZK8(JJJ-1  + INO)
              JMAI1 = JMAI1 + 1
            ENDIF
 230      CONTINUE

        ENDIF
C
C --- VERIFICATION QU'IL N Y A PAS DUPLICATION DES ENTITES ET STOCKAGE
C     ON MET 'O' SI L'ENTITE EST DUPPLIQUEE
C
C       ALLOCATION DU VECTEUR .LEVRESUP  .MAIL ET .LEVREINF  .MAIL
        NOMOBJ = RESU//'.LEVRE'//MOTFAC(7:9)//'  .MAIL'
        CALL WKVECT(NOMOBJ,'G V K8',NBMAL,JMAI2)
        K2 = 1
        DO 600 I=1,NBMAL-1
          IF ( ZK8(JMAII-1 + I).NE.'0' ) THEN
            ZK8(JMAI2-1 + K2) = ZK8(JMAII-1 + I)
            K2 = K2 + 1
            DO 605 J=I+1,NBMAL
              IF ( ZK8(JMAII-1 + I).EQ.ZK8(JMAII-1 + J) ) THEN
                ZK8(JMAII-1 + J) = '0'
                J2 = I
              ENDIF
 605        CONTINUE
          ENDIF
 600    CONTINUE
        IF (ZK8(JMAII-1 + NBMAL).NE.'0') THEN
          ZK8(JMAI2-1 + K2) = ZK8(JMAII-1 + NBMAL)
          K2 = K2 + 1
        ENDIF
        K2 = K2 - 1
C
        IF (K2.NE.NBMAL) THEN
           VALK(1) = MOTFAC
           VALK(2) = ZK8(JMAII-1 + J2)
           CALL U2MESK('E','RUPTURE0_70',2, VALK)
        ENDIF
C
C --- VERIFICATION COHERENCE LEVRE SUP / FOND
C
        CALL JEEXIN(RESU//'.FOND      .NOEU',IRET)
        IF(IRET.NE.0) THEN
          CALL JELIRA(RESU//'.FOND      .NOEU' , 'LONUTI', 
     &                                                  NBNOFF, K8B)
          CALL JEVEUO(RESU//'.FOND      .NOEU' ,'L',JNOE1)
        ELSE
          IF (MOTFAC .EQ. 'LEVRE_SUP') THEN
            CALL JELIRA(RESU//'.FOND_SUP  .NOEU' , 'LONUTI', 
     &                                                  NBNOFF, K8B)
            CALL JEVEUO(RESU//'.FOND_SUP  .NOEU' ,'L',JNOE1)
          ELSEIF (MOTFAC .EQ. 'LEVRE_INF') THEN
            CALL JELIRA(RESU//'.FOND_INF  .NOEU' , 'LONUTI',
     &                                                  NBNOFF, K8B)
            CALL JEVEUO(RESU//'.FOND_INF  .NOEU' ,'L',JNOE1)
          ENDIF
        ENDIF
        IF (NBNOFF.GT.1) THEN
          DO 610 I=1,NBNOFF
            COMPTA = 0
            DO 620 J=1,NBMAL
              CALL JENUNO(JEXNUM(NOMMAI,ZI(JADR-1 + J)),MAILLE)
              CALL JENONU (JEXNOM(NOMMAI,MAILLE),IBID)
              ITYP=IATYMA-1+IBID
              CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
              CALL DISMOI('F','NBNO_TYPMAIL',TYPE,'TYPE_MAILLE',
     &                                       NN,K8B,IRET)
              IF((TYPE(1:5).NE.'QUAD8').AND.(TYPE(1:5).NE.'TRIA3').AND.
     &         (TYPE(1:5).NE.'QUAD4').AND.(TYPE(1:5).NE.'TRIA6')) THEN
                VALK(1) = TYPE(1:5)
                VALK(2) = MOTFAC
                CALL U2MESK('F','RUPTURE0_65',2,VALK)
              ENDIF
              CALL JEVEUO(JEXNUM(CONEC,IBID),'L',IAMASE)
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',ZI(IAMASE)),NOEUG)
              DO 630 K=1,NN
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',ZI(IAMASE-1 + K)),
     &              NOEUG)
                IF(NOEUG.EQ.ZK8(JNOE1-1 + I)) THEN
                   COMPTA = COMPTA + 1
                   GOTO 610
                ENDIF   
 630           CONTINUE
 620        CONTINUE
            IF(COMPTA .EQ. 0)  THEN
               VALK(1) = ZK8(JNOE1-1 + I)
               VALK(2) = MOTFAC
               CALL U2MESK('F','RUPTURE0_72',2,VALK)
            ENDIF
 610      CONTINUE
        ENDIF
C
C
C --- DESTRUCTION DES OBJETS DE TRAVAIL
C
        CALL JEDETR (TRAV)
C
        CALL JEDETR('&&'//NOMPRO//'.MAIL')
C
  10  CONTINUE
C ----------------------------------------------------------
C    COMPARAISON LEVRE SUP / LEVRE INF AFIN DE S'ASSURER
C    QU'ELLES N'ONT PAS DE MAILLES EN COMMUN
C ----------------------------------------------------------
      CALL JEEXIN(RESU//'.LEVRESUP  .MAIL',IRET1)
      CALL JEEXIN(RESU//'.LEVREINF  .MAIL',IRET2)
      IF ((IRET1.NE.0).AND.(IRET2.NE.0)) THEN
        CALL JEVEUO ( RESU//'.LEVRESUP  .MAIL', 'L', JSUP )
        CALL JEVEUO ( RESU//'.LEVREINF  .MAIL', 'L', JINF )
        CALL JELIRA ( RESU//'.LEVRESUP  .MAIL', 'LONMAX',NBMAS1,K8B)
        CALL JELIRA ( RESU//'.LEVRESUP  .MAIL', 'LONMAX',NBMAS2,K8B)
        DO 710 I = 1,NBMAS1
             DO 715 J = 1,NBMAS2
              IF (ZK8(JSUP-1 + I) .EQ. ZK8(JINF-1 + J) ) THEN
                CALL U2MESK('F','RUPTURE0_73',1,ZK8(JSUP-1 + I))
              END IF
 715         CONTINUE
 710   CONTINUE
      ENDIF
9999  CONTINUE
C
C     LORSQUE LE FOND DE FISSURE EST DEFINI PAR FOND_INF ET FOND_SUP,
C     ON VERIFIE QUE LES NOEUDS SONT EN VIV A VIS
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JVALE)
      CALL JEEXIN(RESU//'.FOND_INF  .NOEU',IRET1)
      CALL JEEXIN(RESU//'.FOND_SUP  .NOEU',IRET2)
      IF(IRET1.NE.0 .AND. IRET2.NE.0)THEN
         CALL JEVEUO(RESU//'.FOND_INF  .NOEU','L',JFINF)
         CALL JEVEUO(RESU//'.FOND_SUP  .NOEU','L',JFSUP)
         CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JFINF)),JUFINF)
         CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JFINF+1)),JUINF2)
         D = ABS(ZR(JVALE+3*(JUFINF-1))-
     &               ZR(JVALE+3*(JUINF2-1)))
         D = D+ABS(ZR(JVALE+3*(JUFINF-1)+1)-
     &               ZR(JVALE+3*(JUINF2-1)+1))
         D = D+ABS(ZR(JVALE+3*(JUFINF-1)+2)-
     &               ZR(JVALE+3*(JUINF2-1)+2))
         PRECR = PREC*D
         DO 555 INO = 1 , NBNOFF
           CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JFINF-1+INO)),
     &                                                         JUFINF)
           CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JFSUP-1+INO)),
     &                                                         JUFSUP)
           D = ABS(ZR(JVALE+3*(JUFINF-1))-
     &               ZR(JVALE+3*(JUFSUP-1)))
           D = D+ABS(ZR(JVALE+3*(JUFINF-1)+1)-
     &               ZR(JVALE+3*(JUFSUP-1)+1))
           D = D+ABS(ZR(JVALE+3*(JUFINF-1)+2)-
     &               ZR(JVALE+3*(JUFSUP-1)+2))
           IF ( SQRT(D) .GT.PRECR)THEN
             VALK(1) = ZK8(JFINF+INO-1)
             VALK(2) = ZK8(JFSUP+INO-1)
             CALL U2MESK('F','RUPTURE0_69', 2 ,VALK)
           ENDIF
 555     CONTINUE
      ENDIF
C
      CALL JEDEMA()
C
      END
