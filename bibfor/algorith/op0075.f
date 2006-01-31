      SUBROUTINE OP0075 (IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            IER
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C-----------------------------------------------------------------------
C
C     OPERATEUR REST_BASE_PHYS
C
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
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
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ---------------------------
C
      CHARACTER*8  K8B, NOMRES, RESIN, NOMSST, MAILSK, MODE 
      CHARACTER*8  K8BID,BID,RESULT,BLANC
      CHARACTER*16 CONCEP, NOMCMD, TYPRES, TYPMAT, TYPREP, CHAMP(4)
      CHARACTER*19 PROFNO
      CHARACTER*24 MATGEN, NUMGEN
      LOGICAL      PROMES
      INTEGER      IOC1
C
C     -----------------------------------------------------------------
      DATA K8B    /'        '/
      CALL JEMARQ()
      CALL INFMAJ()
      K8BID =  '        '
      BLANC =  '        '
C
C     -----------------------------------------------------------------
C
C
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C --- PHASE DE TEST SUR LES CHAMPS A RESTITUER
      CALL GETVTX(' ','NOM_CHAM',1,1,4,CHAMP,NBCHAM)
      IF (NBCHAM .LT.0 ) THEN
         CALL UTMESS('E','OP0075','TROP D''ARGUMENTS POUR "NOM_CHAM"')
      ELSE
         DO 1 I=1,NBCHAM
            DO 2 J=I+1,NBCHAM
               IF ( CHAMP(I).EQ. CHAMP(J) ) THEN
                  CALL UTMESS('E','OP0075','ARGUMENT EN DOUBLE POUR'//
     +                                    ' "NOM_CHAM"')
               ENDIF
    2       CONTINUE
            IF ( CHAMP(I).EQ. 'ACCE_ABSOLU' ) THEN
               CALL GETVID(' ','ACCE_MONO_APPUI',1,1,1,K8B,N1)
               IF ( N1 .EQ. 0 ) THEN
                  CALL UTMESS('E','OP0075','POUR CALCULER UNE '//
     +                    'ACCE_ABSOLU, IL FAUT "ACCE_MONO_APPUI"')
               ENDIF
            ENDIF
    1    CONTINUE
      ENDIF
C

C --- CREATION DU .REFN DU PROFIL :
C     ---------------------------
      PROFNO = NOMRES//'.PROFC.NUME'
      CALL WKVECT(PROFNO(1:19)//'.REFN','G V K24',2,LDREF)
      ZK24(LDREF+1)='DEPL_R'
C

C --- LE RESULTAT EST-IL GENERALISE OU PAS :
C     ---------------------------
      CALL GETVID(' ','RESULTAT',1,1,0,K8BID,IR)
      IF (IR .EQ. 0) THEN
         CALL GETVID(' ','RESU_GENE',1,1,1,RESIN,IBID)
         CALL GETTCO(RESIN,CONCEP)
      ELSE
C      --- PROJECTION RESULTAT SUR UN SQUELETTE ENRICHI ---
         CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,IBID)
         CALL GETVID(' ','RESULTAT',1,1,1,RESULT,IBID)
         ZK24(LDREF) = MAILSK
         CALL GETFAC('CYCLIQUE',IOC1)
         IF ( IOC1 .GT. 0 ) THEN
           CALL EXCYGL(NOMRES,TYPRES,RESULT,MAILSK,PROFNO)
           CALL JEVEUO(PROFNO(1:19)//'.REFN','E',LDREFB)
           ZK24(LDREFB) = MAILSK
           ZK24(LDREFB+1) = 'DEPL_R'
           CONCEP(1:9) = '         '
           GO TO 9999
         ELSE
           IF (TYPRES.EQ.'MODE_MECA') THEN
             CALL REGRES(NOMRES,MAILSK,RESULT)
             CALL JEVEUO(PROFNO(1:19)//'.REFN','E',LDREFB)
             ZK24(LDREFB) = MAILSK
             ZK24(LDREFB+1) = 'DEPL_R'
             CONCEP(1:9) = '         '
             GO TO 9999
           ELSE
             CALL UTMESS('E','OP0075','POUR RESTITUER SUR '//
     +          'UN SQUELETTE, IL FAUT "MODE_MECA"')
           ENDIF
         ENDIF
      ENDIF

C INDICATEUR CALCUL SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
      PROMES = .FALSE.
      IF ((CONCEP(1:9).EQ.'TRAN_GENE').OR.
     &    (CONCEP(1:9).EQ.'MODE_GENE') .OR.
     &    (CONCEP(1:9).EQ.'HARM_GENE')) THEN
        CALL JEVEUO(RESIN//'           .REFD','L',J1REFE)
        MATGEN = ZK24(J1REFE)
        NUMGEN = ZK24(J1REFE+3)
C LE RESU_GENE VIENT DE PROJ_MESU_MODAL
        IF ((MATGEN(1:8) .EQ. BLANC).AND.
     &      (NUMGEN(1:8) .EQ. BLANC)) THEN 
          PROMES = .TRUE.
          TYPREP=BLANC
        ELSE
          IF (NUMGEN(1:8) .EQ. BLANC) THEN
            CALL JEVEUO(MATGEN(1:8)//'           .REFA','L',J2REFE)
            NUMGEN = ZK24(J2REFE+1)(1:14)
          ENDIF
          CALL JEVEUO(NUMGEN(1:14)//'.NUME.REFN','L',J3REFE)
          CALL GETTCO(ZK24(J3REFE),TYPREP)
        ENDIF
      ENDIF
C
C     --- DYNAMIQUE TRANSITOIRE ---
C
      IF (CONCEP(1:9).EQ.'TRAN_GENE') THEN
C
        IF (PROMES) THEN
            CALL TRAN75(NOMRES,TYPRES,RESIN,NOMCMD,K8B)
C --- CAS DU CALCUL TRANSITOIRE CLASSIQUE
        ELSE
C --- CAS DE LA SOUS-STRUCTURATION TRANSITOIRE
          IF (TYPREP(1:11).EQ.'MODELE_GENE') THEN
            CALL GETVID(' ','SQUELETTE',1,1,0,K8B,ISK)
            IF (ISK.EQ.0) THEN
              CALL GETVTX(' ','SOUS_STRUC',1,1,1,NOMSST,IBID)
              CALL RETREC(NOMRES,RESIN,NOMSST)
            ELSE
              CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,IBID)
              PROFNO = NOMRES//'.PROFC.NUME'
              CALL RETRGL(NOMRES,RESIN,MAILSK,PROFNO)
              CALL JEVEUO(PROFNO(1:19)//'.REFN','E',LDREFB)
              ZK24(LDREFB) = MAILSK
              ZK24(LDREFB+1) = 'DEPL_R'
            ENDIF
C --- CAS SANS LA SOUS-STRUCTURATION TRANSITOIRE
          ELSEIF ((TYPREP(1:9).EQ.'MODE_MECA').OR.
     +           (TYPREP(1:9).EQ.'MODE_STAT').OR.
     +           (TYPREP(1:11).EQ.'BASE_MODALE')) THEN
            CALL TRAN75(NOMRES,TYPRES,RESIN,NOMCMD,K8BID)
C
          ELSEIF (TYPREP(1:9).EQ.'MODE_GENE') THEN
            CALL GETVTX(' ','SOUS_STRUC',1,1,1,NOMSST,N1)
            CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,N2)
            IF ((N1.NE.0.AND.N2.NE.0).OR.(N1.NE.0).OR.(N2.NE.0)) THEN
              CALL UTMESS('F','OP0075','MOTS-CLES''SOUS_STRUC'' '//
     +                    'ET''SQUELETTE''INTERDITS')
            ENDIF
            CALL GETVID(' ','MODE_MECA',1,1,1,MODE,IBID)
            IF (IBID.EQ.0) THEN
              CALL UTMESS('F','OP0075','MOTS-CLE''MODE_MECA'' '//
     +                    'DOIT ETRE PRESENT')
            ENDIF
            CALL TRAN75(NOMRES,TYPRES,RESIN,NOMCMD,MODE)
          ENDIF
C
C
        ENDIF
C
C     --- CALCUL MODAL PAR SOUS-STRUCTURATION CLASSIQUE ---
C                  OU SANS SOUS-STRUCTURATION
C
      ELSEIF(CONCEP(1:9).EQ.'MODE_GENE') THEN
        IF (PROMES) THEN
          CALL REGENE(NOMRES,RESIN)
        ELSE
C --- CAS DE LA SOUS-STRUCTURATION MODALE
          IF (TYPREP(1:11).EQ.'MODELE_GENE') THEN
             CALL GETVID(' ','SQUELETTE',1,1,0,K8B,ISK)
             IF (ISK.EQ.0) THEN
                CALL GETVTX(' ','SOUS_STRUC',1,1,1,NOMSST,IBID)
                CALL REGEEC(NOMRES,RESIN,NOMSST)
             ELSE
                CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,IBID)
                PROFNO = NOMRES//'.PROFC.NUME'
                CALL REGEGL(NOMRES,RESIN,MAILSK,PROFNO)
                CALL JEVEUO(PROFNO(1:19)//'.REFN','E',LDREFB)
                ZK24(LDREFB) = MAILSK
                ZK24(LDREFB+1) = 'DEPL_R'
             ENDIF
          ELSE
C     --- CALCUL MODAL SANS SOUS-STRUCTURATION ---
             CALL REGENE(NOMRES,RESIN)
          ENDIF
        ENDIF    
C
C     --- CALCUL MODAL PAR SOUS-STYRUCTURATION CYCLIQUE ---
C
      ELSEIF (CONCEP(1:9).EQ.'MODE_CYCL') THEN
         CALL GETVID(' ','SQUELETTE',1,1,0,K8B,ISK)
         IF (ISK.EQ.0) THEN
            CALL GETVIS(' ','SECTEUR',1,1,1,NUMSEC,IBID)
            CALL RECYEC(NOMRES,RESIN,NUMSEC,'MODE_MECA')
         ELSE
            CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,IBID)
            PROFNO = NOMRES//'.PROFC.NUME'
            CALL RECYGL(NOMRES,'MODE_MECA',RESIN,MAILSK,PROFNO)
            CALL JEVEUO(PROFNO(1:19)//'.REFN','E',LDREFB)
            ZK24(LDREFB) = MAILSK
            ZK24(LDREFB+1) = 'DEPL_R'
         ENDIF
C
C     --- CALCUL HARMONIQUE PAR SOUS-STRUCTURATION CLASSIQUE ---
C
      ELSEIF (CONCEP(1:9).EQ.'HARM_GENE') THEN
        IF (PROMES) THEN
          CALL HARM75(NOMRES,TYPRES,RESIN,NOMCMD,K8BID) 
        ELSE
C --- CAS DE LA SOUS-STRUCTURATION HARMONIQUE
          IF (TYPREP(1:11).EQ.'MODELE_GENE') THEN
            CALL GETVID(' ','SQUELETTE',1,1,0,K8B,ISK)
            IF (ISK.EQ.0) THEN
             CALL GETVTX(' ','SOUS_STRUC',1,1,1,NOMSST,IBID)
             CALL REHAEC(NOMRES,RESIN,NOMSST)
            ELSE
              CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,IBID)
              PROFNO = NOMRES//'.PROFC.NUME'
              CALL REHAGL(NOMRES,RESIN,MAILSK,PROFNO)
              CALL JEVEUO(PROFNO(1:19)//'.REFN','E',LDREFB)
              ZK24(LDREFB) = MAILSK
              ZK24(LDREFB+1) = 'DEPL_R'
            ENDIF
C     --- CALCUL HARMONIQUE SANS SOUS-STRUCTURATION ---
          ELSEIF ((TYPREP(1:9).EQ.'MODE_MECA').OR.
     +          (TYPREP(1:9).EQ.'MODE_STAT').OR.
     +          (TYPREP(1:11).EQ.'BASE_MODALE')) THEN
            CALL HARM75(NOMRES,TYPRES,RESIN,NOMCMD,K8BID)
          ELSEIF (TYPREP(1:9).EQ.'MODE_GENE') THEN
            CALL GETVTX(' ','SOUS_STRUC',1,1,1,NOMSST,N1)
            CALL GETVID(' ','SQUELETTE',1,1,1,MAILSK,N2)
            IF ((N1.NE.0.AND.N2.NE.0)) THEN
              CALL UTMESS('F','OP0075','MOTS-CLES''SOUS_STRUC'' '//
     +                    'ET''SQUELETTE''INTERDITS')
            ENDIF
            CALL GETVID(' ','MODE_MECA',1,1,1,MODE,IBID)
            IF (IBID.EQ.0) THEN
              CALL UTMESS('F','OP0075','MOTS-CLE''MODE_MECA'' '//
     +                'DOIT ETRE PRESENT')
            ENDIF
            CALL HARM75(NOMRES,TYPRES,RESIN,NOMCMD,MODE)
          ENDIF
        ENDIF
C
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
