      SUBROUTINE FOLIE5 ( SPECTR, SORTIE )
      IMPLICIT  NONE
      CHARACTER*(*)       SPECTR, SORTIE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     VERIFIE QUE LA FONCTION EN SORTIE EST SUPERIEUR AU SPECTRE
C
C  IN : SPECTR : NOM DU SPECTRE BRUT
C  IN : SORTIE : NOM DE LA FONCTION EN SORTIE
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       I, LPRO, NBVAL, NBFRB, NBFRL, JSPEC, JSORT, IER,
     +              NBPAR, IPARA, LPAR
      LOGICAL       BUG
      REAL*8        F1, F2, FR, ACBR, ACLI, VALEP(2)
      CHARACTER*8   K8B
      CHARACTER*16  TYPFON, CONCEP, NOMCMD, NOPARA(2)
      CHARACTER*24  PROL, VALE, PARA
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      CALL GETRES ( K8B, CONCEP, NOMCMD )
C
C --- RECUPERATION DE L'ACCELEROGRAMME ---
C
      PROL( 1:19) = SPECTR
      PROL(20:24) = '.PROL'
      CALL JEVEUO ( PROL, 'L', LPRO )
      TYPFON = ZK16(LPRO)
C
      IF (TYPFON.EQ.'FONCTION') THEN
C         --------------------
         BUG = .FALSE.
         VALE( 1:19) = SPECTR
         VALE(20:24) = '.VALE'
         CALL JELIRA ( VALE, 'LONMAX', NBVAL, K8B )
         CALL JEVEUO ( VALE, 'L', JSPEC )
         NBFRB = NBVAL / 2
C
         VALE( 1:19) = SORTIE
         VALE(20:24) = '.VALE'
         CALL JELIRA ( VALE, 'LONMAX', NBVAL, K8B )
         CALL JEVEUO ( VALE, 'L', JSORT )
         NBFRL = NBVAL / 2
         F1 = ZR(JSORT)
         F2 = ZR(JSORT+NBFRL-1)
C
         DO 10 I = 1 , NBFRB
            FR = ZR(JSPEC+I-1)
            ACBR = ZR(JSPEC+NBFRB+I-1)
            IF ( FR .LT. F1 ) GOTO 10
            IF ( FR .GT. F2 ) GOTO 10
            CALL FOINTE ( 'F', SORTIE, 1, ZK16(LPRO+2), FR, ACLI, IER )
            IF ( ABS(ACLI-ACBR) .LT. 1.D-09 ) GOTO 10
            IF ( ACLI .LT. ACBR ) THEN
               IF (.NOT.BUG) THEN
                  CALL UTDEBM('A','FOLIE5',
     +            '*** SPECTRE LISSE INFERIEUR AU SPECTRE BRUT ***')
               ENDIF
               CALL UTIMPR('L','POUR LA FREQUENCE ',1,FR)
               CALL UTIMPR('L','     ACCELERATION LISSEE ', 1, ACLI)
               CALL UTIMPR('L','     ACCELERATION INITIALE ', 1, ACBR)
               BUG = .TRUE.
            ENDIF
 10      CONTINUE      
         IF ( BUG )  CALL UTFINM()
C
C
      ELSEIF (TYPFON.EQ.'NAPPE   ') THEN
C             --------------------
         PARA( 1:19) = SPECTR
         PARA(20:24) = '.PARA'
         CALL JELIRA ( PARA, 'LONUTI', NBPAR, K8B )
         CALL JEVEUO ( PARA, 'L', LPAR )
C
         NOPARA(1) = ZK16(LPRO+2)
         NOPARA(2) = ZK16(LPRO+5)
C
         DO 20 IPARA = 1 , NBPAR
C
            BUG = .FALSE.
            VALEP(1) = ZR(LPAR+IPARA-1)
C
            VALE( 1:19) = SORTIE
            VALE(20:24) = '.VALE'
            CALL JELIRA ( JEXNUM(VALE,IPARA), 'LONUTI', NBVAL, K8B )
            CALL JEVEUO ( JEXNUM(VALE,IPARA), 'L', JSORT )
            NBFRL = NBVAL / 2
            F1 = ZR(JSORT)
            F2 = ZR(JSORT+NBFRL-1)
C
            VALE( 1:19) = SPECTR
            VALE(20:24) = '.VALE'
            CALL JELIRA ( JEXNUM(VALE,IPARA), 'LONUTI', NBVAL, K8B )
            CALL JEVEUO ( JEXNUM(VALE,IPARA), 'L', JSPEC )
            NBFRB = NBVAL / 2
C
            DO 22 I = 1 , NBFRB
               VALEP(2) = ZR(JSPEC+I-1)
               ACBR = ZR(JSPEC+NBFRB+I-1)
               IF ( VALEP(2) .LT. F1 ) GOTO 22
               IF ( VALEP(2) .GT. F2 ) GOTO 22
               CALL FOINTE ('F',SORTIE,2,NOPARA,VALEP,ACLI,IER)
               IF ( ABS(ACLI-ACBR) .LT. 1.D-09 ) GOTO 22
               IF ( ACLI .LT. ACBR ) THEN
                  IF (.NOT.BUG) THEN
                     CALL UTDEBM('A','FOLIE5',
     +            '*** SPECTRE LISSE INFERIEUR AU SPECTRE BRUT ***')
                     CALL UTIMPR('S',' AMORTISSEMENT: ',1,VALEP(1))
                  ENDIF
                  CALL UTIMPR('L','POUR LA FREQUENCE ',1,VALEP(2))
                  CALL UTIMPR('L','     ACCELERATION LISSEE ', 1, ACLI)
                CALL UTIMPR('L','     ACCELERATION INITIALE ', 1, ACBR)
                  BUG = .TRUE.
               ENDIF
 22         CONTINUE      
            IF ( BUG )  CALL UTFINM()
C
 20      CONTINUE 
C
      ELSE
         CALL UTMESS('F','FOLIE5','TYPE FONTION NON TRAITEE '//TYPFON)
      ENDIF
C
      CALL JEDEMA()
      END
