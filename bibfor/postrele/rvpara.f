      SUBROUTINE RVPARA ( NOMRES )
      IMPLICIT   NONE
      CHARACTER*(*)       NOMRES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
C TOLE CRP_20
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
C     ------------------------------------------------------------------
C
C     INITIALISE LA TABLE DE POST_RELEVE_T
C
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       NBPOST, JCHEM, JCHAM, JRESU, JSENS, JNCMP, NCMP, I,
     +              JINVA, JPRIN, JMOME, JMAIL, JMOYE, J, JTRAD,
     +              JTRAN, IOCC, N1, N2, N3, JCMP, NBC, NUME, JFREQ,
     +              IRET, IBID, NBP, JINST, JORDR, JMODE, JABSC,
     +              JNOEU, N11, N12, N13, N14, N15, N16, N17, N18, N19,
     +              JNCAS, JANGL, JNOCP, NUMCMP, JNUCP, NBORDR, JNUME
      REAL*8        R8B
      COMPLEX*16    C16B
      CHARACTER*8   K8B, RESU, NOCMP(50), TYPARA(100), NOMCMP
      CHARACTER*16  K16B, NOMSY, TYSD
      CHARACTER*24  NOMOBJ, CHEXTR, NOPARA(100), KNUME
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETFAC ( 'ACTION', NBPOST )
C
      JABSC = 0
      JCHEM = 0
      JCHAM = 0
      JRESU = 0
      JSENS = 0
      JORDR = 0
      JMODE = 0
      JINST = 0
      JFREQ = 0
      JNCAS = 0
      JANGL = 0
      JNOCP = 0
      JNCMP = 0
      JINVA = 0
      JPRIN = 0
      JMOME = 0
      JNOEU = 0
      JMAIL = 0
      JMOYE = 0
      JTRAN = 0
      JTRAD = 0
      NCMP  = 0
C
      DO 100, IOCC = 1, NBPOST, 1
C
         CALL GETVID ( 'ACTION', 'CHEMIN', IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) JCHEM = JCHEM + 1
C
         CALL GETVID ( 'ACTION', 'CHAM_GD', IOCC,1,0, K8B, N2 )
         IF ( N2 .NE. 0 ) JCHAM = JCHAM + 1
C
         CALL GETVID ( 'ACTION', 'RESULTAT', IOCC,1,0, K8B, N3 )
         IF ( N3 .NE. 0 ) THEN
            JRESU = JRESU + 1
            CALL GETVID ( 'ACTION', 'RESULTAT', IOCC,1,1, K8B, N3 )
            CALL GETTCO ( K8B, TYSD )
            IF     ( TYSD .EQ. 'EVOL_ELAS'  .OR.
     +               TYSD .EQ. 'EVOL_THER'  .OR.
     +               TYSD .EQ. 'EVOL_NOLI'  .OR.
     +               TYSD .EQ. 'EVOL_CHAR'  .OR.
     +               TYSD .EQ. 'DYNA_TRANS' )  THEN
               JINST = JINST + 1
            ELSEIF ( TYSD .EQ. 'DYNA_HARMO'  .OR.
     +               TYSD .EQ. 'HARM_GENE'   .OR.
     +               TYSD .EQ. 'ACOU_HARMO'  )  THEN
               JFREQ = JFREQ + 1
            ELSEIF ( TYSD .EQ. 'MODE_MECA'   .OR.
     +               TYSD .EQ. 'MODE_GENE'   .OR.
     +               TYSD .EQ. 'MODE_ACOU'   .OR.
     +               TYSD .EQ. 'BASE_MODALE' )  THEN
               JFREQ = JFREQ + 1
               JMODE = JMODE + 1
            ELSEIF ( TYSD .EQ. 'MODE_STAT'   )  THEN
               JNOCP = JNOCP + 1
            ELSEIF ( TYSD .EQ. 'MULT_ELAS' )  THEN
               JNCAS = JNCAS + 1
            ELSEIF ( TYSD .EQ. 'FOURIER_ELAS' )  THEN
               JMODE = JMODE + 1
            ELSEIF ( TYSD .EQ. 'COMB_FOURIER' )  THEN
               JANGL = JANGL + 1
            ENDIF
         ENDIF
C
         CALL GETVID ( 'ACTION', 'LIST_ORDRE', IOCC,1,0, K8B, N11 )
         IF ( N11 .NE. 0 ) JORDR = JORDR + 1
C
         CALL GETVIS ( 'ACTION', 'NUME_ORDRE', IOCC,1,0, IBID, N12 )
         IF ( N12 .NE. 0 ) JORDR = JORDR + 1
C
         CALL GETVID ( 'ACTION', 'LIST_MODE', IOCC,1,0, K8B, N13 )
         IF ( N13 .NE. 0 ) JMODE = JMODE + 1
C
         CALL GETVIS ( 'ACTION', 'NUME_MODE', IOCC,1,0, IBID, N14 )
         IF ( N14 .NE. 0 ) JMODE = JMODE + 1
C
         CALL GETVID ( 'ACTION', 'LIST_INST', IOCC,1,0, K8B, N15 )
         IF ( N15 .NE. 0 ) JINST = JINST + 1
C
         CALL GETVR8 ( 'ACTION', 'INST', IOCC,1,0, R8B, N16 )
         IF ( N16 .NE. 0 ) JINST = JINST + 1
C
         CALL GETVID ( 'ACTION', 'LIST_FREQ', IOCC,1,0, K8B, N17 )
         IF ( N17 .NE. 0 ) JFREQ = JFREQ + 1
C
         CALL GETVR8 ( 'ACTION', 'FREQ', IOCC,1,0, R8B, N18 )
         IF ( N18 .NE. 0 ) JFREQ = JFREQ + 1
C
         CALL GETVID ( 'ACTION', 'SENSIBILITE', IOCC,1,0, K8B, N19 )
         IF ( N19 .NE. 0 ) JSENS = JSENS + 1
C
         IF ((N2+N11+N12+N13+N14+N15+N16+N17+N18).EQ.0) JORDR = JORDR+1
C
         CALL GETVTX ( 'ACTION', 'TOUT_CMP', IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            JNCMP = JNCMP  + 1
            NOMOBJ = '&&RVPARA.NCMP'
            IF ( N2 .NE. 0 ) THEN
               CALL GETVID ( 'ACTION', 'CHAM_GD', IOCC,1,1, NOMSY, N2 )
               CALL UTNCMP ( NOMSY, NBC, NOMOBJ )
            ELSE
               CALL GETVID ( 'ACTION', 'RESULTAT', IOCC,1,1, RESU , N3 )
               CALL GETVTX ( 'ACTION', 'NOM_CHAM', IOCC,1,1, NOMSY, N1 )

               CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,
     +                                                 NBORDR,1,IBID)
               KNUME = '&&RVPARA.NUME_ORDRE'
               CALL WKVECT ( KNUME , 'V V I', NBORDR, JNUME )
               CALL RSORAC(RESU,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     +                                        ZI(JNUME),NBORDR,IBID)
               DO 14 I = 1 , NBORDR
                  NUME = ZI(JNUME+I-1)
                  CALL RSEXCH ( RESU, NOMSY, NUME, CHEXTR, IRET )
                  IF ( IRET .EQ. 0 ) GOTO 16
 14            CONTINUE
               CALL UTMESS('F','RVPARA','PAS DE CHAMP TROUVE ')
               CALL UTIMPK('S','POUR L''OPTION ',1,NOMSY)
               CALL UTFINM()
 16            CONTINUE
               CALL JEDETR ( KNUME )
               CALL UTNCMP ( CHEXTR, NBC, NOMOBJ )
            ENDIF
            IF (NBC.EQ.0) CALL UTMESS('F','RVPARA','Y A UN BUG 2')
            CALL JEVEUO ( NOMOBJ, 'L', JCMP )
            DO 10 I = 1 , NBC
               DO 12 J = 1 , NCMP
                  IF ( NOCMP(J) .EQ. ZK8(JCMP+I-1) ) GOTO 10
 12            CONTINUE
               NCMP = NCMP + 1
               NOCMP(NCMP) = ZK8(JCMP+I-1)
 10         CONTINUE
            CALL JEDETR ( NOMOBJ )
         ENDIF
C
         CALL GETVTX ( 'ACTION', 'NOM_CMP', IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
C
            CALL GETVTX ( 'ACTION', 'TRAC_NORMALE', IOCC,1,0, K8B, N11)
            IF ( N11 .NE. 0 ) JTRAN = JTRAN  + 1
            CALL GETVTX ( 'ACTION', 'TRAC_NOR'    , IOCC,1,0, K8B, N12)
            IF ( N12 .NE. 0 ) JTRAN = JTRAN  + 1
C
            CALL GETVTX ( 'ACTION', 'TRAC_DIRECTION', IOCC,1,0, K8B,N13)
            IF ( N13 .NE. 0 ) JTRAD = JTRAD  + 1
            CALL GETVTX ( 'ACTION', 'TRAC_DIR'      , IOCC,1,0, K8B,N14)
            IF ( N14 .NE. 0 ) JTRAD = JTRAD  + 1
C
            IF ( (N11+N12+N13+N14) .NE. 0 ) GOTO 24
            JNCMP = JNCMP  + 1
            NBC = -N1
            CALL WKVECT ( '&&RVPARA.NCMP', 'V V K8', NBC, JCMP )
            CALL GETVTX ( 'ACTION', 'NOM_CMP', IOCC,1,NBC, ZK8(JCMP),N1)
C           CALL GETVIS ( 'ACTION', 'NUME_CMP', IOCC,1,0, IBID,N11)
            N11=0
            IF ( N11 .NE. 0 ) THEN
               NUMCMP = -N11
               CALL WKVECT ( '&&RVPARA.NU_CMP', 'V V I', NUMCMP, JNUCP )
C           CALL GETVIS('ACTION','NUME_CMP',IOCC,1,NUMCMP,ZI(JNUCP),N11)
            N11=0
               IF (ZK8(JCMP)(1:4).EQ.'VARI') THEN
                  IF (NBC.NE.1) CALL UTMESS('F','RVPARA','Y A UN BUG 3')
                  DO 120 I = 1 , NUMCMP
                     CALL CODENT ( ZI(JNUCP+I-1), 'G', K8B )
                     NOMCMP = 'VARI_'//K8B(1:3)
                     DO 122 J = 1 , NCMP
                        IF ( NOCMP(J) .EQ. NOMCMP ) GOTO 120
 122                 CONTINUE
                     NCMP = NCMP + 1
                     NOCMP(NCMP) = NOMCMP
 120              CONTINUE
               ELSE
                  DO 124 I = 1 , NBC
                     DO 126 J = 1 , NCMP
                        IF ( NOCMP(J) .EQ. ZK8(JCMP+I-1) ) GOTO 124
 126                 CONTINUE
                     NCMP = NCMP + 1
                     NOCMP(NCMP) = ZK8(JCMP+I-1)
 124              CONTINUE
               ENDIF
               CALL JEDETR ( '&&RVPARA.NU_CMP' )
            ELSE
               DO 20 I = 1 , NBC
                  DO 22 J = 1 , NCMP
                     IF ( NOCMP(J) .EQ. ZK8(JCMP+I-1) ) GOTO 20
 22               CONTINUE
                  NCMP = NCMP + 1
                  NOCMP(NCMP) = ZK8(JCMP+I-1)
 20            CONTINUE
            ENDIF
            CALL JEDETR ( '&&RVPARA.NCMP' )
         ENDIF
 24      CONTINUE
C
         CALL GETVTX ( 'ACTION', 'ELEM_PRINCIPAUX', IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) JPRIN = JPRIN + 1
C
         CALL GETVTX ( 'ACTION', 'RESULTANTE', IOCC,1,0, K8B, N1 )
         CALL GETVTX ( 'ACTION', 'MOMENT'    , IOCC,1,0, K8B, N2 )
         IF ( (N1 .NE. 0) .AND. (N2 .NE. 0) ) JMOME = JMOME + 1
         IF ( (N1 .NE. 0) .AND. (N2 .EQ. 0) ) THEN
            JNCMP = JNCMP  + 1
            NBC = -N1
            CALL WKVECT ( '&&RVPARA.NCMP', 'V V K8', NBC, JCMP )
            CALL GETVTX ('ACTION','RESULTANTE',IOCC,1,NBC,ZK8(JCMP),N1)
            DO 30 I = 1 , NBC
               DO 32 J = 1 , NCMP
                  IF ( NOCMP(J) .EQ. ZK8(JCMP+I-1) ) GOTO 30
 32            CONTINUE
               NCMP = NCMP + 1
               NOCMP(NCMP) = ZK8(JCMP+I-1)
 30         CONTINUE
            CALL JEDETR ( '&&RVPARA.NCMP' )
         ENDIF
C
         CALL GETVTX ( 'ACTION', 'OPERATION', IOCC,1,1, K16B, N3 )
         IF ( K16B .EQ. 'MOYENNE' ) JMOYE = JMOYE + 1
         IF ( K16B .EQ. 'EXTRACTION' ) THEN
C
            CALL GETVTX ( 'ACTION', 'INVARIANT', IOCC,1,0, K8B, N2 )
            IF ( N2 .NE. 0 ) JINVA = JINVA + 1
C
            IF ( N1 .EQ. 0 ) JABSC = JABSC + 1
C
            CALL GETVID ( 'ACTION', 'NOEUD', IOCC,1,0, K8B, N2 )
            IF ( (N1 .EQ. 0) .AND. (N2 .NE. 0) ) JNOEU = JNOEU + 1
C
            CALL GETVID ( 'ACTION', 'GROUP_NO', IOCC,1,0, K8B, N2 )
            IF ( (N1 .EQ. 0) .AND. (N2 .NE. 0) ) JNOEU = JNOEU + 1
         ENDIF
C
         CALL GETVTX ( 'ACTION', 'MOYE_NOEUD', IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL GETVTX ( 'ACTION', 'MOYE_NOEUD', IOCC,1,1, K8B, N1 )
            IF ( K8B(1:3) .EQ. 'NON' ) JMAIL = JMAIL + 1
         ENDIF
C
 100  CONTINUE
C
      NBP = 1
      NOPARA(NBP) = 'INTITULE'
      TYPARA(NBP) = 'K16'
      IF ( JCHEM .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'CHEMIN'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'SEGMENT'
         TYPARA(NBP) = 'I'
         NBP = NBP + 1
         NOPARA(NBP) = 'CMP_CNX'
         TYPARA(NBP) = 'I'
      ENDIF
      IF ( JNOEU .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NOEUD'
         TYPARA(NBP) = 'K8'
      ENDIF
      IF ( JCHAM .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'CHAM_GD'
         TYPARA(NBP) = 'K8'
      ENDIF
      IF ( JRESU .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'RESU'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'NOM_CHAM'
         TYPARA(NBP) = 'K16'
      ENDIF
      IF ( JSENS .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'PAR_SENS'
         TYPARA(NBP) = 'K8'
      ENDIF
      IF ( JORDR .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NUME_ORDRE'
         TYPARA(NBP) = 'I'
      ENDIF
      IF ( JMODE .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NUME_MODE'
         TYPARA(NBP) = 'I'
      ENDIF
      IF ( JINST .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'INST'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JFREQ .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'FREQ'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JNCAS .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NOM_CAS'
         TYPARA(NBP) = 'K16'
      ENDIF
      IF ( JNOCP .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NOEUD_CMP'
         TYPARA(NBP) = 'K16'
      ENDIF
      IF ( JANGL .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'ANGL'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JMAIL .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'MAILLE'
         TYPARA(NBP) = 'K8'
      ENDIF
      IF ( JABSC .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'ABSC_CURV'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'COOR_X'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'COOR_Y'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'COOR_Z'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JNCMP .NE. 0 ) THEN
         DO 40 I = 1 , NCMP
            NBP = NBP + 1
            NOPARA(NBP) = NOCMP(I)
            TYPARA(NBP) = 'R'
 40      CONTINUE
      ENDIF
      IF ( JINVA .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'VMIS'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TRESCA'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TRACE'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'DETER'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JPRIN .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'PRIN_1'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'PRIN_2'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'PRIN_3'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_1_X'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_1_Y'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_1_Z'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_2_X'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_2_Y'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_2_Z'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_3_X'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_3_Y'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'VECT_3_Z'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JMOME .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'RESULT_X'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'RESULT_Y'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'RESULT_Z'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'MOMENT_X'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'MOMENT_Y'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'MOMENT_Z'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JTRAN .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'TRAC_NOR'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TR_NOR_1'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TR_NOR_2'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TR_NOR_3'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JTRAD .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'TRAC_DIR'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TR_DIR_1'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TR_DIR_2'
         TYPARA(NBP) = 'R'
         NBP = NBP + 1
         NOPARA(NBP) = 'TR_DIR_3'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( JMOYE .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'QUANTITE'
         TYPARA(NBP) = 'K16'
      ENDIF
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NBP, NOPARA, TYPARA )
C
      CALL JEDEMA ( )
      END
