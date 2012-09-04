      SUBROUTINE RVPARA ( NOMTAB,MCF, NBPOST )
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C IN  NOMTAB  : NOM DE LA TABLE PRINCIPALE PRODUITE PAR LA COMMANDE
C IN  MCF     : MOT-CLE FACTEUR
C IN  NBPOST  : NOMBRE DE POST-TRAITEMENT A CONSIDERER
C ----------------------------------------------------------------------
C     INITIALISE LA TABLE DE POST_RELEVE_T ASSOCIEE A LA TABLE DE
C     REFERENCE NOMTAB
C     ------------------------------------------------------------------
C
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      INCLUDE 'jeveux.h'
      CHARACTER*6 MCF
      CHARACTER*8 NOMTAB
      INTEGER NBPOST
C
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'RVPARA' )
C
      INTEGER IFM, NIV
      INTEGER IOCC, IBID
      INTEGER JCHEM, JCHAM, JRESU, JNCMP, NCMP, I
      INTEGER JINVA, JPRIN, JMOME, JMAIL, JMOYE, J, JTRAD
      INTEGER JTRAN, N1, N2, N3, JCMP1, JCMP2, JCMP3, NBC, NUME
      INTEGER IRET, NBP, JINST, JORDR, JMODE, JABSC, JFREQ
      INTEGER JNOEU, N11, N12, N13, N14, N15, N16, N17, N18
      INTEGER JNCAS, JANGL, JNOCP, NUMCMP, JNUCP, NBORDR, JNUME
      REAL*8        R8B
      LOGICAL       LMIMA, LMOYE, LEXTR, LMOYGR
      COMPLEX*16    C16B
      CHARACTER*8   K8B, RESU, TYPARA(100), NOMCMP
      CHARACTER*16  K16B, NOMSY, TYSD
      CHARACTER*24  NOMOBJ, CHEXTR, NOPARA(100), KNUME
      CHARACTER*24 VALK(3)
      CHARACTER*24  K24BID
C
      CHARACTER*24  NOCMP
      INTEGER      JNOCMP,NCMPMX
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ ( )
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
      IF ( NIV.GE.2 ) CALL U2MESK('I','POSTRELE_8',1,NOMTAB)
C
      NOCMP  = '&&'//NOMPRO//'_NOM_CMP_TABLE  '
      NCMPMX = 100
      CALL WKVECT(NOCMP ,'V V K8',NCMPMX,JNOCMP)
C
      JABSC = 0
      JCHEM = 0
      JCHAM = 0
      JRESU = 0
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
C====
C 2. ON PARCOURT TOUTES LES ACTIONS DEMANDEES
C====
C
      DO 2, IOCC = 1, NBPOST
C
C 2.1. ==> ON CHERCHE SI C'EST LA BONNE TABLE
C
        CALL GETVID ( MCF, 'RESULTAT', IOCC,IARG,0, K8B, N3 )


C
         CALL GETVID ( MCF, 'CHEMIN', IOCC,IARG,0, K8B, N1 )
         IF ( N1 .NE. 0 ) JCHEM = JCHEM + 1
C
         CALL GETVID ( MCF, 'CHAM_GD', IOCC,IARG,0, K8B, N2 )
         IF ( N2 .NE. 0 ) JCHAM = JCHAM + 1
C
         IF ( N3 .NE. 0 ) THEN
            JRESU = JRESU + 1
            CALL GETVID ( MCF, 'RESULTAT', IOCC,IARG,1, K8B, N3 )
            CALL GETTCO ( K8B, TYSD )
            IF     ( TYSD .EQ. 'EVOL_ELAS'  .OR.
     &               TYSD .EQ. 'EVOL_THER'  .OR.
     &               TYSD .EQ. 'EVOL_NOLI'  .OR.
     &               TYSD .EQ. 'EVOL_CHAR'  .OR.
     &               TYSD .EQ. 'DYNA_TRANS' )  THEN
               JINST = JINST + 1
            ELSEIF ( TYSD .EQ. 'DYNA_HARMO'  .OR.
     &               TYSD .EQ. 'HARM_GENE'   .OR.
     &               TYSD .EQ. 'ACOU_HARMO'  )  THEN
               JFREQ = JFREQ + 1
            ELSEIF ( TYSD .EQ. 'MODE_MECA'   .OR.
     &               TYSD .EQ. 'MODE_GENE'   .OR.
     &               TYSD .EQ. 'MODE_ACOU'   )  THEN
               JFREQ = JFREQ + 1
               JMODE = JMODE + 1
               JNOCP = JNOCP + 1
            ELSEIF ( TYSD .EQ. 'MULT_ELAS' )  THEN
               JNCAS = JNCAS + 1
            ELSEIF ( TYSD(1:8) .EQ. 'FOURIER_' )  THEN
               JMODE = JMODE + 1
            ELSEIF ( TYSD .EQ. 'COMB_FOURIER' )  THEN
               JANGL = JANGL + 1
            ENDIF
         ENDIF
C
         CALL GETVID ( MCF, 'LIST_ORDRE', IOCC,IARG,0, K8B, N11 )
         IF ( N11 .NE. 0 ) JORDR = JORDR + 1
C
         CALL GETVIS ( MCF, 'NUME_ORDRE', IOCC,IARG,0, IBID, N12 )
         IF ( N12 .NE. 0 ) JORDR = JORDR + 1
C
         CALL GETVID ( MCF, 'LIST_MODE', IOCC,IARG,0, K8B, N13 )
         IF ( N13 .NE. 0 ) JMODE = JMODE + 1
C
         CALL GETVIS ( MCF, 'NUME_MODE', IOCC,IARG,0, IBID, N14 )
         IF ( N14 .NE. 0 ) JMODE = JMODE + 1
C
         CALL GETVID ( MCF, 'LIST_INST', IOCC,IARG,0, K8B, N15 )
         IF ( N15 .NE. 0 ) JINST = JINST + 1
C
         CALL GETVR8 ( MCF, 'INST', IOCC,IARG,0, R8B, N16 )
         IF ( N16 .NE. 0 ) JINST = JINST + 1
C
         CALL GETVID ( MCF, 'LIST_FREQ', IOCC,IARG,0, K8B, N17 )
         IF ( N17 .NE. 0 ) JFREQ = JFREQ + 1
C
         CALL GETVR8 ( MCF, 'FREQ', IOCC,IARG,0, R8B, N18 )
         IF ( N18 .NE. 0 ) JFREQ = JFREQ + 1
C
         IF ((N2+N11+N12+N13+N14+N15+N16+N17+N18).EQ.0) JORDR = JORDR+1
C
         CALL GETVTX ( MCF, 'TOUT_CMP', IOCC,IARG,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            JNCMP = JNCMP  + 1
            NOMOBJ = '&&'//NOMPRO//'.NCMP'
            IF ( N2 .NE. 0 ) THEN
               CALL GETVID ( MCF, 'CHAM_GD', IOCC,IARG,1, NOMSY, N2 )
               CALL UTNCMP ( NOMSY, NBC, NOMOBJ )
            ELSE
               CALL GETVID ( MCF, 'RESULTAT', IOCC,IARG,1, RESU , N3 )
               CALL GETVTX ( MCF, 'NOM_CHAM', IOCC,IARG,1, NOMSY, N1 )

               CALL RSORAC(RESU,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                                 NBORDR,1,IBID)
               KNUME = '&&'//NOMPRO//'.NUME_ORDRE'
               CALL WKVECT ( KNUME , 'V V I', NBORDR, JNUME )
               CALL RSORAC(RESU,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                        ZI(JNUME),NBORDR,IBID)
               DO 14 I = 1 , NBORDR
                  NUME = ZI(JNUME+I-1)
                  CALL RSEXCH(' ',RESU, NOMSY, NUME, CHEXTR, IRET )
                  IF ( IRET .EQ. 0 ) GOTO 16
 14            CONTINUE
               CALL U2MESK('F','POSTRELE_9',1,NOMSY)
 16            CONTINUE
               CALL JEDETR ( KNUME )
               CALL UTNCMP ( CHEXTR, NBC, NOMOBJ )
            ENDIF
            IF (NBC.EQ.0) CALL U2MESS('F','POSTRELE_59')
            CALL JEVEUO ( NOMOBJ, 'L', JCMP1 )
            DO 10 I = 1 , NBC
               DO 12 J = 1 , NCMP
                  IF ( ZK8(JNOCMP-1+J) .EQ. ZK8(JCMP1+I-1) ) GOTO 10
 12            CONTINUE
               NCMP = NCMP + 1
               IF (NCMP.GT.NCMPMX) THEN
                 NCMPMX = 2*NCMPMX
                 CALL JUVECA(NOCMP ,NCMPMX)
                 CALL JEVEUO(NOCMP ,'E',JNOCMP)
               ENDIF
               ZK8(JNOCMP-1+NCMP) = ZK8(JCMP1+I-1)
 10         CONTINUE
            CALL JEDETR ( NOMOBJ )
         ENDIF
C
         CALL GETVTX ( MCF, 'NOM_CMP', IOCC,IARG,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
C
            CALL GETVTX ( MCF, 'TRAC_NOR'    , IOCC,IARG,0, K8B, N12)
            IF ( N12 .NE. 0 ) JTRAN = JTRAN  + 1
C
            CALL GETVTX ( MCF, 'TRAC_DIR'      , IOCC,IARG,0, K8B,N14)
            IF ( N14 .NE. 0 ) JTRAD = JTRAD  + 1
C
            IF ( (N12+N14) .NE. 0 ) GOTO 24
            JNCMP = JNCMP  + 1
            NBC = -N1
            CALL WKVECT ( '&&'//NOMPRO//'.NCMP', 'V V K8', NBC, JCMP2 )
            CALL GETVTX ( MCF, 'NOM_CMP', IOCC,IARG,NBC, ZK8(JCMP2),N1)
C           CALL GETVIS ( MCF, 'NUME_CMP', IOCC,1,0, IBID,N11)
            N11=0
            IF ( N11 .NE. 0 ) THEN
               NUMCMP = -N11
               CALL WKVECT ( '&&'//NOMPRO//'.NU_CMP', 'V V I',
     &                       NUMCMP, JNUCP )
C           CALL GETVIS(MCF,'NUME_CMP',IOCC,IARG,NUMCMP,ZI(JNUCP),N11)
            N11=0
               IF (ZK8(JCMP2)(1:4).EQ.'VARI') THEN
                  CALL ASSERT( NBC.EQ.1 )
                  DO 120 I = 1 , NUMCMP
                     CALL CODENT ( ZI(JNUCP+I-1), 'G', K8B )
                     NOMCMP = 'VARI_'//K8B(1:3)
                     DO 122 J = 1 , NCMP
                        IF ( ZK8(JNOCMP-1+J) .EQ. NOMCMP ) GOTO 120
 122                 CONTINUE
                     NCMP = NCMP + 1
                     IF (NCMP.GT.NCMPMX) THEN
                       NCMPMX = 2*NCMPMX
                       CALL JUVECA(NOCMP ,NCMPMX)
                       CALL JEVEUO(NOCMP ,'E',JNOCMP)
                     ENDIF
                     ZK8(JNOCMP-1+NCMP) = NOMCMP
 120              CONTINUE
               ELSE
                  DO 124 I = 1 , NBC
                     DO 126 J = 1 , NCMP
                        IF (ZK8(JNOCMP-1+J).EQ.ZK8(JCMP2+I-1)) GOTO 124
 126                 CONTINUE
                     NCMP = NCMP + 1
                     IF (NCMP.GT.NCMPMX) THEN
                       NCMPMX = 2*NCMPMX
                       CALL JUVECA(NOCMP ,NCMPMX)
                       CALL JEVEUO(NOCMP ,'E',JNOCMP)
                     ENDIF
                     ZK8(JNOCMP-1+NCMP) = ZK8(JCMP2+I-1)
 124              CONTINUE
               ENDIF
               CALL JEDETR ( '&&'//NOMPRO//'.NU_CMP' )
            ELSE
               DO 20 I = 1 , NBC
                  DO 22 J = 1 , NCMP
                     IF ( ZK8(JNOCMP-1+J) .EQ. ZK8(JCMP2+I-1) ) GOTO 20
 22               CONTINUE
                  NCMP = NCMP + 1
                  IF (NCMP.GT.NCMPMX) THEN
                    NCMPMX = 2*NCMPMX
                    CALL JUVECA(NOCMP ,NCMPMX)
                    CALL JEVEUO(NOCMP ,'E',JNOCMP)
                  ENDIF
                  ZK8(JNOCMP-1+NCMP) = ZK8(JCMP2+I-1)
 20            CONTINUE
            ENDIF
            CALL JEDETR ( '&&'//NOMPRO//'.NCMP' )
         ENDIF
 24      CONTINUE
C
         CALL GETVTX ( MCF, 'ELEM_PRINCIPAUX', IOCC,IARG,0, K8B, N1 )
         IF ( N1 .NE. 0 ) JPRIN = JPRIN + 1
C
         CALL GETVTX ( MCF, 'RESULTANTE', IOCC,IARG,0, K8B, N1 )
         CALL GETVTX ( MCF, 'MOMENT'    , IOCC,IARG,0, K8B, N2 )
         IF ( (N1 .NE. 0) .AND. (N2 .NE. 0) ) JMOME = JMOME + 1
         IF ( (N1 .NE. 0) .AND. (N2 .EQ. 0) ) THEN
            JNCMP = JNCMP  + 1
            NBC = -N1
            CALL WKVECT ( '&&'//NOMPRO//'.NCMP', 'V V K8', NBC, JCMP3 )
            CALL GETVTX (MCF,'RESULTANTE',IOCC,IARG,NBC,ZK8(JCMP3),N1)
            DO 30 I = 1 , NBC
               DO 32 J = 1 , NCMP
                  IF ( ZK8(JNOCMP-1+J) .EQ. ZK8(JCMP3+I-1) ) GOTO 30
 32            CONTINUE
               NCMP = NCMP + 1
               IF (NCMP.GT.NCMPMX) THEN
                 NCMPMX = 2*NCMPMX
                 CALL JUVECA(NOCMP ,NCMPMX)
                 CALL JEVEUO(NOCMP ,'E',JNOCMP)
               ENDIF
               ZK8(JNOCMP-1+NCMP) = ZK8(JCMP3+I-1)
 30         CONTINUE
            CALL JEDETR ( '&&'//NOMPRO//'.NCMP' )
         ENDIF
C
         LMIMA = .FALSE.
         LMOYGR = .FALSE.
         LMOYE = .FALSE.
         LEXTR = .FALSE.
         CALL GETVTX ( MCF, 'OPERATION', IOCC,IARG,1, K16B, N3 )
         IF ( K16B .EQ. 'EXTREMA' ) LMIMA = .TRUE.
         IF ( K16B .EQ. 'MOYENNE_ARITH' ) LMOYGR = .TRUE.
         IF ( K16B .EQ. 'MOYENNE' ) THEN
            JMOYE = JMOYE + 1
            LMOYE = .TRUE.
         ENDIF
         IF ( K16B .EQ. 'EXTRACTION' ) THEN
            LEXTR = .TRUE.
C
            CALL GETVTX ( MCF, 'INVARIANT', IOCC,IARG,0, K8B, N2 )
            IF ( N2 .NE. 0 ) JINVA = JINVA + 1
C
            IF ( N1 .EQ. 0 ) JABSC = JABSC + 1
C
            CALL GETVTX ( MCF, 'NOEUD', IOCC,IARG,0, K8B, N2 )
            IF ( (N1 .EQ. 0) .AND. (N2 .NE. 0) ) JNOEU = JNOEU + 1
C
            CALL GETVTX ( MCF, 'GROUP_NO', IOCC,IARG,0, K8B, N2 )
            IF ( (N1 .EQ. 0) .AND. (N2 .NE. 0) ) JNOEU = JNOEU + 1
         ENDIF
C
         CALL GETVTX ( MCF, 'MOYE_NOEUD', IOCC,IARG,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL GETVTX ( MCF, 'MOYE_NOEUD', IOCC,IARG,1, K8B, N1 )
            IF ( K8B(1:3) .EQ. 'NON' ) JMAIL = JMAIL + 1
         ENDIF
C
    2 CONTINUE
C
C====
C 3. CONNAISSANT LES CARACTERISTIQUES DE LA TABLE, ON INITIALISE
C====
C
C 3.1. ==> MISE EN PLACE DES PARAMETRES
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
      IF ( (LEXTR .OR. LMOYE) .AND. JNOEU .NE. 0 ) THEN
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
      IF ( JORDR .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NUME_ORDRE'
         TYPARA(NBP) = 'I'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JMODE .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NUME_MODE'
         TYPARA(NBP) = 'I'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JINST .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'INST'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JFREQ .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'FREQ'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JNCAS .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NOM_CAS'
         TYPARA(NBP) = 'K16'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JNOCP .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'NOEUD_CMP'
         TYPARA(NBP) = 'K16'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JANGL .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'ANGL'
         TYPARA(NBP) = 'R'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JMAIL .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'MAILLE'
         TYPARA(NBP) = 'K8'
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JABSC .NE. 0 ) THEN
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
      IF ( (LEXTR .OR. LMOYE) .AND. JNCMP .NE. 0 ) THEN
         DO 40 I = 1 , NCMP
            NBP = NBP + 1
            NOPARA(NBP) = ZK8(JNOCMP-1+I)
            TYPARA(NBP) = 'R'
 40      CONTINUE
      ENDIF
      IF ( (LEXTR .OR. LMOYE) .AND. JINVA .NE. 0 ) THEN
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
      IF ( (LEXTR .OR. LMOYE) .AND. JPRIN .NE. 0 ) THEN
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
      IF ( (LEXTR .OR. LMOYE) .AND. JMOME .NE. 0 ) THEN
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
      IF ( (LEXTR .OR. LMOYE) .AND. JTRAN .NE. 0 ) THEN
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
      IF ( (LEXTR .OR. LMOYE) .AND. JTRAD .NE. 0 ) THEN
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
      IF ( (LEXTR .OR. LMOYE) .AND. JMOYE .NE. 0 ) THEN
         NBP = NBP + 1
         NOPARA(NBP) = 'QUANTITE'
         TYPARA(NBP) = 'K16'
      ENDIF
      IF ( LMIMA ) THEN
         IF ( JRESU.NE.0 .AND. JORDR.EQ.0 ) THEN
            NBP = NBP + 1
            NOPARA(NBP) = 'NUME_ORDRE'
            TYPARA(NBP) = 'I'
         ENDIF
         NBP = NBP + 1
         NOPARA(NBP) = 'EXTREMA'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'MAILLE'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'NOEUD'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'CMP'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'VALE'
         TYPARA(NBP) = 'R'
      ENDIF
C
      IF ( LMOYGR ) THEN
         IF ( JRESU.NE.0 .AND. JORDR.EQ.0 ) THEN
            NBP = NBP + 1
            NOPARA(NBP) = 'NUME_ORDRE'
            TYPARA(NBP) = 'I'
         ENDIF
         NBP = NBP + 1
         NOPARA(NBP) = 'CMP'
         TYPARA(NBP) = 'K8'
         NBP = NBP + 1
         NOPARA(NBP) = 'MOYENNE'
         TYPARA(NBP) = 'R'
      ENDIF
C
      IF ( NIV.GE.2 ) THEN
        DO 1789 , N1 = 1 , NBP
           VALK(1) = NOPARA(N1)
           VALK(2) = TYPARA(N1)
           CALL U2MESK('I','POSTRELE_10', 2 ,VALK)
 1789   CONTINUE
      ENDIF
C
C 3.2. ==> CREATION/INITIALISATION DE LA TABLE
C
      CALL TBCRSD (NOMTAB, 'G' )
      CALL TBAJPA (NOMTAB, NBP, NOPARA, TYPARA )
C
      K24BID = NOMTAB(1:8)//'           .TITR'
      CALL TITREA('T',NOMTAB,NOMTAB,K24BID,'C',' ',0,'G',
     &            '(1PE12.5)')
C
      CALL JEDETR(NOCMP )
C
      CALL JEDEMA()
C
      END
