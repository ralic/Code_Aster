      SUBROUTINE SLEELT(MAXNOD,NBTYMA,INDIC,PERMUT,NBMAIL,MINT,MANT,
     &                  DATSET,INUM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 03/05/2004   AUTEUR NICOLAS O.NICOLAS 
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
C TOLE CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
C     ==============================================================
CA PRESUPER
C
C     ============================================================
C     !                                                          !
C     !  FONCTION:LECTURE DES MAILLES SUR LE FICHIER UNIVERSEL   !
C     !           ISSU DE SUPERTAB I-DEAS 4.0, 6.0 OU 7.0   ET   !
C     !           STOCKAGE DANS DES OBJETS JEVEUX                !
C     !                                                          !
C     ============================================================
C     !                                                          !
C     !  ROUTINES APPELES : IUNIFI (FONCTION)                    !
C     !                          : LECELT                        !
C     !                                                          !
C     !  ROUTINE APPELANTE : PRESUP                              !
C     !                                                          !
C     ============================================================
C     !                                                          !
C     !                   ***************                        !
C     !                   *  ARGUMENTS  *                        !
C     !                   ***************                        !
C     !                                                          !
C     !  ******************************************************  !
C     !  *   NOM    *  TYPE * MODE *ALTERE *      ROLE        *  !
C     !  ******************************************************  !
C     !  *          *       *      *       *                  *  !
C     !  * MAXNOD   *INTEGER*ENTREE* NON   *NBRE MAXI DE NOEUD*  !
C     !  *          *       *      *       * POUR UNE MAILLE  *  !
C     !  * NBTYMA   *INTEGER*ENTREE* NON   *NBRE DE TYPE DE   *  !
C     !  *          *       *      *       *MAILLES SUPERTAB  *  !
C     !  * INDIC    *INTEGER*ENTREE* NON   *INDIQUE SI UNE    *  !
C     !  *          *       *      *       * PERMUTATION EST  *  !
C     !  *          *       *      *       *  NECESSAIRE      *  !
C     !  * PERMUT   *INTEGER*ENTREE* NON   *TABLEAU DES PERMU-*  !
C     !  *          *       *      *       *TATION DES NOEUDS *  !
C     !  *          *       *      *       *POUR UNE MAILLE   *  !
C     !  * NBMAIL   *INTEGER*SORTIE* NON   * TABLEAU CONTENANT*  !
C     !  *          *       *      *       *LE NOMBRE DE MAIL-*  !
C     !  *          *       *      *       *LES DE CHAQUE TYPE*  !
C     !  * MANT     *INTEGER*SORTIE* NON   * TABLEAU CONTENANT*  !
C     !  *          *       *      *       *LE N DE MAILLE MAX*  !
C     !  *          *       *      *       *POUR CHAQUE TYPE  *  !
C     !  *          *       *      *       * DE MAILLE        *  !
C     !  * MINT     *INTEGER*SORTIE* NON   * TABLEAU CONTENANT*  !
C     !  *          *       *      *       *LE N DE MAILLE MIN*  !
C     !  *          *       *      *       *POUR CHAQUE TYPE  *  !
C     !  *          *       *      *       * DE MAILLE        *  !
C     !  *          *       *      *       * DE MAILLE        *  !
C     !  * DATSET   *INTEGER*ENTREE* NON   * NUMERO DU DATASET*  !
C     !  *          *       *      *       *TRAITE            *  !
C     !  *          *       *      *       *                  *  !
C     !  ******************************************************  !
C     !                                                          !
C     ============================================================
C
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER      ZI
      REAL*8       ZR
      COMPLEX*16   ZC
      LOGICAL      ZL,EXISDG
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM,JEXATR
      CHARACTER*80 ZK80
C ---------------------------------------------------------------
C  --> DECLARATION DES ARGUMENTS
      INTEGER MAXNOD,NBTYMA,DATSET
      INTEGER MINT(NBTYMA),MANT(NBTYMA)
      INTEGER NBMAIL(NBTYMA),INDIC(NBTYMA),PERMUT(MAXNOD,NBTYMA)
C  --> DECLARATION DES VARIABLES LOCALES
      CHARACTER*80 CBUF
      INTEGER IND,JNUM,CODGRA,CODMEC,IPROP,IMAT,ICOL,NBNODE,INUM
      INTEGER NODE(32),NOD82(10000),ICO,IBID2,ICP,INO
      INTEGER CODDES,IPHYB,IMATB
C
C  --------- FIN DECLARATION ----------
C
C  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      CALL JEMARQ()
      IUNV=IUNIFI('IDEAS')
      IMES=IUNIFI('MESSAGE')
C
      INUM = 0
C
C --> LECTURE DES MAILLES DANS LE FICHIER UNIVERSEL
C     ---------------------------------------------
      IPOS  = 0
      ICP=0
      INO=0
      IBID2=1
      NDECA = 0
      NTAIL = 1000
      NITER = 1000
      CALL JEEXIN ( '&&PRESUP.INFO.MAILLE', IRE1 )
      IF ( IRE1 .NE. 0 ) CALL JEDETR( '&&PRESUP.INFO.MAILLE' )
      CALL WKVECT('&&PRESUP.INFO.MAILLE','V V I',4*NTAIL,JINFO)
      CALL JEEXIN ( '&&PRESUP.CONN.MAILLE', IRE1 )
      IF ( IRE1 .NE. 0 ) CALL JEDETR( '&&PRESUP.CONN.MAILLE' )
      CALL WKVECT('&&PRESUP.CONN.MAILLE','V V I',27*NTAIL,JCONN)
            
  1   CONTINUE
      DO 10 I=1,NITER
        READ (IUNV,FMT='(A)') CBUF
        READ (UNIT=CBUF,FMT='(I6)') IND
C
        IF (INUM.NE.0) THEN
          ICO=CODGRA
        ENDIF
C
        IF (IND.EQ.-1) GO TO 99
C
C=== TRAITEMENT DIFFERENT SELON LA VERSION DE SUPERTAB
C    (I-DEAS 4.0 OU I-DEAS 5.0)
C
        IF (DATSET.EQ.71) THEN
         READ(CBUF,'(7I10)') JNUM,CODGRA,CODMEC,IPROP,IMAT,ICOL,NBNODE
        ELSE IF ((DATSET.EQ.82).OR.(DATSET.EQ.2431)) THEN
          READ(CBUF,'(3I10)') ICOL,NBNODE,IBID2
          JNUM=1
          CODGRA=1
          IPROP=1
          IMAT=1
          MINT(CODGRA)=0
          MANT(CODGRA)=0
          READ(IUNV,'(A)') CBUF
        ELSE IF (DATSET.EQ.780) THEN
         READ(CBUF,'(8I10)') JNUM,CODDES,IPHYB,IPROP,IMATB,IMAT,ICOL,
     +                                             NBNODE
         IF (CODDES.EQ.11.OR.
     &       CODDES.EQ.21.OR.CODDES.EQ.22.OR.
     &      CODDES.EQ.23.OR.CODDES.EQ.24)  THEN
            READ(IUNV,'(A)') CBUF
         ENDIF
         CALL COV4V5 (CODDES,CODGRA)
        ELSE IF (DATSET.EQ.2412) THEN
         READ(CBUF,'(6I10)') JNUM,CODDES,IPROP,IMAT,ICOL,NBNODE
         IF (CODDES.EQ.11.OR.
     &       CODDES.EQ.21.OR.CODDES.EQ.22.OR.
     &      CODDES.EQ.23.OR.CODDES.EQ.24)  THEN
            READ(IUNV,'(A)') CBUF
         ENDIF
         CALL COV4V5 (CODDES,CODGRA)
        ENDIF
C
        IF ((DATSET.EQ.82).OR.(DATSET.EQ.2431)) THEN
          DO 666 INO=1,10000
            NOD82(INO)=0
  666     CONTINUE
          CALL LECT82(NOD82,NBNODE,JNUM)
        ELSE
          CALL LECELT(MAXNOD,NBTYMA,INDIC,PERMUT,CODGRA,NODE,NBNODE)
        ENDIF
C --> RECHERCHE DU N MIN ET N MAX POUR UNE CATEGORIE DE MAILLE
C
          INUM = INUM + 1
          IF (ICO.EQ.CODGRA) THEN
            MINT(CODGRA)=MIN(JNUM,MINT(CODGRA))
            MANT(CODGRA)=MAX(JNUM,MANT(CODGRA))
          ELSE
            MINT(CODGRA)=JNUM
            MANT(CODGRA)=JNUM
          ENDIF
          NBMAIL(CODGRA) = NBMAIL(CODGRA) + 1
C
C --> ECRITURE DES MAILLES DANS LE FICHIER BUFFER IMA
C
        IF ((DATSET.EQ.82).OR.(DATSET.EQ.2431)) THEN
          ICP=0
          DO  22 INO=1,2*JNUM,2
            IF ((NOD82(INO).NE.0).AND.(NOD82(INO+1).NE.0)) THEN
              ZI(JCONN-1+INO) = NOD82(INO)
              ZI(JCONN-1+INO+1) = NOD82(INO+1)
              ZI(JINFO-1+NDECA+(ICP)*4+1) = ICP+NBMAIL(CODGRA)
              ZI(JINFO-1+NDECA+(ICP)*4+2) = CODGRA
              ZI(JINFO-1+NDECA+(ICP)*4+3) = 2
              ZI(JINFO-1+NDECA+(ICP)*4+4) = ICOL   
              ICP=ICP+1
            ENDIF
   22     CONTINUE
          NBMAIL(CODGRA) = JNUM + NBMAIL(CODGRA)          
          IPOS= JNUM
          MINT(CODGRA)=1
          MANT(CODGRA)=JNUM
        ELSE
          ZI(JINFO-1+NDECA+(I-1)*4+1) = JNUM
          ZI(JINFO-1+NDECA+(I-1)*4+2) = CODGRA
          ZI(JINFO-1+NDECA+(I-1)*4+3) = NBNODE
          ZI(JINFO-1+NDECA+(I-1)*4+4) = ICOL
          DO  21 INO=1,NBNODE
            ZI(JCONN-1+IPOS+INO) = NODE(INO)
   21     CONTINUE
          IPOS= IPOS+NBNODE
        ENDIF
C
   10 CONTINUE
      NTAIL = NTAIL + NITER
      NDECA= NDECA + 4000
      CALL JUVECA('&&PRESUP.INFO.MAILLE',4*NTAIL)
      CALL JEVEUO('&&PRESUP.INFO.MAILLE','E',JINFO)
      CALL JUVECA('&&PRESUP.CONN.MAILLE',IPOS+27*NTAIL)
      CALL JEVEUO('&&PRESUP.CONN.MAILLE','E',JCONN)
      GO TO 1
   99 CONTINUE
      IMES = IUNIFI('MESSAGE')
      WRITE (IMES,*) 'NOMBRE DE MAILLES :',INUM
      CALL JEDEMA()
      END
