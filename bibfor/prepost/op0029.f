      SUBROUTINE OP0029 ( IER )
      IMPLICIT   NONE
      INTEGER    IER
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/07/2002   AUTEUR CIBHHAB S.VANDENBERGHE 
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
C ======================================================================
C TOLE CRS_602
C --- BUT : COMMANDE LIRE_TABLE ---------------------------------------
C ======================================================================
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ======================================================================
C
      INTEGER           IBID,I, J,K,ITIT, IS, IR
      INTEGER           UNIT, NUME, NTIT,NBLTIT,NBPAR,NBTYP
      INTEGER           NBI, NBR, NBK, NBT, COL, NBCOL
      INTEGER           NBL, NB,LDEB, LFIN, DEB, FIN
      PARAMETER         ( NBCOL = 200)
      INTEGER           VALI(NBCOL),LIM, TRUC
      REAL*8            VALR(NBCOL)
      COMPLEX*16        CBID
      CHARACTER*8       FORM, RESULT, K8B
      CHARACTER*2       SEP
      CHARACTER*1       TY
      CHARACTER*3       TYP(NBCOL)
      CHARACTER*14      TYPTAB
      CHARACTER*16      NOMCMD, PAR(NBCOL), PAR2(NBCOL)
      CHARACTER*80      TIT,LIGNE, TITRE2(20), VAL, VALK(NBCOL)
      CHARACTER*2000    TAB
C ======================================================================
      CALL JEMARQ()
C ======================================================================
      IER = 0  
      CALL GETRES ( RESULT, K8B, NOMCMD )        
C ======================================================================
C --------- RECUPERATION DES DONNEES
C ======================================================================
C --- LE FORMAT
      CALL GETVTX ( ' ', 'FORMAT', 1,1,1, FORM , IBID )
C      
C --- UNITE DU FICHIER 
      CALL GETVIS ( ' ', 'UNITE', 1,1,1, UNIT , IBID )
      CALL ASOPEN ( UNIT, ' ' )
C
C --- NUMERO DU TABLEAU DANS LE FICHIER
      CALL GETVIS ( ' ', 'NUME_TABLE', 1,1,1, NUME , IBID )
C
C --- TYPE DE SEPARATEUR
      CALL GETVTX ( ' ', 'SEPARATEUR', 1,1,1, SEP , IBID ) 
C
C --- TYPE DE TABLEAU
      CALL GETVTX ( ' ', 'TYPE_TABLE', 1,1,1, TYPTAB , IBID )
C
C ======================================================================
C --------- INITIALISATION
C ======================================================================
      NB=0
      NBLTIT=0
      NBPAR=0
      NBTYP=0

C ======================================================================
C --------- ON SE PLACE AU DEBUT DU TABLEAU
C ====================================================================
 10   CONTINUE
      READ(UNIT,2000)TAB
      IF (TAB(1:12).EQ.'#DEBUT_TABLE') THEN
             NB=NB+1 
         IF (NB.EQ.NUME)  GOTO 20
       ELSE
          GOTO 10     
       ENDIF 
       GOTO 10    
                 
C ======================================================================
C --------- STOCKAGE DU TITRE
C ======================================================================
 20   CONTINUE
      READ(UNIT,2000) TAB
         IF (TAB(1:6).EQ.'#TITRE') THEN
             NBLTIT=NBLTIT+1
             TITRE2(NBLTIT)=TAB(7:80)
          GOTO 20
         ELSE
          GOTO 30
         ENDIF   
C
 30   CONTINUE
      CALL WKVECT(RESULT//'           .TITR','G V K80',NBLTIT,ITIT)  
      DO 40 I=1,NBLTIT
         ZK80(ITIT + I -1)= TITRE2(I)
 40   CONTINUE

            
C ===================================================================
C --------- CREATION ET REMPLISSAGE DE LA TABLE
C ====================================================================
      CALL TBCRSD (RESULT,'G')
C 
C -------- RECUPERATION DES PARAMETRES DU TABLEAU 
      DO 50 I=1,1999
         IF ((TAB(I:I).EQ.SEP) .AND. (TAB(I+1:I+1).EQ.SEP)) THEN
            GOTO 50
         ELSE IF ((TAB(I:I).NE.SEP) .AND. (TAB(I+1:I+1).NE.SEP)) THEN
            GOTO 50
         ELSE IF ((TAB(I:I).EQ.SEP) .AND. (TAB(I+1:I+1).NE.SEP)) THEN
            DEB=I+1
            GOTO 50
         ELSE IF ((TAB(I:I).NE.SEP) .AND. (TAB(I+1:I+1).EQ.SEP)) THEN 
            FIN=I
            NBPAR=NBPAR+1
            PAR(NBPAR)=TAB(DEB:FIN)            
         ENDIF
 50   CONTINUE     

C         
C ------- RECUPERATION DES TYPES DES PARAMETRES
      READ(UNIT,2000) TAB
      DO 60 I=1,1999
         IF ((TAB(I:I).EQ.SEP) .AND. (TAB(I+1:I+1).EQ.SEP)) THEN
            GOTO 60
         ELSE IF ((TAB(I:I).NE.SEP) .AND. (TAB(I+1:I+1).NE.SEP)) THEN
            GOTO 60
         ELSE IF ((TAB(I:I).EQ.SEP) .AND. (TAB(I+1:I+1).NE.SEP)) THEN
            DEB=I+1
            GOTO 60
         ELSE IF ((TAB(I:I).NE.SEP) .AND. (TAB(I+1:I+1).EQ.SEP)) THEN 
            FIN=I
            NBTYP=NBTYP+1
            TYP(NBTYP)=TAB(DEB:FIN)
         ENDIF
 60   CONTINUE  
 
C --------- VERIFICATION QUE LES TYPES DES PARAMETRES SONT AUTORISES
C
       DO 70 I=1,NBTYP
         IF ((TYP(I).NE.'K8 ').AND.(TYP(I).NE.'K16')
     &    .AND.(TYP(I).NE.'K24').AND.(TYP(I).NE.'K32')
     &    .AND.(TYP(I).NE.'K80').AND.(TYP(I).NE.'I  ')
     &    .AND. (TYP(I).NE.'R  ')) THEN
     
           CALL UTMESS('F',NOMCMD,'TYPE: '//TYP(I)//' NON AUTORISE.')
           GOTO 200
         ENDIF
  70   CONTINUE 
C
C --------- AJOUT DES PARAMETRES
       CALL TBAJPA(RESULT,NBPAR,PAR,TYP)
C
C --------- AJOUT DES LIGNES AU TABLEAU
  80  CONTINUE 
      READ(UNIT,2000) TAB
      COL=0
      NBT=0
      NBK=0
      NBI=0
      NBR=0  
       IF (TAB(1:10).EQ.'#FIN_TABLE') GOTO 200

C ------------------
         DO 90 J=1,1999
            IF ((TAB(J:J).EQ.SEP) .AND. (TAB(J+1:J+1).EQ.SEP)) THEN
             GOTO 90
            ELSE IF ((TAB(J:J).NE.SEP) .AND. (TAB(J+1:J+1).NE.SEP)) THEN
             GOTO 90
            ELSE IF ((TAB(J:J).EQ.SEP) .AND. (TAB(J+1:J+1).NE.SEP)) THEN
             DEB=J+1
             GOTO 90
            ELSE IF ((TAB(J:J).NE.SEP) .AND. (TAB(J+1:J+1).EQ.SEP)) THEN
               FIN=J
               VAL=TAB(DEB:FIN)
               IF (VAL.NE.'-') THEN
                  COL=COL+1
                  TY=TYP(COL+NBT)
                  PAR2(COL)=PAR(COL+NBT)
                  IF (TY.EQ.'K') THEN 
                     NBK=NBK+1
                     VALK(NBK)=VAL
                  ELSE IF (TY.EQ.'I') THEN
                     NBI=NBI+1
                     CALL LXLIIS(VAL,VALI(NBI),IS)
                     IF (IS.NE.0) THEN
                     CALL UTMESS('F',NOMCMD,'VAL: '//VAL//
     +                                      ' PAS UN ENTIER.')
                     GOTO 200
                     ENDIF
                  ELSE IF (TY.EQ.'R') THEN
                     NBR=NBR+1
                     CALL LXLIR8(VAL,VALR(NBR),IR)
                     IF (IR.NE.0) THEN
                     CALL UTMESS('F',NOMCMD,'VAL: '//VAL//
     +                                      ' PAS UN REEL.')
                     GOTO 200
                     ENDIF
                  ENDIF
               ELSE
                  NBT = NBT+1
                  GOTO 90
               ENDIF   
            ENDIF
  90    CONTINUE
C
        CALL TBAJLI(RESULT,COL,PAR2,VALI,VALR,CBID,VALK,0)
        GOTO 80      
C  
 200  CONTINUE 
      CALL TITRE
      CALL ASOPEN ( -UNIT, ' ' )           
C ======================================================================
2000  FORMAT(A)     
      CALL JEDEMA()
C ======================================================================
      END      
