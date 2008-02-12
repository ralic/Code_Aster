      SUBROUTINE ARLTIM(UNIT  ,DIME  ,NNM   ,LINCLU,LINCL1,
     &                  NUM1  ,NUM2  ,L1    ,L2    ,TYPEM1,
     &                  TYPEM2,H1    ,H2    ,NG    ,NT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER  UNIT,DIME,NNM
      REAL*8   L1(*),L2(*)
      INTEGER  NUM1,NUM2
      LOGICAL  LINCLU,LINCL1
      INTEGER  NG,NT
      CHARACTER*8  TYPEM1,TYPEM2
      REAL*8       H1,H2
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION D'UNE MATRICE ELEMENTAIRE ARLEQUIN (PRODUITE PAR ARLTE)
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION 
C IN  NNM    : NOMBRE DE NOEUDS PAR MAILLE (27 DANS ASTER)
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  LINCLU : .TRUE.  INTEGRATION STANDARD
C              .FALSE. INTEGRATION PAR SOUS-MAILLES
C IN  LINCL1 : .TRUE.  INTEGRATION SUR LA PREMIERE MAILLE SI INTG. STD.
C              .FALSE. INTEGRATION SUR LA SECONDE MAILLE SI INTG. STD.
C IN  L1     : MATRICE ELEMENTAIRE SUR LA MAILLE 1
C IN  NUM1   : NUMERO ABSOLU (DANS MAILLAGE) DE LA MAILLE 1
C IN  TYPEM1 : TYPE DE LA MAILLE 1
C IN  H1     : TAILLE DE LA MAILLE 1
C IN  L2     : MATRICE ELEMENTAIRE SUR LA MAILLE 2
C IN  NUM2   : NUMERO ABSOLU (DANS MAILLAGE) DE LA MAILLE 2
C IN  TYPEM2 : TYPE DE LA MAILLE 2
C IN  H2     : TAILLE DE LA MAILLE 2
C IN  NG     : NOMBRE DE POINTS D'INTEGRATION NUMERIQUE
C IN  NT     : NOMBRE DE TRIANGLE/TETRAEDRE SI DECOUPE (INT.
C              SOUS-MAILLES)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM
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
C      
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      J
      CHARACTER*8  NOMO,NOMA,NOMMA1,NOMMA2
      INTEGER      IOCC,JLGRF
      REAL*8       VAL1,VAL2
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION MAILLAGE 
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<ARLTE   > AFFICHAGE IMPOSSIBLE (VOIR ARLTIM) '
        GOTO 999  
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',JLGRF)
      NOMA = ZK8(JLGRF)                                
C
C --- CALCUL EQUIVALENT MATRICE L1
C
      VAL1 = 0.D0  
      DO 10 J = 1, 10*NNM*NNM   
        VAL1 = VAL1 + J*L1(J)
  10  CONTINUE          
C
C --- CALCUL EQUIVALENT MATRICE L2
C
      VAL2 = 0.D0
      DO 20 J = 1, 10*NNM*NNM   
        VAL2 = VAL2 + J*L2(J)
  20  CONTINUE   
C
      CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUM1),NOMMA1)
      CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUM2),NOMMA2)  
C
      WRITE(UNIT,*) '<ARLTE   > ... COUPLE...'
C
      IF (LINCLU) THEN
        IF (LINCL1) THEN
          WRITE(UNIT,*) '<ARLTE   > ...... INTEGRATION STANDARD SUR ',
     &                  NOMMA1,'( NB GAUSS: ',NG,' )'
        ELSE
          WRITE(UNIT,*) '<ARLTE   > ...... INTEGRATION STANDARD SUR ',
     &                  NOMMA2,'( NB GAUSS: ',NG,' )'        
        ENDIF  
      ELSE
        IF (DIME.EQ.2) THEN
          WRITE(UNIT,*) '<ARLTE   > ...... INTEGRATION PAR '//
     &                  'SOUS-MAILLES ( NB GAUSS: ',NG,' - '//
     &                  'NB TRIANGLES: ',NT,' )'
        ELSE
          WRITE(UNIT,*) '<ARLTE   > ...... INTEGRATION PAR '//
     &                  'SOUS-MAILLES ( NB GAUSS: ',NG,' - '//
     &                  'NB TETRAEDRES: ',NT,' )'        
        
        ENDIF
      ENDIF
C
      WRITE(UNIT,*) '<ARLTE   > ...... ',NOMMA1,'( ',
     &               TYPEM1,') - TAILLE: ',H1,' - VALE : ',VAL1
      WRITE(UNIT,*) '<ARLTE   > ...... ',NOMMA2,'( ',
     &               TYPEM2,') - TAILLE: ',H2,' - VALE : ',VAL2  
         
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
