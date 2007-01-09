      SUBROUTINE ARLSUP(DIME  ,PRECBO,NOMGRP,BC    ,APP)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8       PRECBO
      CHARACTER*10 NOMGRP
      INTEGER      DIME
      REAL*8       BC(2,*)
      LOGICAL      APP(*)      
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C RECONSTRUCTION ZONE DE SUPERPOSITION A PARTIR ZONE DE COLLAGE
C
C ----------------------------------------------------------------------
C      
C    
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  PRECBO : PRECISION RELATIVE POUR TESTER LA BOITE ENGLOBANTE
C IN  BC     : BOITE ENGLOBANT LA ZONE DE RECOUVREMENT
C IN  NOMGRP : NOMGRP DE LA SD DE STOCKAGE MAILLES 
C I/O APP(*) : IN  - .TRUE. SSI MAILLE * APPARIEE (INDEX GLOBAL)
C              OUT - .TRUE. SSI MAILLE * DANS ZONE SUPERPOSITION
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXATR
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
      REAL*8       PREC,C1,C2
      INTEGER      NMA,NC,MA,IMA,DIM2,I,J,K
      INTEGER      A0,A1,A2,A3,A4,P0,P1,Q0,Q1,Q2,Q3
      REAL*8       MX(3),MN(3),R,R1,R2
      CHARACTER*16 NOMBOI
      CHARACTER*8  K8BID
      CHARACTER*24 NGRMA,GRMAMA
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C      
      DIM2 = 2*DIME   
      PREC = SQRT(PRECBO)
      C1   = (1+PREC)/(1+2.D0*PREC)
      C2   = PREC/(1+2.D0*PREC)
C
C --- LECTURE DONNEES GROUPE DE MAILLES
C
      NGRMA  = NOMGRP(1:10)//'.GROUPEMA'           
      CALL JELIRA(NGRMA,'LONMAX',NMA,K8BID)
      IF (NMA.EQ.1) THEN
        GOTO 999
      ELSE
        CALL JEVEUO(NGRMA,'L',A0)
      ENDIF  
C
C --- LECTURE DONNEES BOITES 
C      
      NOMBOI = NOMGRP(1:10)//'.BOITE' 
      CALL JEVEUO(NOMBOI(1:16)//'.MINMAX','L',A1)
      CALL JEVEUO(NOMBOI(1:16)//'.MMGLOB','L',A2)
C
C --- LECTURE GRAPHE MAILLE/MAILLE
C
      GRMAMA = NOMGRP(1:10)//'.GRMAMA'      
      CALL JEVEUO(GRMAMA,'L',A3)
      CALL JEVEUO(JEXATR(GRMAMA,'LONCUM'),'L',A4)
C
C --- ALLOCATION OBJETS TEMPORAIRES
C
      CALL WKVECT('&&ARLSUP.CMP','V V I',NMA,Q0)
      CALL WKVECT('&&ARLSUP.PILE','V V I',NMA,Q1)

C --- 1. CALCUL DES COMPOSANTES CONNEXES DES ZONES NON APPARIEES

      DO 10 I = 1, NMA
        IMA = ZI(A0-1+I)
        IF (APP(IMA)) THEN
          ZI(Q0-1+I) = -1
        ELSE
          ZI(Q0-1+I) = 0
        ENDIF
 10   CONTINUE

      NC = 0

      DO 20 I = 1, NMA

        IF (ZI(Q0-1+I).NE.0) GOTO 20

        NC = NC + 1
        ZI(Q0-1+I) = NC

        ZI(Q1) = I
        Q2 = Q1
        Q3 = Q1

 30     CONTINUE
      
        MA = ZI(Q2)
        Q2 = Q2 + 1
                          
        P0 = ZI(A4-1+MA)
        P1 = ZI(A4+MA)-1

        DO 40 J = P0, P1
 
          MA = ZI(A3-1+J)
          IF (ZI(Q0-1+MA).NE.0) GOTO 40

          Q3 = Q3 + 1
          ZI(Q3) = MA
          ZI(Q0-1+MA) = NC

 40     CONTINUE

        IF (Q2.LE.Q3) GOTO 30

 20   CONTINUE

C --- 2. COMPOSANTES CONNEXES APPARTENANT A LA ZONE DE RECOUVREMENT

      DO 50 I = 1, NC

C ----- 2.1. BOITE ENGLOBANT LA COMPOSANTE CONNEXE I

        P0 = A2
        DO 60 J = 1, DIME
          MX(J) = ZR(P0)
          MN(J) = ZR(P0+1)
          P0 = P0 + 2
 60     CONTINUE

        DO 70 J = 1, NMA

          IF (ZI(Q0-1+J).NE.I) GOTO 70

          P0 = A1+DIM2*(J-1)

          DO 80 K = 1, DIME

            R1 = ZR(P0)
            R2 = ZR(P0+1)
            P0 = P0 + 2

            R = C1*R1+C2*R2
            IF (R.LT.MN(K)) MN(K) = R
            R = C1*R2+C2*R1
            IF (R.GT.MX(K)) MX(K) = R

 80       CONTINUE

 70     CONTINUE

C ----- 2.2. INCLUSION STRICTE DANS BOITE ENGLOBANT ZONE COLLAGE ?

        DO 90 J = 1, DIME
          IF (.NOT.((BC(1,J).LT.MN(J)).AND.(BC(2,J).GT.MX(J)))) GOTO 50
 90     CONTINUE

C ----- 2.3. MARQUAGE VECTEUR APP

        DO 100 J = 1, NMA
          IF (ZI(Q0-1+J).EQ.I) APP(ZI(A0-1+J)) = .TRUE.
 100    CONTINUE

 50   CONTINUE
C
C --- DESALLOCATIONS
C
      CALL JEDETR('&&ARLSUP.CMP')
      CALL JEDETR('&&ARLSUP.PILE')

 999  CONTINUE

      CALL JEDEMA()

      END
