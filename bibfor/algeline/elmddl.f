      SUBROUTINE ELMDDL(RAIDE,NEQ,DDL,NDDLE,NBDDL,VECDDL,IER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/04/2010   AUTEUR MICHEL S.MICHEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT NONE
      CHARACTER*19 RAIDE
      INTEGER      NEQ,NBDDL,VECDDL(NEQ),NDDLE,IER
      CHARACTER*8  DDL(NDDLE)
C
C     ------------------------------------------------------------------
C     CONSTRUCTION D'UN TABLEAU D'ENTIERS REPERANT LA POSITION DES DDL
C     EXCLUS DE LA RECHERCHE DE VALEURS PRORPES
C     ------------------------------------------------------------------
C IN  RAIDEUR : K  : NOM DE LA MATRICE DE "RAIDEUR"
C IN  NEQ     : IS : NPMBRE DE DDL
C IN  DDL     : K  : NOM DU DDL A ELIMINER
C IN  NDDLE   : IS : NOMBRE DE TYPES DE DDL EXCLUS
C OUT NBDDL   : IS : NOMBRE DE DDL A ELIMINER
C OUT VECDDL  : IS : POSITION DES DDL A ELIMINER
C
C     ------------------------------------------------------------------
C
      INTEGER      IBID,IERD,IEQ,IFM,NIV,I,INTER(NEQ)
      CHARACTER*14 NUME
C
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFNIV(IFM,NIV)
C     
C     CALCUL DU NOMBRE DE DDL A ELIMINER 
C     
      NBDDL = 0      
C
C     RECUPERATION DU NOM DE LA NUMEROTATION ASSOCIEE AUX MATRICES
C
      CALL DISMOI('F','NOM_NUME_DDL',RAIDE,'MATR_ASSE',IBID,NUME,IERD)
      
      DO 5 IEQ =1,NEQ
        VECDDL(IEQ) = 1
   5  CONTINUE  
C       
      IF (NDDLE.GT.0) THEN
        DO 10 I = 1,NDDLE     
C
C       RECUPERATION DES POSITIONS DES DDL
C  
          CALL PTEDDL('NUME_DDL',NUME,1,DDL(I),NEQ,INTER)   
C
C       CALCUL DU NOMBRE DE 'DDL': NBDDL
C
          DO 20 IEQ = 1,NEQ
            NBDDL = NBDDL + INTER(IEQ)
  20      CONTINUE
C  
C       STOP SI ON CHERCHE A ELIM UN DDL ABSENT DE LA MODELISATION  
C  
          IF (NBDDL.EQ.0) THEN
            CALL ASSERT(.FALSE.)
          ENDIF      
C
C       INVERSION : INTER = 0 SI DDL TROUVE ET 1 SINON
C
          DO 30 IEQ = 1,NEQ
            INTER(IEQ) = ABS(INTER(IEQ)-1)
  30      CONTINUE 
C       
          DO 40 IEQ = 1,NEQ
            VECDDL(IEQ) = VECDDL(IEQ)*INTER(IEQ)
  40      CONTINUE  
C       
  10    CONTINUE
      ENDIF    
C
C     IMPRESSION DES DDL 
C
      IF (NIV.GE.1) THEN  
        IF (NBDDL.GT.0) THEN  
          WRITE (IFM,9000) DDL(1),DDL(2),DDL(3),DDL(4)          
          IF (NDDLE.GT.4) THEN   
            WRITE (IFM,9010) DDL(5),DDL(6),DDL(7),DDL(8)
          ENDIF
          IF (NDDLE.GT.8) THEN  
            WRITE (IFM,9020) DDL(9),DDL(10),DDL(11),DDL(12)
          ENDIF
          IF (NDDLE.GT.12) THEN
            WRITE (IFM,9030) DDL(13),DDL(14),DDL(15),DDL(16)  
          ENDIF
          IF (NDDLE.GT.16) THEN
            WRITE (IFM,9040) DDL(17),DDL(18),DDL(19),DDL(20)   
          ENDIF                 
          WRITE (IFM,9050) NBDDL
        ENDIF  
        WRITE (IFM,9060) 
      END IF
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C  
      CALL JEDETC('V','&&ELIMDDL',1)
      IER = 0
      CALL JEDEMA()
C
 9000 FORMAT ('DDL_EXCLUS:',1X,A8,1X,A8,1X,A8,1X,A8,/)
 9010 FORMAT (12X,A8,1X,A8,1X,A8,1X,A8,/) 
 9020 FORMAT (12X,A8,1X,A8,1X,A8,1X,A8,/) 
 9030 FORMAT (12X,A8,1X,A8,1X,A8,1X,A8,/) 
 9040 FORMAT (12X,A8,1X,A8,1X,A8,1X,A8,/) 
 9050 FORMAT ('NOMBRE DE DDL_EXCLUS:',10X,I7,/)
 9060 FORMAT (72('-'))
C 
      END
