      SUBROUTINE NMRVAI(SDSTAT,QUESTZ,PHASE,VALI  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*(*) QUESTZ
      CHARACTER*24  SDSTAT
      CHARACTER*1   PHASE
      INTEGER       VALI
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C MESURE DE STATISTIQUES - NOMBRE D'OCCURRENCES
C
C ----------------------------------------------------------------------
C
C
C IN  SDSTAT : SD STATISTIQUES
C IN  QUESTI : QUESTION DE STATISTIQUE
C IN  PHASE  : PHASE
C               'E' ECRITURE
C               'N' LECTURE SUR L'ITERATION DE NEWTON COURANTE
C               'P' LECTURE SUR LE PAS COURANT
C               'T' LECTURE SUR TOUT LE TRANSITOIRE    
C I/O VALI   : VALEUR ENTIERE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      CHARACTER*24 STVIP,STVIT,STVIN
      INTEGER      JSTVIP,JSTVIT,JSTVIN
      CHARACTER*24 QUESTI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C    
C --- INITIALISATIONS
C                
      QUESTI  = QUESTZ
      IF (PHASE.EQ.'E') THEN
        IF (VALI.LE.0) VALI = 0
      ELSE
        VALI = 0
      ENDIF
C
C --- ACCES SDSTAT
C      
      STVIP  = SDSTAT(1:19)//'.VLIP'
      STVIT  = SDSTAT(1:19)//'.VLIT'
      STVIN  = SDSTAT(1:19)//'.VLIN'
      CALL JEVEUO(STVIP ,'E',JSTVIP)
      CALL JEVEUO(STVIT ,'E',JSTVIT)
      CALL JEVEUO(STVIN ,'E',JSTVIN)
C        
      IF (PHASE.EQ.'E') THEN
        IF (QUESTI.EQ.'FACTOR') THEN
          ZI(JSTVIP+1-1) = ZI(JSTVIP+1-1) + VALI
          ZI(JSTVIT+1-1) = ZI(JSTVIT+1-1) + VALI
          ZI(JSTVIN+1-1) = ZI(JSTVIN+1-1) + VALI
        ELSEIF (QUESTI.EQ.'SOLVE') THEN
          ZI(JSTVIP+2-1) = ZI(JSTVIP+2-1) + VALI 
          ZI(JSTVIT+2-1) = ZI(JSTVIT+2-1) + VALI
          ZI(JSTVIN+2-1) = ZI(JSTVIN+2-1) + VALI
        ELSEIF (QUESTI.EQ.'INTEGRATION') THEN
          ZI(JSTVIP+3-1) = ZI(JSTVIP+3-1) + VALI 
          ZI(JSTVIT+3-1) = ZI(JSTVIT+3-1) + VALI
          ZI(JSTVIN+3-1) = ZI(JSTVIN+3-1) + VALI
        ELSEIF (QUESTI.EQ.'CONT_GEOM') THEN
          ZI(JSTVIP+4-1) = ZI(JSTVIP+4-1) + VALI
          ZI(JSTVIT+4-1) = ZI(JSTVIT+4-1) + VALI
          ZI(JSTVIN+4-1) = ZI(JSTVIN+4-1) + VALI
        ELSEIF (QUESTI.EQ.'CTCC_MATR') THEN
          ZI(JSTVIP+5-1) = ZI(JSTVIP+5-1) + VALI
          ZI(JSTVIT+5-1) = ZI(JSTVIT+5-1) + VALI
          ZI(JSTVIN+5-1) = ZI(JSTVIN+5-1) + VALI
        ELSEIF (QUESTI.EQ.'CTCC_CONT') THEN
          ZI(JSTVIP+6-1) = ZI(JSTVIP+6-1) + VALI
          ZI(JSTVIT+6-1) = ZI(JSTVIT+6-1) + VALI
          ZI(JSTVIN+6-1) = ZI(JSTVIN+6-1) + VALI
        ELSEIF (QUESTI.EQ.'CTCC_FROT') THEN
          ZI(JSTVIP+7-1) = ZI(JSTVIP+7-1) + VALI         
          ZI(JSTVIT+7-1) = ZI(JSTVIT+7-1) + VALI
          ZI(JSTVIN+7-1) = ZI(JSTVIN+7-1) + VALI
        ELSEIF (QUESTI.EQ.'RECH_LINE_ITER') THEN
          ZI(JSTVIP+8-1) = ZI(JSTVIP+8-1) + VALI
          ZI(JSTVIT+8-1) = ZI(JSTVIT+8-1) + VALI
          ZI(JSTVIN+8-1) = ZI(JSTVIN+8-1) + VALI
        ELSEIF (QUESTI.EQ.'FETI_ITER') THEN
          ZI(JSTVIP+9-1) = ZI(JSTVIP+9-1) + VALI
          ZI(JSTVIT+9-1) = ZI(JSTVIT+9-1) + VALI
          ZI(JSTVIN+9-1) = ZI(JSTVIN+9-1) + VALI   
        ELSEIF (QUESTI.EQ.'CTCD_ALGO_ITER') THEN
          ZI(JSTVIP+10-1) = ZI(JSTVIP+10-1) + VALI
          ZI(JSTVIT+10-1) = ZI(JSTVIT+10-1) + VALI
          ZI(JSTVIN+10-1) = ZI(JSTVIN+10-1) + VALI
        ELSEIF (QUESTI.EQ.'CONT_NBLIAC')    THEN
          ZI(JSTVIP+11-1) = VALI
          ZI(JSTVIT+11-1) = VALI
          ZI(JSTVIN+11-1) = VALI
        ELSEIF (QUESTI.EQ.'CONT_NBLIAF')    THEN
          ZI(JSTVIP+12-1) = VALI
          ZI(JSTVIT+12-1) = VALI
          ZI(JSTVIN+12-1) = VALI
        ELSEIF (QUESTI.EQ.'CTCC_PREP') THEN
          ZI(JSTVIP+13-1) = ZI(JSTVIP+13-1) + VALI         
          ZI(JSTVIT+13-1) = ZI(JSTVIT+13-1) + VALI
          ZI(JSTVIN+13-1) = ZI(JSTVIN+13-1) + VALI
        ELSEIF (QUESTI.EQ.'PAS') THEN
          ZI(JSTVIP+14-1) = ZI(JSTVIP+14-1) + VALI         
          ZI(JSTVIT+14-1) = ZI(JSTVIT+14-1) + VALI
          ZI(JSTVIN+14-1) = ZI(JSTVIN+14-1) + VALI
        ELSEIF (QUESTI.EQ.'ITE') THEN
          ZI(JSTVIP+15-1) = ZI(JSTVIP+15-1) + VALI         
          ZI(JSTVIT+15-1) = ZI(JSTVIT+15-1) + VALI
          ZI(JSTVIN+15-1) = ZI(JSTVIN+15-1) + VALI          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        
      ELSEIF ((PHASE.EQ.'P').OR.
     &        (PHASE.EQ.'T').OR.
     &        (PHASE.EQ.'N')) THEN
        
        IF (QUESTI.EQ.'FACTOR') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+1-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+1-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+1-1)
        
        ELSEIF (QUESTI.EQ.'SOLVE') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+2-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+2-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+2-1)
        
        ELSEIF (QUESTI.EQ.'INTEGRATION') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+3-1) 
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+3-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+3-1)
        
        ELSEIF (QUESTI.EQ.'CONT_GEOM') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+4-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+4-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+4-1)
        
        ELSEIF (QUESTI.EQ.'CTCC_MATR') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+5-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+5-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+5-1)
        
        ELSEIF (QUESTI.EQ.'CTCC_CONT') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+6-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+6-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+6-1)
        
        ELSEIF (QUESTI.EQ.'CTCC_FROT') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+7-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+7-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+7-1)
        
        ELSEIF (QUESTI.EQ.'RECH_LINE_ITER') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+8-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+8-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+8-1)
    
        ELSEIF (QUESTI.EQ.'FETI_ITER') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+9-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+9-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+9-1)
             
        ELSEIF (QUESTI.EQ.'CTCD_ALGO_ITER') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+10-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+10-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+10-1)
        
        ELSEIF (QUESTI.EQ.'CONT_NBLIAC')    THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+11-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+11-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+11-1)

        ELSEIF (QUESTI.EQ.'CONT_NBLIAF')    THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+12-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+12-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+12-1)
          
        ELSEIF (QUESTI.EQ.'CTCC_PREP') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+13-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+13-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+13-1)

        ELSEIF (QUESTI.EQ.'PAS') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+14-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+14-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+14-1)

        ELSEIF (QUESTI.EQ.'ITE') THEN
          IF (PHASE.EQ.'P') VALI = ZI(JSTVIP+15-1)
          IF (PHASE.EQ.'T') VALI = ZI(JSTVIT+15-1)
          IF (PHASE.EQ.'N') VALI = ZI(JSTVIN+15-1)
                         
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      
      ELSE
        CALL ASSERT(.FALSE.)
      
      ENDIF
C
      CALL JEDEMA()
      END
