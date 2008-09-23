      SUBROUTINE NMSOLM(SDDYNA,SOLALG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*19 SDDYNA
      CHARACTER*19 SOLALG(*)
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
C
C CONVERSION RESULTAT dU VENANT DE K.dU = F APRES CONTACT
C SUIVANT SCHEMAS
C      
C ----------------------------------------------------------------------
C
C
C IN  SDDYNA : SD DYNAMIQUE
C OUT SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      LOGICAL      NDYNLO,LSTAT,LDYNA 
      LOGICAL      LDEPL,LVITE,LACCE         
      REAL*8       NDYNRE,COEVIT,COEACC  
      CHARACTER*19 NMCHEX
      CHARACTER*19 DDEPLA,DVITLA,DACCLA  
      INTEGER      NDYNIN    
      INTEGER      IFM,NIV         
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> CONVERSION DES INCREMENTS APRES '//
     &                'CORRECTION DU CONTACT ' 
      ENDIF      
C
C --- FONCTIONNALITES ACTIVEES
C
      LSTAT  = NDYNLO(SDDYNA,'STATIQUE')
      LDYNA  = NDYNLO(SDDYNA,'DYNAMIQUE')
C
C --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
C
      IF (LDYNA) THEN
        LDEPL  = NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.1
        LVITE  = NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.2
        LACCE  = NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.3  
      ENDIF        
C
C --- DECOMPACTION VARIABLES CHAPEAUX
C   
      DDEPLA = NMCHEX(SOLALG,'SOLALG','DDEPLA')
      DVITLA = NMCHEX(SOLALG,'SOLALG','DVITLA')    
      DACCLA = NMCHEX(SOLALG,'SOLALG','DACCLA')       
C
C --- COEFFICIENTS POUR CHANGER INCREMENT
C      
      IF (LSTAT) THEN
        GOTO 999  
      ELSEIF (LDYNA) THEN
        COEVIT = NDYNRE(SDDYNA,'COEF_VITE')
        COEACC = NDYNRE(SDDYNA,'COEF_ACCE')           
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF  
C
C --- RE-CALCUL DES INCREMENTS
C    
      IF (LDEPL) THEN
        CALL VTZERO(DVITLA)
        CALL VTZERO(DACCLA)
        CALL VTAXPY(COEVIT,DDEPLA,DVITLA) 
        CALL VTAXPY(COEACC,DDEPLA,DACCLA)                     
      ELSEIF (LVITE) THEN
        CALL ASSERT(.FALSE.)
      
      
      ELSEIF (LACCE) THEN
        CALL ASSERT(.FALSE.)
      
      
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF              
C 
C --- AFFICHAGE
C
  999 CONTINUE 
C  
      IF (NIV.GE.2) THEN      
        WRITE (IFM,*) '<MECANONLINE> ... DEPL. SOLU.     : '        
        CALL NMDEBG('VECT',DDEPLA,IFM   )
        IF (LDYNA) THEN        
          WRITE (IFM,*) '<MECANONLINE> ... VITE. SOLU.     : '        
          CALL NMDEBG('VECT',DVITLA,IFM   )                
          WRITE (IFM,*) '<MECANONLINE> ... ACCE. SOLU.     : '        
          CALL NMDEBG('VECT',DACCLA,IFM   )        
        ENDIF                      
      ENDIF                
C      
      CALL JEDEMA()
      END
