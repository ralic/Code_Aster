      SUBROUTINE NMSSSV(MODELZ,MATE  ,CARELE,NUMEDD,LISCHA,
     &                  INSTAN,VEELEM,MEELEM,MEASSE,VECTOR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/03/2008   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_21
C
      IMPLICIT NONE
      CHARACTER*(*) MODELZ 
      CHARACTER*24  MATE,CARELE,NUMEDD            
      CHARACTER*19  MEASSE(8),LISCHA           
      CHARACTER*19  MEELEM(8),VEELEM(30)
      CHARACTER*24  VECTOR 
      REAL*8        INSTAN
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - SOUS-STRUCTURATION)
C
C CALCUL DU VECTEUR 
C      
C ----------------------------------------------------------------------
C
C
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
      CHARACTER*8  MODELE   
      CHARACTER*19 VESSTR
      CHARACTER*24 CNSSTR,FOMUL2  
      INTEGER      IRET             
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C     
      MODELE = MODELZ 
      CNSSTR = '&&CNSSTR'  
      VESSTR = VEELEM(17)  
      FOMUL2 = LISCHA(1:19)//'.FCSS'          
C
C --- CALCUL
C
      CALL JEEXIN(FOMUL2,IRET)
      IF (IRET.EQ.0) THEN
        CALL ASSERT(.FALSE.)
      ELSE
        CALL EXISD('CHAMP',CNSSTR(1:19),IRET)
        IF (IRET.EQ.0) THEN
          CALL MEMARE('V',VESSTR,MODELE,MATE  ,CARELE(1:8),
     &                'CHAR_MECA')
          CALL JEDETR(VESSTR//'.RELC')
          CALL SS2MME(MODELE(1:8),VESSTR,'V')
        END IF
      ENDIF
      CALL ASSVSS('V',CNSSTR,VESSTR,NUMEDD,' ','ZERO',1,FOMUL2,
     &            INSTAN)
      CALL VTAXPY(1.D0,CNSSTR,VECTOR)
C
      CALL JEDEMA()      
C
      END
