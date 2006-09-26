      SUBROUTINE DDMPFN(NMNBN,NMPLAS,NMDPLA,NMDDPL
     &                 ,NMZEF,NMZEG,NMIEF,NMPROX )

       IMPLICIT  NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,VECTEU,MATRIC,TEMPNO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C---------------------------------------------
      REAL*8  NMNBN(6)         
      REAL*8  NMPLAS(2,3)   
      REAL*8  NMDPLA(2,2)  
      REAL*8  NMDDPL(2,2)
      REAL*8  NMZEF        
      REAL*8  NMZEG         
      INTEGER NMIEF  
      INTEGER NMPROX(2)  


      INTEGER I,IER0,IER1,IER2
C--------------------------------------------        
      INTEGER    MP1F(2),MP2F(2),DMP1F(2),DMP2F(2)
      INTEGER    DDMP1F(2),DDMP2F(2)
      CHARACTER*24    CGLR
      INTEGER       IMP1,IMP2,IDMP1,IDMP2,IDDMP1,IDDMP2,INN,INM
C--------------------------------------------        

C----- LECTURE DES OBJETS "GLRC"      
      

      CALL JEMARQ()
      
      CGLR = '&&GLRC.MP1FONC'
      CALL JEVEUO(CGLR,'L',IMP1)      
      
      CGLR = '&&GLRC.MP2FONC'
      CALL JEVEUO(CGLR,'L',IMP2)      
      
      CGLR = '&&GLRC.DMP1FONC'
      CALL JEVEUO(CGLR,'L',IDMP1)      
      
      CGLR = '&&GLRC.DMP2FONC'
      CALL JEVEUO(CGLR,'L',IDMP2)      
      
      CGLR = '&&GLRC.DDMP1FONC'
      CALL JEVEUO(CGLR,'L',IDDMP1)      
      
      CGLR = '&&GLRC.DDMP2FONC'
      CALL JEVEUO(CGLR,'L',IDDMP2)      
      
      DO 15, I = 1,2
        MP1F(I)   = ZI(IMP1-1   + I) 
        MP2F(I)   = ZI(IMP2-1   + I)
        DMP1F(I)  = ZI(IDMP1-1  + I) 
        DMP2F(I)  = ZI(IDMP2-1  + I) 
        DDMP1F(I) = ZI(IDDMP1-1 + I) 
        DDMP2F(I) = ZI(IDDMP2-1 + I)         
 15   CONTINUE
 
      
       DO 10, I=1,2 
         CALL CDNFON(DDMP1F(I),NMNBN(I),0,
     &                     NMDDPL(1,I),IER0)
         IF (IER0 .GT. 0) THEN 
           CALL CDNFON(DMP1F(I),NMNBN(I),1,
     &                       NMDDPL(1,I),IER1)
           IF (IER1 .GT. 0) THEN 
              CALL CDNFON(MP1F(I),NMNBN(I),2,
     &                          NMDDPL(1,I),IER2)
              IF (IER2 .EQ. 3) THEN
                CALL UTMESS('F','GLRC:DDMPFN'
     &                      ,'DERIVATIVES OF "MP" NOT DEFINED')
              ENDIF

           ENDIF
         ENDIF
         CALL CDNFON(DDMP2F(I),NMNBN(I),0,
     &                     NMDDPL(2,I),IER0)
         IF (IER0 .GT. 0) THEN 
           CALL CDNFON(DMP2F(I),NMNBN(I),1,
     &                       NMDDPL(2,I),IER1)
           IF (IER1 .GT. 0) THEN 
              CALL CDNFON(MP2F(I),NMNBN(I),2,
     &                          NMDDPL(2,I),IER2)
              IF (IER2 .EQ. 3) THEN
                CALL UTMESS('F','GLRC:DDMPFN'
     &                     ,'DERIVATIVES OF "MP" NOT DEFINED')
              ENDIF

           ENDIF
         ENDIF
 10   CONTINUE 

      CALL JEDEMA()     

 
      END 
