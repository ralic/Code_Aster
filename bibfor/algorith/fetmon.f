      SUBROUTINE FETMON(INFOFE,NBI2,NBI,NBTOT,NBSD,DIMGI,IFM,MAMOY,
     &                  LSTOGI,IFET1)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/01/2005   AUTEUR BOITEAU O.BOITEAU 
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  MONITORING DE ALFETI
C
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       14/01/05 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBI2,NBTOT,NBSD,DIMGI,IFM,MAMOY,NBI,IFET1
      LOGICAL      LSTOGI
      CHARACTER*24 INFOFE
      
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER    IFET2,IFET3,IFET4,IFET5,IFET6,I
      REAL*8     R1,R2,R3,R1M,R2M,R3M,RAUX2
      
      IF (INFOFE(9:9).EQ.'T') THEN
        WRITE(IFM,*)
        WRITE(IFM,*)'*****************************************'
        WRITE(IFM,*)'<FETI/ALFETI>'
        WRITE(IFM,*)'NB SOUS-DOMAINES ',NBSD
        WRITE(IFM,*)'NB DE MODES RIGIDES ',DIMGI
        WRITE(IFM,1081)NBI2,NBTOT,100.D0* NBI2/NBTOT
        CALL JEVEUO('&FETI.INFO.STOCKAGE.FVAF','L',IFET2)
        CALL JEVEUO('&FETI.INFO.STOCKAGE.FNBN','L',IFET3)
        CALL JEVEUO('&FETI.INFO.CPU.FACS','L',IFET4)
        CALL JEVEUO('&FETI.INFO.CPU.ASSE','L',IFET5)
        CALL JEVEUO('&FETI.INFO.CPU.FACN','L',IFET6)
        
        WRITE(IFM,*)'SOUS-DOMAINE/ MATRICE / FACTORISEE / NOEUDS '
        DO 15 I=1,NBSD
          WRITE(IFM,1075)I,ZI(IFET1+I-1),ZI(IFET2+I-1),ZI(IFET3+I-1)
   15   CONTINUE
        WRITE(IFM,*)'-----------------------------------'
        WRITE(IFM,1080)ZI(IFET1+NBSD),ZI(IFET2+NBSD),ZI(IFET3+NBSD)
        WRITE(IFM,1083)ZI(IFET1+NBSD)/NBSD,ZI(IFET2+NBSD)/NBSD,
     &                 ZI(IFET3+NBSD)/NBSD
        WRITE(IFM,*)'SOUS-DOMAINE/CPU FACSYM/CPU ASSE/CPU FACNUM '
        R1=0.D0
        R2=0.D0
        R3=0.D0
        R1M=-1.D0
        R2M=-1.D0
        R3M=-1.D0
        DO 16 I=0,NBSD
          WRITE(IFM,1084)I,ZR(IFET4+I),ZR(IFET5+I),ZR(IFET6+I)
          R1=R1+ZR(IFET4+I)
          R2=R2+ZR(IFET5+I)
          R3=R3+ZR(IFET6+I)
          IF (ZR(IFET4+I).GT.R1M) R1M=ZR(IFET4+I)
          IF (ZR(IFET5+I).GT.R2M) R2M=ZR(IFET5+I)
          IF (ZR(IFET6+I).GT.R3M) R3M=ZR(IFET6+I)         
   16   CONTINUE
        WRITE(IFM,*)'-----------------------------------'
        WRITE(IFM,1085)R1,R2,R3
        WRITE(IFM,1086)R1M,R2M,R3M
   
        IF (MAMOY.NE.0) THEN
          IF (LSTOGI) THEN
            RAUX2=(100.D0*DIMGI*(NBI+DIMGI))/MAMOY
          ELSE
            RAUX2=(100.D0*DIMGI*DIMGI)/MAMOY    
          ENDIF
        ENDIF
        WRITE(IFM,1082)RAUX2
        WRITE(IFM,*)'******************************************'
        CALL JEDETR('&FETI.INFO.STOCKAGE.FVAL')
        CALL JEDETR('&FETI.INFO.STOCKAGE.FVAF')
        CALL JEDETR('&FETI.INFO.STOCKAGE.FNBN')
        CALL JEDETR('&FETI.INFO.CPU.FACS')
        CALL JEDETR('&FETI.INFO.CPU.ASSE')      
        CALL JEDETR('&FETI.INFO.CPU.FACN')            
      ENDIF
 1075 FORMAT(' N ',I4,'     : ',I9,' ',I9,' ',I9)
 1080 FORMAT('TOTAL       :',I12,' ',I12,' ',I12)
 1081 FORMAT('POINTS INTERFACE / MAILLAGE / RAPPORT',I12,' ',I12,' ',
     &       D8.2,' %')
 1082 FORMAT('TAILLE (GI + GIT*GI)/MATRICE MOYENNE :',D10.2,' %')
 1083 FORMAT('MOYENNE     :',I12,' ',I12,' ',I12) 
 1084 FORMAT(' N ',I4,'     : ',D10.2,' ',D10.2,' ',D10.2)
 1085 FORMAT('CPU + SYS TOTAL  :',D10.2,' ',D10.2,' ',D10.2) 
 1086 FORMAT('CPU + SYS LA PIRE:',D10.2,' ',D10.2,' ',D10.2)       
      END 
