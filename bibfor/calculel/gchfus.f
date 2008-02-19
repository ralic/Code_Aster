      SUBROUTINE GCHFUS(FONCT1,FONCT2,FONCT3)
      IMPLICIT NONE
      CHARACTER*(*) FONCT1,FONCT2,FONCT3
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C
C     BUT : DETERMINE UNE FONCTION A PARTIR DE 2 FONCTIONS.
C           LES VALEURS DE LA FONCTION OUT CORRESPONDENT
C           A LA SOMMES DES VALEURS DES FONCTIONS IN
C
C     IN :  FONCT1
C
C ======================================================================
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ----------------------------------------------------------------------
      INTEGER      NPTF1,NPTF2,JPROL,JVALE,IRET,JVAL,NPTF,I
      REAL*8       Y
      CHARACTER*8  K8B
      CHARACTER*16 PARA
      CHARACTER*19 FO1,FO2,FO3,FOTMP1,FOTMP2
C
      CALL JEMARQ()

      FO1=FONCT1
      FO2=FONCT2
      FO3=FONCT3

      CALL JELIRA(FO1//'.VALE','LONMAX',NPTF1,K8B)
      CALL JELIRA(FO2//'.VALE','LONMAX',NPTF2,K8B)
C
      NPTF1=NPTF1/2
      NPTF2=NPTF2/2

      IF(NPTF1.GE.NPTF2)THEN
         CALL COPISD('FONCTION','V',FO1,FO3)
         FOTMP1=FO2
         FOTMP2=FO1
         NPTF=NPTF1
      ELSE
         CALL COPISD('FONCTION','V',FO2,FO3)
         FOTMP1=FO1
         FOTMP2=FO2
         NPTF=NPTF2
      ENDIF
    
      CALL JEVEUO(FOTMP1//'.PROL','L',JPROL)
      CALL JEVEUO(FOTMP2//'.VALE','L',JVAL)
      CALL JEVEUO(FO3//'.VALE','E',JVALE)

      DO 10 I=1,NPTF
         CALL FOINTE('A',FOTMP1,1,ZK24(JPROL+3-1),ZR(JVAL+I-1),Y,IRET)
         ZR(JVALE+NPTF+I-1)=ZR(JVAL+NPTF+I-1)+Y
 10   CONTINUE

      CALL JEDEMA()

      END
