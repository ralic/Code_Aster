      SUBROUTINE SURFAT(NA,NB,NC,S,IVALE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/01/98   AUTEUR ADBHHPM P.MASSIN 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER  IVALE,NA,NB,NC
C----------------------------------------------------------------------
C     CALCUL DE LA SURFACE D'UN TRIANGLE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX -----------------------------
C
      CALL JEMARQ()
C DEBUT----------------------------------------------------------------
C     COORDONNEES DES SOMMETS DU TRIANGLE
C
      XNA = ZR(IVALE-1+(NA-1)*3+1)
      YNA = ZR(IVALE-1+(NA-1)*3+2)
      ZNA = ZR(IVALE-1+(NA-1)*3+3)
C      WRITE(6,*)'XNA ',XNA,'YNA ',YNA,'ZNA ',ZNA
C
      XNB = ZR(IVALE-1+(NB-1)*3+1)
      YNB = ZR(IVALE-1+(NB-1)*3+2)
      ZNB = ZR(IVALE-1+(NB-1)*3+3)
C      WRITE(6,*)'XNB ',XNB,'YNB ',YNB,'ZNB ',ZNB
C
      XNC = ZR(IVALE-1+(NC-1)*3+1)
      YNC = ZR(IVALE-1+(NC-1)*3+2)
      ZNC = ZR(IVALE-1+(NC-1)*3+3)
C      WRITE(6,*)'XNC ',XNC,'YNC ',YNC,'ZNC ',ZNC
C
C     CALCUL DE LA SURFACE PAR LA NORME DU PRODUIT VECTORIEL

      XVECT = (YNB-YNA)*(ZNC-ZNA)-(ZNB-ZNA)*(YNC-YNA)
      YVECT = (ZNB-ZNA)*(XNC-XNA)-(XNB-XNA)*(ZNC-ZNA)
      ZVECT = (XNB-XNA)*(YNC-YNA)-(YNB-YNA)*(XNC-XNA)
C      WRITE(6,*)'XVECT ',XVECT,'YVECT ',YVECT,'ZVECT ',ZVECT
C
      S = 0.5D0*SQRT(XVECT*XVECT+YVECT*YVECT+ZVECT*ZVECT)
C      WRITE(6,*)'SURFACE',S
C FIN -----------------------------------------------------------------
      CALL JEDEMA()
      END
