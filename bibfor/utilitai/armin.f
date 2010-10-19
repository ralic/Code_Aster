      REAL*8 FUNCTION ARMIN(NOMAZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/10/2010   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT NONE
      CHARACTER*8  NOMAZ
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE - MAILLAGE
C
C CETTE FONCTION PERMET DE RECUPERER LA PLUS PETITE ARETE DU MAILLAGE
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA  : NOM DU MODELE
C OUT ARMIN : TAILLE DE LA PLUS PETITE ARETE DU MAILLAGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                   ZK24
      CHARACTER*32                           ZK32
      CHARACTER*80                                   ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*8  NOMA,K8B
      CHARACTER*19 NOMT19
      CHARACTER*24 PARA
      INTEGER      IBID,IER
      INTEGER      NBPAR
      REAL*8       R8B,ARETE
      COMPLEX*16   CBID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
C
      NOMA = NOMAZ
      CALL JEEXIN(NOMA//'           .LTNT',IER   )
      IF (IER.NE.0) THEN
         CALL LTNOTB(NOMA  ,'CARA_GEOM',NOMT19)
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA(NOMT19,NBPAR ,' '   ,IBID  ,R8B   ,CBID  ,K8B   ,
     &               K8B   ,R8B   ,PARA  ,K8B   ,IBID  ,ARETE ,CBID  ,
     &               K8B   ,IER   )
         IF (IER.EQ.0) THEN
            ARMIN = ARETE
         ELSE
            CALL U2MESS('F','MODELISA2_13')
         ENDIF
      ELSE
         CALL U2MESS('F','MODELISA3_18')
      ENDIF
C
      CALL JEDEMA()
C
      END
