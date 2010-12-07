      SUBROUTINE MMMRED(NDIMG ,LCTFC ,CHAMP ,CHAMPR,NDD1  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      NDIMG
      CHARACTER*19 CHAMP,CHAMPR
      LOGICAL      LCTFC
      INTEGER      NDD1
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
C
C REDUCTION DU CHAMP SUR LES DDL
C
C ----------------------------------------------------------------------
C
C
C IN  NDIMG  : DIMENSION DE L'ESPACE
C IN  LCTFC  : .TRUE. SI FROTTEMENT
C IN  CHAMP  : CHAM_NO A REDUIRE
C OUT CHAMPR : CHAM_NO_S REDUIT DE L'INCREMENT DE DEPLACEMENT CUMULE
C OUT NDD1   : NOMBRE DE DDL/NOEUD
C
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C 
      CHARACTER*8  LICMP4(4),LICMP6(6)
      CHARACTER*19 CHAMPS
C ----------------------------------------------------------------------
      DATA LICMP4
     &   / 'DX'     ,'DY'      ,
     &     'LAGS_C' ,'LAGS_F1' /
      DATA LICMP6
     &   / 'DX'     ,'DY'      ,'DZ'      ,
     &     'LAGS_C' ,'LAGS_F1' ,'LAGS_F2' /      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- TRANSFORMATION DU CHAM_NO EN CHAM_NO_S
C
      CHAMPS = '&&MMMRED.CHAMPS'
      CALL CNOCNS(CHAMP,'V',CHAMPS)
C
C --- REDUCTION DU CHAM_NO_S DES DDL EN UN CHAM_NO_S DES LAGRANGES
C --- DE CONTACT/FROTTEMENT
C
      IF (NDIMG.EQ.3) THEN
        IF ( LCTFC ) THEN
          NDD1 = 6
        ELSE
          NDD1 = 4
        ENDIF
        CALL CNSRED(CHAMPS,0,0,NDD1,LICMP6,'V',CHAMPR)
      ELSE IF (NDIMG.EQ.2) THEN
        IF ( LCTFC ) THEN
          NDD1 = 4
        ELSE
          NDD1 = 3
        ENDIF
        CALL CNSRED(CHAMPS,0,0,NDD1,LICMP4,'V',CHAMPR)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
      CALL DETRSD('CHAMP',CHAMPS)
C
      CALL JEDEMA()
      END
