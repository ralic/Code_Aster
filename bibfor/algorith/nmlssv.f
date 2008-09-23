      SUBROUTINE NMLSSV(MODE  ,LISCHA,NBSST)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*4   MODE 
      INTEGER       NBSST
      CHARACTER*19  LISCHA
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - SOUS-STRUCTURATION)
C
C LECTURE ET PREPARATION POUR SOUS_STRUCT 
C      
C ----------------------------------------------------------------------
C
C
C IN  MODE   : TYPE d'OPERATION
C              'LECT' LIT LE MOT-CLEF 'SOUS_STRUC'
C              'INIT' CREE LE VECTEUR DES CHARGES POUR SOUS_STRUC
C IN  LISCHA : SD L_CHARGES
C OUT NBSST  : NOMBRE de SOUS-STRUCTURES
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
      CHARACTER*24 FOMUL2
      INTEGER      JFOMU2 
      INTEGER      I,IBID      
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C   
      NBSST  = 0
      IF (MODE.EQ.'LECT') THEN
        CALL GETFAC('SOUS_STRUC',NBSST)
      ELSEIF (MODE.EQ.'INIT') THEN
        CALL GETFAC('SOUS_STRUC',NBSST)
        IF (NBSST.GT.0) THEN
          FOMUL2 = LISCHA(1:19)//'.FCSS'
          CALL WKVECT(FOMUL2,'V V K24',NBSST,JFOMU2)
          DO 1 I = 1,NBSST
            CALL GETVID('SOUS_STRUC','FONC_MULT',I,1,1,
     &                  ZK24(JFOMU2+I-1),IBID)
            IF (IBID.EQ.0) THEN
              ZK24(JFOMU2+I-1) = '&&CONSTA' 
            ENDIF  
    1     CONTINUE
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF    
      CALL JEDEMA()      
C
      END
