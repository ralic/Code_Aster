      SUBROUTINE NMRLDB (LMAT,RCINE,RESU,NBSM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2002   AUTEUR ROSE C.ROSE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            LMAT, NBSM
      REAL*8                  RCINE(*),RESU(*)
C ----------------------------------------------------------------------
C     CALCUL DE RESU = MAT-1(RESU,RCINE)
C     NON LINEAIRE
C
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE
C IN  RCINE   : .VALE DU CHAM_NO CINEMATIQUE
C VAR RESU    : .VALE DU CHAM_NO RESULTAT EN OUT , SMB EN IN
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*8  KBID
      CHARACTER*24 MAT
      COMPLEX*16   CBID
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------
      MAT = ZK24(ZI(LMAT+1))
      NEQ = ZI(LMAT+2)
      CALL JEEXIN(MAT(1:19)//'.CONI',IRCONI)
      IF ( IRCONI.NE.0 ) THEN
         DO 1 I=1,NBSM
            CALL CSMBGG(LMAT,RESU(NEQ*(I-1)+1) ,RCINE,CBID,CBID,'R')
 1       CONTINUE
      ENDIF
      DO 2 I=1,NBSM
         CALL MRCONL (LMAT,NEQ,'R',RESU(NEQ*(I-1)+1),1)
 2    CONTINUE
      CALL RLDLGB (LMAT,RESU,CBID,NBSM)
      DO 3 I=1,NBSM
         CALL MRCONL (LMAT,NEQ,'R',RESU(NEQ*(I-1)+1),1)
 3    CONTINUE
      END
