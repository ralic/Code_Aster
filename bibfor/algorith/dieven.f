      SUBROUTINE DIEVEN(SDDISC,EVEN,IOCC,VERIF)
      IMPLICIT   NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/02/2011   AUTEUR MASSIN P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*19 SDDISC
      CHARACTER*(*) EVEN
      INTEGER       IOCC
      LOGICAL       VERIF
C
C ----------------------------------------------------------------------
C
C
C ROUTINE UTILITAIRE PERMETTANT DE TROUVER L'OCCURENCE
C ET LA VALEUR D'UN EVENEMENT DANS LA SD DISCRETISATION
C
C ----------------------------------------------------------------------
C
C IN  SDDISC : SDDISC LOCALE A OP0070
C IN  EVEN   : NOM D'EVENEMENT
C OUT  IOCC  : NUMERO D'ORDRE
C OUT  VERIF : TESTE SI L'EVENEMENT EST VERIFIE
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C      
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*8   REP,K8B
      CHARACTER*24  NOM
      INTEGER       NOCC,IEVEN,IB
      REAL*8        R8B
C
C ----------------------------------------------------------------------
C      
      
      IOCC = 0
      CALL UTDIDT('L',SDDISC,'ECHE',IB,'NB_OCC',R8B,NOCC,K8B)
      DO 10 IEVEN = 1,NOCC
          CALL UTDIDT('L',SDDISC,'ECHE',IEVEN,
     &     'NOM_EVEN',R8B,IB,NOM)
          IF(NOM.EQ.EVEN) THEN
             IOCC = IEVEN
             CALL UTDIDT('L',SDDISC,'ECHE',IEVEN,
     &        'VERIF_EVEN',R8B,IB,REP)
             VERIF = (REP.EQ.'OUI')
             GOTO 11             
          ENDIF                   
10    CONTINUE

11    CONTINUE      
      END
