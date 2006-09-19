      SUBROUTINE MMMJEU(NDIM,JEUSUP,ICOMPL,NORM,BETA,GAMMA,DELTAT,
     &                  GEOME,GEOMM,
     &                  DEPLE,DEPLM,DEPLME,DEPLMM, 
     &                  VITME,VITMM,ACCME,ACCMM,    
     &                  JEU,JDEPP,JDEPM,JEVITP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
      IMPLICIT NONE
      INTEGER NDIM 
      REAL*8  JEUSUP
      REAL*8  NORM(3)
      INTEGER ICOMPL
      REAL*8  BETA,GAMMA,DELTAT
      REAL*8  GEOMM(3),GEOME(3)
      REAL*8  DEPLE(6),DEPLME(6)
      REAL*8  DEPLM(3),DEPLMM(3)      
      REAL*8  JEU,JDEPP,JDEPM,JEVITM,JEACCM,JEVITP
      REAL*8  ACCME(6),ACCMM(6)
      REAL*8  VITME(6),VITMM(6)
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0364/TE0365
C ----------------------------------------------------------------------
C
C CALCUL DES JEUX 
C
C !!!! METTRE A JOUR !!!!!
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  JEUSUP : JEU SUPPLEMENTAIRE PAR DIST_ESCL/DIST_MAIT
C IN  ICOMPL : VAUT 1 SI CALCUL DE LA COMPLIANCE
C IN  BETA   : PARAMETRE POUR COMPLIANCE
C IN  GAMMA  : PARAMETRE POUR COMPLIANCE
C IN  DELTA  : INCREMENT DE TEMPS POUR COMPLIANCE
C IN  NORM   : VALEUR DE LA NORMALE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER K
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      JEU    = JEUSUP
      JDEPP  = 0.D0
      JDEPM  = 0.D0
      JEVITM = 0.D0 
      JEACCM = 0.D0  
      JEVITP = 0.D0    
      DO 10 K = 1,NDIM
        JEU   = JEU + (GEOME(K)+DEPLE(K)-GEOMM(K)-DEPLM(K))*NORM(K)   
        JDEPP = JDEPP + (DEPLE(K)-DEPLM(K))*NORM(K)
        JDEPM = JDEPM + (DEPLME(K)-DEPLMM(K))*NORM(K)
 10   CONTINUE
      IF (ICOMPL .EQ. 1) THEN
        DO 20 K = 1,NDIM
          JEVITM = JEVITM + (VITME(K)-VITMM(K))*NORM(K)
          JEACCM = JEACCM + (ACCME(K)-ACCMM(K))*NORM(K)
 20     CONTINUE    
        JEVITP = (JDEPP-JDEPM)*GAMMA/(BETA*DELTAT) +
     &            JEVITM*(BETA-GAMMA)/BETA +
     &            JEACCM*DELTAT*(2*BETA-GAMMA)/(2*BETA)   
      ENDIF             
C
      CALL JEDEMA()      
      END
