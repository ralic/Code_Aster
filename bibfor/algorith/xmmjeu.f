      SUBROUTINE XMMJEU(NDIM,NORM,GEOME,GEOMM,DEPLE,DEPLM,
     &                  DEPLME,DEPLMM,JEU,JDEPP,JDEPM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/10/2007   AUTEUR NISTOR I.NISTOR 
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
      REAL*8  NORM(3)
      REAL*8  GEOMM(3),GEOME(3)
      REAL*8  DEPLE(6),DEPLME(6)
      REAL*8  DEPLM(3),DEPLMM(3)      
      REAL*8  JEU,JDEPP,JDEPM

C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0366/TE0367
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C CALCUL DES JEUX
C 
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NORM   : VALEUR DE LA NORMALE
C IN  GEOME  : COORDONNÉES RÉELES DU POINT DE CONTACT
C IN  GEOMM  : COORDONNÉES RÉELES DU PROJETE DU POINT DE CONTACT
C IN  DEPLE  : DEPLACEMENTS DU POINT DE CONTACT
C IN  DEPLM  : DEPLACEMENTS DU PROJETE DU POINT DE CONTACT
C IN  DEPLME : DEPLACEMENTS PRECEDENTS DU POINT DE CONTACT
C IN  DEPLMM : DEPLACEMENTS PRECEDENTS DU PROJETE DU POINT DE CONTACT
C OUT JEU    : VALEUR DU JEU
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
      JEU    = 0.D0
      JDEPP  = 0.D0
      JDEPM  = 0.D0

      DO 10 K = 1,NDIM

        JEU   = JEU + 
     &          (GEOME(K)+DEPLE(K)-DEPLE(K+NDIM)-
     &           GEOMM(K)-DEPLM(K)-DEPLM(K+NDIM))*NORM(K)
  
        JDEPP = JDEPP + (DEPLE(K)-DEPLE(K+NDIM)-
     &          DEPLM(K)-DEPLM(K+NDIM))*NORM(K)
        JDEPM = JDEPM + (DEPLME(K)-DEPLME(K+NDIM)-
     &          DEPLMM(K)-DEPLMM(K+NDIM))*NORM(K)
 10   CONTINUE
     
      CALL JEDEMA()      
      END
