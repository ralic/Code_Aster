      SUBROUTINE AFFDIS(NDIM,IREP,ETA,CAR,VAL,JDC,JDV,JDCNS,JDVNS,
     +                  IVR,IV,KMA,NCMP,NTP,JDCINF,JDVINF,ISYM,IFM)
      IMPLICIT   NONE
      INTEGER           NDIM,IREP,JDV(3),JDC(3),IVR(*),IV,NCMP,NTP,IFM
      INTEGER           JDVNS(3),JDCNS(3),ISYM,JDCINF,JDVINF
      REAL*8                 ETA,    VAL(*)
      CHARACTER*1             KMA(3)
      CHARACTER*9                CAR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/10/2008   AUTEUR TORKHANI M.TORKHANI 
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
C     ------------------------------------------------------------------
C     AFFECTATION DES VALEURS DES MATRICES A TOUS LES ELEMENTS
C     DEMANDES PAR L UTILISATEUR DANS LES CARTES CORRESPONDANTES
C     LES ELEMENTS CONCERNES SONT LES ELEMENTS DISCRETS 2D ET 3D
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      CHARACTER*7  KI
C     ------------------------------------------------------------------
C
      IF (NDIM.EQ.2) THEN
         CALL AFDI2D(IREP,ETA,CAR,VAL,JDC,JDV,JDCNS,JDVNS,IVR,IV,KMA,
     &               NCMP,NTP,JDCINF,JDVINF,ISYM,IFM)
      ELSEIF (NDIM.EQ.3) THEN
         CALL AFDI3D(IREP,ETA,CAR,VAL,JDC,JDV,JDCNS,JDVNS,IVR,IV,KMA,
     &               NCMP,NTP,JDCINF,JDVINF,ISYM,IFM)
      ENDIF
C
      END
