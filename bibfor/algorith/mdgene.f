      SUBROUTINE MDGENE (BASEMO,NBMODE,NUMGEN,MASGEN,RIGGEN,AMOGEN,
     &                                                 NEXCIT,JVEC,IER)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                   NBMODE,                NEXCIT,JVEC,IER
      CHARACTER*8        BASEMO,              MASGEN,RIGGEN,AMOGEN
      CHARACTER*14                     NUMGEN
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     VERIFICATION DES DONNEES GENERALISEES
C     ------------------------------------------------------------------
C IN  : BASEMO : NOM DU CONCEPT BASE MODALE
C IN  : NBMODE : NOMBRE DE MODES
C IN  : NUMGEN : NOM DU CONCEPT NUMEROTATION GENERALISEE
C IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
C IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
C IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
C IN  : JVEC   : ADRESSE JEVEUX DES NOMS DES VECTEURS GENERALISES
C OUT : IER    : CODE RETOUR
C ----------------------------------------------------------------------
C
C
C
C
      INTEGER       NVEC1, NVEC2
      CHARACTER*8   BASE1, BASE2,K8B,VECGEN
      CHARACTER*14  NU1GEN,NU2GEN,NU3GEN,NU4GEN,K14B
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,JDES1 ,JDES2 ,JREF1 ,JREF2 
C-----------------------------------------------------------------------
      DATA K8B /'        '/
      DATA K14B /'              '/
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
C
      IF (NUMGEN.EQ.K14B) THEN
C
C     --- SI MATR_GENE (STOCKAGE PLEIN) ---
C
        CALL JEVEUO(MASGEN//'           .REFA','L',JREF1)
        CALL JEVEUO(RIGGEN//'           .REFA','L',JREF2)
        BASE1 = ZK24(JREF1)(1:8)
        BASE2 = ZK24(JREF2)(1:8)
        IF (BASE1.NE.BASEMO) THEN
          IER = IER + 1
          CALL U2MESS('E','ALGORITH5_42')
        ENDIF
        IF (BASE2.NE.BASEMO) THEN
          IER = IER + 1
          CALL U2MESS('E','ALGORITH5_42')
        ENDIF
C
        CALL JEVEUO(MASGEN//'           .DESC','L',JDES1)
        CALL JEVEUO(RIGGEN//'           .DESC','L',JDES2)
        NVEC1 = ZI(JDES1+1)
        NVEC2 = ZI(JDES2+1)
        IF (NVEC1.NE.NBMODE) THEN
          IER = IER + 1
        CALL U2MESS('E','ALGORITH5_43')
        ENDIF
        IF (NVEC2.NE.NBMODE) THEN
          IER = IER + 1
        CALL U2MESS('E','ALGORITH5_43')
        ENDIF
C
        IF (AMOGEN.NE.K8B) THEN
          CALL JEVEUO(AMOGEN//'           .REFA','L',JREF1)
          BASE1 = ZK24(JREF1)(1:8)
          IF (BASE1.NE.BASEMO) THEN
            IER = IER + 1
            CALL U2MESS('E','ALGORITH5_42')
          ENDIF
          CALL JEVEUO(AMOGEN//'           .DESC','L',JDES1)
          NVEC1 = ZI(JDES1+1)
          IF (NVEC1.NE.NBMODE) THEN
            IER = IER + 1
          CALL U2MESS('E','ALGORITH5_43')
          ENDIF
        ENDIF
C
      ELSEIF (BASEMO.EQ.K8B) THEN
C
C     --- SI MATR_ASSE_GENE_R (STOCKAGE LIGNE DE CIEL) ---
C
        CALL JEVEUO(MASGEN//'           .REFA','L',JREF1)
        CALL JEVEUO(RIGGEN//'           .REFA','L',JREF2)
        NU1GEN = ZK24(JREF1+1)(1:14)
        NU2GEN = ZK24(JREF2+1)(1:14)
        IF (NU1GEN.NE.NUMGEN) THEN
          IER = IER + 1
          CALL U2MESS('E','ALGORITH5_44')
        ENDIF
        IF (NU2GEN.NE.NUMGEN) THEN
          IER = IER + 1
          CALL U2MESS('E','ALGORITH5_44')
        ENDIF
C
        IF (AMOGEN.NE.K8B) THEN
          CALL JEVEUO(AMOGEN//'           .REFA','L',JREF1)
          NU3GEN = ZK24(JREF1+1)(1:14)
          IF (NU3GEN.NE.NUMGEN) THEN
            IER = IER + 1
            CALL U2MESS('E','ALGORITH5_44')
          ENDIF
        ENDIF
C
        IF (NEXCIT.NE.0) THEN
          DO 10 I=1,NEXCIT
            VECGEN = ZK8(JVEC-1+I)
            CALL JEVEUO(VECGEN//'           .REFE','L',JREF1)
            NU4GEN = ZK24(JREF1+1)(1:14)
            IF (NU4GEN.NE.NUMGEN) THEN
              IER = IER + 1
            CALL U2MESS('E','ALGORITH5_45')
            ENDIF
10        CONTINUE
        ENDIF
C
      ENDIF
C
      CALL JEDEMA()
      END
