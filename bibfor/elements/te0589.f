      SUBROUTINE TE0589 ( OPTION , NOMTE )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2002   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C
C    - FONCTION REALISEE:  CALCUL DU SECOND MEMBRE : TRAVAIL DE LA
C                          DILATATION THERMIQUE
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*8        ELREFE
      INTEGER            NBRDDM
      PARAMETER          (NBRDDM=156)
      REAL*8             F(NBRDDM),B(4,NBRDDM),VOUT(NBRDDM)
      REAL*8             VTEMP(NBRDDM),PASS(NBRDDM,NBRDDM)
      REAL*8             MAT(NBRDDM,4)
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*24       CARAC
      INTEGER            NNO,M,ICARAC,NBRDDL
C
      CALL ELREF1(ELREFE)

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
C
      NNO   = ZI(ICARAC  )
      M     = ZI(ICARAC+6)
C
C     FORMULE GENERALE
C
      NBRDDL = NNO*(6+3+6*(M-1))
C
C     VERIFS PRAGMATIQUES
C
      IF (NBRDDL.GT.NBRDDM) THEN
         CALL UTMESS('F','TUYAU','LE NOMBRE DE DDL EST TROP GRAND')
      ENDIF
      IF (NOMTE.EQ.'MET3SEG3') THEN
         IF (NBRDDL.NE.63) THEN
            CALL UTMESS('F','MET3SEG3','LE NOMBRE DE DDL EST FAUX')
         ENDIF
      ELSEIF (NOMTE.EQ.'MET6SEG3') THEN
         IF (NBRDDL.NE.117) THEN
            CALL UTMESS('F','MET6SEG3','LE NOMBRE DE DDL EST FAUX')
         ENDIF
      ELSEIF (NOMTE.EQ.'MET3SEG4') THEN
         IF (NBRDDL.NE.84) THEN
            CALL UTMESS('F','MET3SEG4','LE NOMBRE DE DDL EST FAUX')
         ENDIF
      ELSE
         CALL UTMESS('F','TUYAU','NOM DE TYPE ELEMENT INATTENDU')
      ENDIF
C
      CALL TUTEMP(OPTION,ELREFE,NBRDDL,F,B,VOUT,MAT,PASS,VTEMP)
      END
