      SUBROUTINE FOORDN (VECPAR , VECNOM , NE , NS , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                              NE , NS , IER
      REAL*8            VECPAR(NE)
      CHARACTER*(*)               VECNOM(NE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
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
C     REORDONNE UNE NAPPE PAR ORDRE CROISSANT DU PARAMETRE
C ----------------------------------------------------------------------
C IN  : VECPAR : VECTEUR DES VALEURS DU PARAMETRE
C IN  : VECNOM : VECTEUR DES NOMS DES FONCTIONS
C IN  : NE     : NOMBRE DE FONCTIONS ET PARAMETRES
C OUT : NS     : NE - EVENTUELLEMENT LE NOMBRE DE DOUBLONS
C OUT : IER    : = 0 , SI TOUT VA BIEN
C                = 1 , SINON
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32  JEXNUM
      INTEGER       ZI
      INTEGER VALI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*8 K8BID
C     ------------------------------------------------------------------
      REAL*8       X
      CHARACTER*24 C
C     ------------------------------------------------------------------
C
C     --- RANGEMENT EN ORDRE CROISSANT ---
      CALL JEMARQ()
      IER = 0
      DO 10 I = 1,NE-1
         DO 10 J = I+1,NE
            IF ( VECPAR(I).GT.VECPAR(J) ) THEN
               X         = VECPAR(I)
               C         = VECNOM(I)
               VECPAR(I) = VECPAR(J)
               VECNOM(I) = VECNOM(J)
               VECPAR(J) = X
               VECNOM(J) = C
            ENDIF
 10   CONTINUE
C
C     --- SUPPRESSION DES DOUBLONS ---
      NS = NE
      DO 20 I = 1,NE-1
         IF ( VECPAR(I).EQ.VECPAR(I+1) ) THEN
            IF ( VECNOM(I).NE.VECNOM(I+1) ) THEN
               IF ( VECNOM(I)  (1:1).EQ. '&' .AND.
     +              VECNOM(I+1)(1:1).EQ. '&' ) THEN
                  CALL JELIRA(VECNOM(I)  ,'LONUTI',NBVAL1,K8BID)
                  CALL JELIRA(VECNOM(I+1),'LONUTI',NBVAL2,K8BID)
                  IF (NBVAL1.EQ.NBVAL2) THEN
                     CALL JEVEUO(VECNOM(I)  ,'L',LVAL1)
                     CALL JEVEUO(VECNOM(I+1),'L',LVAL2)
                     DO 22 IVAL = 0,NBVAL1-1
                        IF (ZR(LVAL1+IVAL).NE.ZR(LVAL2+IVAL)) IER = 1
 22                  CONTINUE
                  ELSE
                     IER = 1
                  ENDIF
               ELSE
                  IER = 1
               ENDIF
            ELSE
               DO 24 J = I,NE-1
                  VECPAR(J) = VECPAR(J+1)
                  VECNOM(J) = VECNOM(J+1)
 24            CONTINUE
               NS = NS - 1
            ENDIF
         ENDIF
 20   CONTINUE
      IF ( NE .NE. NS ) THEN
      K = NE - NS
      VALI = K
      CALL U2MESG('F', 'UTILITAI6_38',0,' ',1,VALI,0,0.D0)
      ENDIF
C
      CALL JEDEMA()
      END
