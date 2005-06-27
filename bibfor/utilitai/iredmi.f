      SUBROUTINE IREDMI ( MACR )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       MACR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/06/2005   AUTEUR NICOLAS O.NICOLAS 
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
C     INTERFACE ASTER - MISS3D : PROCEDURE  IMPR_MACR_ELEM
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8  K8B, MAEL, BASEMO, MASSE, NOMA, INTERF, LISTAM
      CHARACTER*16 NOMCMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      MAEL = MACR
      CALL GETRES( K8B, K8B, NOMCMD )
      PI = R8PI()
      PETIR8 = 1.D-40
C
C     ----- RECUPERATION DES MODES -----
      CALL JEVEUO(MAEL//'.MAEL      .REFE','L',JREFE)
      BASEMO = ZK24(JREFE)(1:8)
      NOMA   = ZK24(JREFE+1)(1:8)
      CALL JELIRA(BASEMO//'           .ORDR','LONMAX',NBMODT,K8B)
      CALL JEVEUO(BASEMO//'           .REFD','L',JVAL)
      INTERF = ZK24(JVAL+4) (1:8)
      IF (INTERF.NE.' ') THEN
       CALL BMNBMD(BASEMO,'MODE',NBMODE)
       CALL BMNBMD(BASEMO,'DEFORMEE',NBMODS)
      ELSE
       CALL JEVEUO(BASEMO//'           .UTIL','L',JVAL)
       NBMODE = ZI(JVAL+2)
       NBMODS = ZI(JVAL+3)
      ENDIF
      NBMODT = NBMODE + NBMODS
C
      CALL JEVEUO(MAEL//'.MAEL.MASS .REFE','L',JREFE)
      MASSE = ZK24(JREFE+1)
C
C     ----- RECUPERATION DES FREQUENCES -----
      CALL JEVEUO(BASEMO//'           .FREQ','L',JFREQ)
C
C     ----- EXTRACTION DU MACRO-ELEMENT DYNAMIQUE -----
C
      IF (NBMODE.EQ.0) THEN
       CALL WKVECT('&&IREDMI.DMASS','V V R',1,JMASS)
       CALL WKVECT('&&IREDMI.DRIGI','V V R',1,JRIGI)
       CALL WKVECT('&&IREDMI.DAMOR','V V R',1,LAMOR)
      ELSE
       CALL WKVECT('&&IREDMI.DMASS','V V R',NBMODE*NBMODE,JMASS)
       CALL WKVECT('&&IREDMI.DRIGI','V V R',NBMODE*NBMODE,JRIGI)
       CALL WKVECT('&&IREDMI.DAMOR','V V R',NBMODE*NBMODE,LAMOR)
      ENDIF
      IF (NBMODS.EQ.0) THEN
       CALL WKVECT('&&IREDMI.SMASS','V V R',1,ISMASS)
       CALL WKVECT('&&IREDMI.SRIGI','V V R',1,ISRIGI)
       CALL WKVECT('&&IREDMI.SAMOR','V V R',1,ISAMOR)
      ELSE
       CALL WKVECT('&&IREDMI.SMASS','V V R',NBMODS*NBMODS,ISMASS)
       CALL WKVECT('&&IREDMI.SRIGI','V V R',NBMODS*NBMODS,ISRIGI)
       CALL WKVECT('&&IREDMI.SAMOR','V V R',NBMODS*NBMODS,ISAMOR)
      ENDIF
      IF (NBMODE.EQ.0.OR.NBMODS.EQ.0) THEN
       CALL WKVECT('&&IREDMI.CMASS','V V R',1,ICMASS)
       CALL WKVECT('&&IREDMI.CRIGI','V V R',1,ICRIGI)
       CALL WKVECT('&&IREDMI.CAMOR','V V R',1,ICAMOR)
      ELSE
       CALL WKVECT('&&IREDMI.CMASS','V V R',NBMODE*NBMODS,ICMASS)
       CALL WKVECT('&&IREDMI.CRIGI','V V R',NBMODE*NBMODS,ICRIGI)
       CALL WKVECT('&&IREDMI.CAMOR','V V R',NBMODE*NBMODS,ICAMOR)
      ENDIF
C
      CALL JEVEUO(MAEL//'.MAEL.MASS .VALE','L',IVAL1)
      CALL JEVEUO(MAEL//'.MAEL.RAID .VALE','L',IVAL2)
      DO 20 J = 1,NBMODE
        DO 21 I = 1,J
          K =J*(J-1)/2 + I
          ZR(JMASS+I-1+(J-1)*NBMODE) = ZR(IVAL1+K-1) + PETIR8
          ZR(JMASS+J-1+(I-1)*NBMODE) = ZR(IVAL1+K-1) + PETIR8
          ZR(JRIGI+I-1+(J-1)*NBMODE) = ZR(IVAL2+K-1) + PETIR8
          ZR(JRIGI+J-1+(I-1)*NBMODE) = ZR(IVAL2+K-1) + PETIR8
 21     CONTINUE
 20   CONTINUE
      DO 22 J = NBMODE+1,NBMODT
         DO 23 I = 1,NBMODE
           K = J*(J-1)/2 + I
           J2 = J - NBMODE
           ZR(ICMASS+J2-1+(I-1)*NBMODS) = ZR(IVAL1+K-1) + PETIR8
           ZR(ICRIGI+J2-1+(I-1)*NBMODS) = ZR(IVAL2+K-1) + PETIR8
 23      CONTINUE
         DO 24 I = NBMODE+1,J
           K = J*(J-1)/2 + I
           I2 = I - NBMODE
           J2 = J - NBMODE
           ZR(ISMASS+I2-1+(J2-1)*NBMODS) = ZR(IVAL1+K-1) + PETIR8
           ZR(ISMASS+J2-1+(I2-1)*NBMODS) = ZR(IVAL1+K-1) + PETIR8
           ZR(ISRIGI+I2-1+(J2-1)*NBMODS) = ZR(IVAL2+K-1) + PETIR8
           ZR(ISRIGI+J2-1+(I2-1)*NBMODS) = ZR(IVAL2+K-1) + PETIR8
 24      CONTINUE
 22   CONTINUE
C
      CALL JEEXIN(MAEL//'.MAEL.AMOR .VALE',IRET)
      IF ( IRET .NE. 0 ) THEN
         CALL JEVEUO(MAEL//'.MAEL.AMOR .VALE','L',IVAL3)
         DO 30 J = 1,NBMODE
           DO 31 I = 1,J
             K =J*(J-1)/2 + I
             ZR(LAMOR+I-1+(J-1)*NBMODE) = ZR(IVAL3+K-1) + PETIR8
             ZR(LAMOR+J-1+(I-1)*NBMODE) = ZR(IVAL3+K-1) + PETIR8
 31        CONTINUE
 30      CONTINUE
         DO 32 J = NBMODE+1,NBMODT
            DO 33 I = 1,NBMODE
               K = J*(J-1)/2 + I
               J2 = J - NBMODE
               ZR(ICAMOR+J2-1+(I-1)*NBMODS) = ZR(IVAL3+K-1) + PETIR8
 33         CONTINUE
            DO 34 I = NBMODE+1,J
               K = J*(J-1)/2 + I
               I2 = I - NBMODE
               J2 = J - NBMODE
               ZR(ISAMOR+I2-1+(J2-1)*NBMODS) = ZR(IVAL3+K-1) + PETIR8
               ZR(ISAMOR+J2-1+(I2-1)*NBMODS) = ZR(IVAL3+K-1) + PETIR8
 34         CONTINUE
 32      CONTINUE
      ELSE
         IVAL3 = 0
      ENDIF
C
C     ----- RECUPERATION DES AMORTISSEMENTS -----
      CALL GETVR8(' ','AMOR_REDUIT',0,1,0,R8B,N1)
      CALL GETVID(' ','LIST_AMOR',0,1,0,K8B,N2)
      IF (NBMODE.EQ.0) THEN
       CALL WKVECT('&&IREDMI.AMORTISSEMENT','V V R',1,JAMOR)
      ELSE
       CALL WKVECT('&&IREDMI.AMORTISSEMENT','V V R',NBMODE,JAMOR)
      ENDIF
      IF (N1.NE.0.OR.N2.NE.0) THEN
         IF (N1.NE.0) THEN
            NBAMOR = -N1
            CALL GETVR8(' ','AMOR_REDUIT',1,1,NBAMOR,ZR(JAMOR),N1)
         ELSE
            CALL GETVID(' ','LIST_AMOR',0,1,1,LISTAM,N2)
            CALL JELIRA(LISTAM//'           .VALE','LONMAX',NBAMOR,K8B)
            CALL JEVEUO(LISTAM//'           .VALE','L',JAMOR)
         ENDIF
         IF (NBAMOR.GT.NBMODE) THEN
            CALL UTDEBM('F',NOMCMD,'TROP D''AMORTISSEMENTS MODAUX')
            CALL UTIMPI('L','   NOMBRE D''AMORTISSEMENTS : ',1,NBAMOR)
            CALL UTIMPI('L','   NOMBRE DE MODES : ',1,NBMODE)
            CALL UTFINM( )
         ENDIF
         IF (NBAMOR.LT.NBMODE) THEN
            CALL WKVECT('&&IREDMI.AMORTISSEMEN2','V V R',NBMODE,JAMO2)
            DO 40 IAM = 1,NBAMOR
               ZR(JAMO2+IAM-1) = ZR(JAMOR+IAM-1)
 40         CONTINUE
            DO 42 IAM = NBAMOR+1,NBMODE
               ZR(JAMO2+IAM-1) = ZR(JAMOR+NBAMOR-1)
 42         CONTINUE
            NBAMOR = NBMODE
            JAMOR = JAMO2
         ENDIF
      ELSE
         DO 44 K = 1,NBMODE
            ZR(JAMOR+K-1) = ZR(LAMOR+(K-1)*(NBMODE+1))/
     +       (4.D0*PI*ZR(JFREQ+K-1)*ZR(JMASS+(K-1)*(NBMODE+1)))
 44      CONTINUE
      ENDIF
C
      CALL IREDM1 ( MASSE, NOMA, BASEMO, NBMODE, NBMODS, IVAL3,
     +              ZR(JMASS) , ZR(JRIGI) , ZR(JAMOR),
     +              ZR(JFREQ) , ZR(ISMASS), ZR(ISRIGI), ZR(ISAMOR),
     +              ZR(ICMASS), ZR(ICRIGI), ZR(ICAMOR)   )
C
      CALL JEDETC('V','&&IREDMI',1)
C
      CALL JEDEMA()
      END
