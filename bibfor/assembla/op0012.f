      SUBROUTINE OP0012(IER)
C======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C                       OPERATEUR ASSE_MATRICE
C======================================================================
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*9 NOMPRO
      CHARACTER*8 NU,MATAS,MATPRO
      CHARACTER*16 TYPM,OPER
      CHARACTER*24 K24B
      CHARACTER*72 KBIDON
      INTEGER TYPE
      CHARACTER*1 NOMTYP, TYPMAT
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------
      CHARACTER*19 CH19
      CALL JEMARQ()
C
C
C
C---- ARGUMENT IMPR
      CALL INFMAJ
C
C---- RECUPERATION DES ARGUMENTS ET DU CONCEPT
      CALL GETRES(MATAS,TYPM,OPER)
      IF (TYPM(16:16).EQ.'R') TYPE = 1
      IF (TYPM(16:16).EQ.'C') TYPE = 2
C
C---- RECUPERATION DES MATRICES ELEMENTAIRES ---
      CALL GETVID(' ','MATR_ELEM',0,1,0,KBIDON,NBMAT)
      NBMAT = -NBMAT
C
C---- RECUPERATION DES CHARGES CINEMATIQUES ---
      CALL GETVID(' ','CHAR_CINE',0,1,0,KBIDON,NBCHC)
      NBCHC = -NBCHC
C
C
C---- MOT CLE : NUME_DDL
      CALL GETVID(' ','NUME_DDL',0,1,1,NU,IBID)
      CALL JEEXIN(NU(1:8)//'      .SDIS.REFE',IRET)
      IF (IRET.GT.0) NOMPRO = 'DIAG     '
      CALL JEEXIN(NU(1:8)//'      .SMOS.REFE',IRET)
      IF (IRET.GT.0) NOMPRO = 'MORSE    '
      CALL JEEXIN(NU(1:8)//'      .SLCS.REFE',IRET)
      IF (IRET.GT.0) NOMPRO = 'LIGN_CIEL'
C
C
C---- MOTS CLE : MATR_ELEM ET LICOEF :
      CALL WKVECT(MATAS//'.LI2MATEL','V V K8',NBMAT,ILIMA2)
      CALL GETVID(' ','MATR_ELEM',0,1,NBMAT,ZK8(ILIMA2),L)
      CALL WKVECT(MATAS//'.LICOEF','V V R',NBMAT,ILICOE)
      DO 5 I = 1,NBMAT
          ZR(ILICOE-1+I) = 1.0D0
    5 CONTINUE
C
C
C------RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFNIV(IFM,NIV)
C      IFM = IUNIFI('MESSAGE')
C-------------------------------------------------------------------
C      IF (NIV.EQ.2) THEN
C          WRITE (IFM,*) ' ----                           ---- '
C          WRITE (IFM,*) ' ---- EXECUTION DE ASSE_MATRICE ---- '
C          WRITE (IFM,*) ' ----                           ---- '
C     END IF
C      IF (NIV.EQ.2) THEN
C          DO 10 I = 1,NBMAT
C          WRITE (IFM,*) ' --- ARGUMENT ',I,' = ',ZK8(ILIMA2+I-1)
C   10     CONTINUE
C      END IF
C
C
C---- ASSEMBLAGE PROPREMENT DIT
      NOMTYP = TYPMAT(NBMAT,ZK8(ILIMA2))
      IF (NOMPRO.EQ.'LIGN_CIEL') THEN
          IF (NOMTYP.EQ.'S') THEN
              CALL ASSMAT('G',MATAS,NBMAT,ZK8(ILIMA2),ZR(ILICOE),NU,
     +                    'ZERO',TYPE)
          ELSE
              CALL ASSMNS('G',MATAS,NBMAT,ZK8(ILIMA2),ZR(ILICOE),NU,
     +                    'ZERO',TYPE)
          ENDIF
      ELSE IF (NOMPRO.EQ.'MORSE') THEN
          IF (NOMTYP.EQ.'S') THEN
              CALL ASSMAM('G',MATAS,NBMAT,ZK8(ILIMA2),ZR(ILICOE),NU,
     +                    'ZERO',TYPE)
          ELSE
              CALL ASSMMN('G',MATAS,NBMAT,ZK8(ILIMA2),ZR(ILICOE),NU,
     +                    'ZERO',TYPE)
          ENDIF
      ELSE IF (NOMPRO.EQ.'DIAG') THEN
C        CALL ASSMAD('G',MATAS,NBMAT,ZK8(ILIMA2),ZR(ILICOE),NU,
C    +                                      'ZERO',TYPE)
      END IF
      CH19 = MATAS
      CALL JEDETR(CH19//'.LILI')
C
C---- PRISE EN COMPTE DES CHARGE CINEMATIQUES SI IL Y EN A
      IF (NBCHC.NE.0) THEN
        CALL WKVECT('OP0012.&&LCHARCINE','V V K8',NBCHC,ILCHCI)
        CALL GETVID(' ','CHAR_CINE',0,1,NBCHC,ZK8(ILCHCI),IBID)
        CALL ASSCHC('G',MATAS,NBCHC,ZK8(ILCHCI),NU,'ZERO')
      ENDIF
      CALL JEDEMA()
      END
