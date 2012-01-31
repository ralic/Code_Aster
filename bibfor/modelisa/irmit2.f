      SUBROUTINE IRMIT2 (NBMODE,IFMIS,FREQ,TABRIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 31/01/2012   AUTEUR IDOUX L.IDOUX 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C      ---- DEBUT DES COMMUNS JEVEUX ----------------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32   JEXNUM
C      ---- FIN DES COMMUNS JEVEUX ------------------------------------
C
      CHARACTER*24 TABRIG, TABFRQ, TABRI2
      REAL*8       A(3), A2(3), NINS2
C      INTEGER*8    LONG1,LONG2,LONG3
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C
      TABRI2 = '&&IRMIT2.RIG2'
      TABFRQ = '&&IRMIT2.FREQ'
      CALL WKVECT(TABRI2,'V V R',NBMODE,JRI2)
      REWIND IFMIS
C
C   Lecture d'entiers INTEGER*8 en binaire venant de MISS3D
C   On convertit ensuite en INTEGER (*4 sur machine 32 bits, sinon *8).
C   Les reels ne posent pas de probleme : ce sont toujours des REAL*8
C
      READ(IFMIS) NINS2
      NFREQ=INT(NINS2)
      CALL WKVECT(TABFRQ,'V V R',NFREQ,JFRQ)
C      NBMODE=LONG2
C      N1=LONG3
      IC=1
      CALL JEVEUO(TABRIG,'E',JRIG)
      READ(IFMIS) (ZR(JFRQ+IFR-1),IFR=1,NFREQ)
      DO 3 I = 1, NFREQ
        A(1) = ZR(JFRQ+I-1)
        IF (FREQ.LE.(A(1) + R8PREM( ))) THEN
          IFREQ = I
          IF (I.GT.1.AND.FREQ.LT.(A(1) - R8PREM( ))) THEN
            IFREQ = IFREQ-1
          ENDIF
          IF (FREQ.LE.R8PREM( )) IC = 2
          IF (I.EQ.1.AND.NFREQ.EQ.1) IC = 0
          IF (I.EQ.NFREQ.AND.FREQ.GE.(A(1) - R8PREM( ))) THEN
            IC = 0
            IFREQ = NFREQ
          ENDIF
          GOTO 7
        ENDIF
    3 CONTINUE
      IFREQ = NFREQ
      IC = 0
    7 CONTINUE
      DO 5 I = 1, IFREQ-1
        READ(IFMIS) A(1)
    5 CONTINUE
      READ(IFMIS) (ZR(JRIG+I1-1),I1=1,NBMODE)
      IF (IC.GE.1) THEN
        READ(IFMIS) (ZR(JRI2+I1-1),I1=1,NBMODE)        
        DO 8 I1 = 1, NBMODE
          ZR(JRIG+I1-1) =
     &    ZR(JRIG+I1-1) +
     &    (FREQ-ZR(JFRQ+IFREQ-1))/(ZR(JFRQ+IFREQ)-ZR(JFRQ+IFREQ-1)) *
     &    (ZR(JRI2+I1-1)-ZR(JRIG+I1-1))
    8   CONTINUE
      ENDIF

      CALL JEDETR(TABRI2)
      CALL JEDETR(TABFRQ)
C
      CALL JEDEMA()
      END
