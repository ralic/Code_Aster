      SUBROUTINE WRSTAT( FCHIER , IDEB , IFIN )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      FCHIER
      INTEGER            IDEB,IFIN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C     IMPRESSIONS DE STATISTIQUES STOCKEES DANS UN K80 (TEMPS, ... )
C     DANS LE FICHIER RESULTAT
C     ------------------------------------------------------------------
C IN  KSTAT : NOM DU K80 DANS LEQUEL SONT RANGEES LES INFORMATIONS
C IN  IDEB  : DEBUT D'IMPRESSION (SI IDEB > LONMAX PAS D'IMPRESSION)
C IN  IFIN  : FIN D'IMPRESSION (SI IFIN > LONUTI IFIN = LONUTI,
C                               SI IFIN = 0 IFIN = LONUTI)
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /GCUCC1/ KINFO , KRESU, KSTAT
      INTEGER      LONUTI, LONMAX, IFR
      CHARACTER*24 KINFO,KRESU,KSTAT
      CHARACTER*1 K1BID
C
      CALL JEMARQ()
      IFR = IUNIFI(FCHIER)
      IF(IFR .EQ.0) GOTO 9999
      CALL JEVEUO(KSTAT,'L',LSTAT)
      CALL JELIRA(KSTAT,'LONUTI',LONUTI,K1BID)
      CALL JELIRA(KSTAT,'LONMAX',LONMAX,K1BID)
      IF (IDEB .GT. LONMAX) GOTO 9999
      IF (IFIN .EQ. 0) IFIN = LONUTI
      IF (IFIN .GT. LONUTI) IFIN = LONUTI
      WRITE(IFR,'(1X,''======>'')')
      WRITE(IFR,'(1X,59(''*''))')
      WRITE(IFR,'(4(1X,''*'',1X,A),1X,''*'')')
     +     'COMMANDE        ',
     +     '      USER',
     +     '   SYSTEME',
     +     '     TOTAL'
      WRITE(IFR,'(1X,59(''*''))')
      DO 10 II = IDEB, IFIN
         WRITE(IFR,'(A)') ZK80(LSTAT-1+II)
 10   CONTINUE
 9999 CONTINUE
      CALL JEDEMA()
      END
