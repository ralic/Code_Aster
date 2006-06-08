      SUBROUTINE ACEVRM(NBOCC,NOMA,NOEMAX,NOEMAF,IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/11/2003   AUTEUR ACBHHCD G.DEVESA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      NBOCC,NOEMAX,IER
      CHARACTER*8  NOMA
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES DIMENSIONS POUR LES RAIDEURS REPARTIES
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C IN  : NOMA   : NOM DU MAILLAGE
C OUT : NOEMAX : NOMBRE TOTAL DE NOEUDS MAX
C ----------------------------------------------------------------------
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
      CHARACTER*24 MAGRMA
      CHARACTER*8  NOGP, NOGL
      CHARACTER*8  K8B
      CALL JEMARQ()
      MAGRMA = NOMA//'.GROUPEMA'
      NOEMAX = 0
      NOEMAF = 0
C --- BOUCLE SUR LES OCCURENCES DE DISCRET
      DO 10 IOC = 1 , NBOCC
         CALL GETVID('RIGI_MISS_3D','GROUP_MA_POI1',IOC,1,1,NOGP,NGP)
         CALL GETVID('RIGI_MISS_3D','GROUP_MA_SEG2',IOC,1,1,NOGL,NGL)
C
         IF (NGP.NE.0) THEN
           CALL JELIRA(JEXNOM(MAGRMA,NOGP),'LONMAX',NMA,K8B)
           CALL JEVEUO(JEXNOM(MAGRMA,NOGP),'L',LDGM)
           DO 11 IN = 0,NMA-1
            NOEMAF = MAX(NOEMAF,ZI(LDGM+IN))
 11        CONTINUE
           NOEMAX = NOEMAX + NMA
         ENDIF
         IF (NGL.NE.0) THEN
           CALL JELIRA(JEXNOM(MAGRMA,NOGL),'LONMAX',NMA,K8B)
           CALL JEVEUO(JEXNOM(MAGRMA,NOGL),'L',LDGM)
           DO 12 IN = 0,NMA-1
            NOEMAF = MAX(NOEMAF,ZI(LDGM+IN))
 12        CONTINUE
           NOEMAX = NOEMAX + NMA
         ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END
