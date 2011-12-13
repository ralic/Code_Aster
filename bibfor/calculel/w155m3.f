      SUBROUTINE W155M3(NUMA,JCE2D,JCE2L,JCE2V,ISP,NUCOU,NUSEC,NUFIB,
     &                  POSIC,POSIS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 19/09/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C ======================================================================
      IMPLICIT NONE
      INTEGER NUMA,NUCOU,NUSEC,NUFIB,POSIC,POSIS,ISP
      INTEGER JCE2L,JCE2D,JCE2V
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER NBCOU,NBSEC,NBFIB,ISEC,ICOU
      INTEGER IAD1,IAD2,IAD3,IAD4,IAD5
      CHARACTER*8 TYPMA

C ----------------------------------------------------------------------
C BUT : DETERMINER NUCOU, NUSEC, NUFIB, POSIC ET POSIS
C       A PARTIR DU NUMERO DE SOUS-POINT ISP
C ----------------------------------------------------------------------


C     -- ON DETERMINE SI LA MAILLE EST DE TYPE COQUE, PMF, GRILLE
C        OU TUYAU :
C       -- CMP1 = COQ_NCOU
      CALL CESEXI('C',JCE2D,JCE2L,NUMA,1,1,1,IAD1)
C       -- CMP2 = TUY_NCOU
      CALL CESEXI('C',JCE2D,JCE2L,NUMA,1,1,2,IAD2)
C       -- CMP3 = TUY_NSEC
      CALL CESEXI('C',JCE2D,JCE2L,NUMA,1,1,3,IAD3)
C       -- CMP4 = NBFIBR
      CALL CESEXI('C',JCE2D,JCE2L,NUMA,1,1,4,IAD4)
C       -- CMP5 = GRI_NCOU
      CALL CESEXI('C',JCE2D,JCE2L,NUMA,1,1,5,IAD5)


      IF (IAD5.GT.0) THEN
        TYPMA='GRI'
        NBCOU=ZI(JCE2V-1+IAD5)
      ELSEIF (IAD4.GT.0) THEN
        TYPMA='PMF'
        NBFIB=ZI(JCE2V-1+IAD4)
      ELSEIF (IAD2.GT.0) THEN
        TYPMA='TUY'
        CALL ASSERT(IAD3.GT.0)
        NBCOU=ZI(JCE2V-1+IAD2)
        NBSEC=ZI(JCE2V-1+IAD3)
      ELSEIF (IAD1.GT.0) THEN
        TYPMA='COQ'
        NBCOU=ZI(JCE2V-1+IAD1)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      NUCOU=-999
      NUSEC=-999
      NUFIB=-999
      POSIC=-999
      POSIS=-999


      IF (TYPMA.EQ.'GRI') THEN
C       -------------------------
        CALL ASSERT(ISP.LE.NBCOU)
        NUCOU=ISP

      ELSEIF (TYPMA.EQ.'PMF') THEN
C       -------------------------
        CALL ASSERT(ISP.LE.NBFIB)
        NUFIB=ISP

      ELSEIF (TYPMA.EQ.'COQ') THEN
C       -------------------------
        CALL ASSERT(ISP.LE.NBCOU*3)
        NUCOU=(ISP+2)/3
        POSIC=MOD(ISP+1,3)-1

      ELSEIF (TYPMA.EQ.'TUY') THEN
C       -------------------------
        CALL ASSERT(ISP.LE.(2*NBCOU+1)*(2*NBSEC+1))
        ICOU=(ISP-1)/(2*NBSEC+1)+1
        ISEC=ISP-(ICOU-1)*(2*NBSEC+1)
        IF (ICOU.EQ.1) THEN
          NUCOU=1
          POSIC=-1
        ELSE
          NUCOU=ICOU/2
          POSIC=ICOU-2*NUCOU
        ENDIF
        IF (ISEC.EQ.1) THEN
          NUSEC=1
          POSIS=-1
        ELSE
          NUSEC=ISEC/2
          POSIS=ISEC-2*NUSEC
        ENDIF

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

   10 CONTINUE
      END
