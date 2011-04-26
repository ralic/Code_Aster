      SUBROUTINE I3CRDM(DESCM)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*24 DESCM
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     CREATION DU POINTEUR SUR LES DESCRIPTEURS DE MAILLES
C     ------------------------------------------------------------------
C IN  DESCM  : K : NOM OJB DES POINTEURS SUR LES DESCRIPTEURS DE MAILLES
C     ------------------------------------------------------------------
C     REMARQUE : MAILLES DECRITES = PENTA, HEXA, TETRA
C     ------------------------------------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8,KBID
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C
      INTEGER ADESCM,ATETRA,AHEXA,APENTA
C
C======================================================================
C
      CALL JEMARQ()
      CALL WKVECT(DESCM,'V V I',3,ADESCM)
      CALL JECREO('&&I3DESCM.DESC.TETRA','V V I')
      CALL JEECRA('&&I3DESCM.DESC.TETRA','LONMAX',38,KBID)
      CALL JEECRA('&&I3DESCM.DESC.TETRA','LONUTI',38,KBID)
      CALL JEVEUT('&&I3DESCM.DESC.TETRA','E',ATETRA)
      CALL JECREO('&&I3DESCM.DESC.PENTA','V V I')
      CALL JEECRA('&&I3DESCM.DESC.PENTA','LONMAX',38,KBID)
      CALL JEECRA('&&I3DESCM.DESC.PENTA','LONUTI',38,KBID)
      CALL JEVEUT('&&I3DESCM.DESC.PENTA','E',APENTA)
      CALL JECREO('&&I3DESCM.DESC.HEXA ','V V I')
      CALL JEECRA('&&I3DESCM.DESC.HEXA ','LONMAX',38,KBID)
      CALL JEECRA('&&I3DESCM.DESC.HEXA ','LONUTI',38,KBID)
      CALL JEVEUT('&&I3DESCM.DESC.HEXA ','E',AHEXA )
      ZI(ADESCM + 1-1)  = ATETRA
      ZI(ADESCM + 2-1)  = APENTA
      ZI(ADESCM + 3-1)  = AHEXA
      ZI(ATETRA + 1 -1) = 4
      ZI(ATETRA + 2 -1) = 4
      ZI(ATETRA + 3 -1) = 3
      ZI(ATETRA + 4 -1) = 3
      ZI(ATETRA + 5 -1) = 3
      ZI(ATETRA + 6 -1) = 3
      ZI(ATETRA + 7 -1) = 0
      ZI(ATETRA + 8 -1) = 0
      ZI(ATETRA + 9 -1) = 1
      ZI(ATETRA + 10-1) = 1
      ZI(ATETRA + 11-1) = 1
      ZI(ATETRA + 12-1) = 2
      ZI(ATETRA + 13-1) = 0
      ZI(ATETRA + 14-1) = 0
      ZI(ATETRA + 15-1) = 3
      ZI(ATETRA + 16-1) = 4
      ZI(ATETRA + 17-1) = 2
      ZI(ATETRA + 18-1) = 3
      ZI(ATETRA + 19-1) = 0
      ZI(ATETRA + 20-1) = 0
      ZI(ATETRA + 21-1) = 2
      ZI(ATETRA + 22-1) = 3
      ZI(ATETRA + 23-1) = 4
      ZI(ATETRA + 24-1) = 4
      ZI(ATETRA + 25-1) = 0
      ZI(ATETRA + 26-1) = 0
      ZI(ATETRA + 27-1) = 0
      ZI(ATETRA + 28-1) = 0
      ZI(ATETRA + 29-1) = 0
      ZI(ATETRA + 30-1) = 0
      ZI(ATETRA + 31-1) = 0
      ZI(ATETRA + 32-1) = 0
      ZI(ATETRA + 33-1) = 4
      ZI(ATETRA + 34-1) = 2
      ZI(ATETRA + 35-1) = 3
      ZI(ATETRA + 36-1) = 1
      ZI(ATETRA + 37-1) = 0
      ZI(ATETRA + 38-1) = 0
      ZI(APENTA + 1 -1) = 5
      ZI(APENTA + 2 -1) = 6
      ZI(APENTA + 3 -1) = 3
      ZI(APENTA + 4 -1) = 4
      ZI(APENTA + 5 -1) = 4
      ZI(APENTA + 6 -1) = 3
      ZI(APENTA + 7 -1) = 4
      ZI(APENTA + 8 -1) = 0
      ZI(APENTA + 9 -1) = 1
      ZI(APENTA + 10-1) = 1
      ZI(APENTA + 11-1) = 1
      ZI(APENTA + 12-1) = 4
      ZI(APENTA + 13-1) = 2
      ZI(APENTA + 14-1) = 0
      ZI(APENTA + 15-1) = 3
      ZI(APENTA + 16-1) = 4
      ZI(APENTA + 17-1) = 2
      ZI(APENTA + 18-1) = 5
      ZI(APENTA + 19-1) = 3
      ZI(APENTA + 20-1) = 0
      ZI(APENTA + 21-1) = 2
      ZI(APENTA + 22-1) = 6
      ZI(APENTA + 23-1) = 5
      ZI(APENTA + 24-1) = 6
      ZI(APENTA + 25-1) = 6
      ZI(APENTA + 26-1) = 0
      ZI(APENTA + 27-1) = 0
      ZI(APENTA + 28-1) = 3
      ZI(APENTA + 29-1) = 4
      ZI(APENTA + 30-1) = 0
      ZI(APENTA + 31-1) = 5
      ZI(APENTA + 32-1) = 0
      ZI(APENTA + 33-1) = 4
      ZI(APENTA + 34-1) = 2
      ZI(APENTA + 35-1) = 3
      ZI(APENTA + 36-1) = 1
      ZI(APENTA + 37-1) = 1
      ZI(APENTA + 38-1) = 0
      ZI(AHEXA  + 1 -1) = 6
      ZI(AHEXA  + 2 -1) = 8
      ZI(AHEXA  + 3 -1) = 4
      ZI(AHEXA  + 4 -1) = 4
      ZI(AHEXA  + 5 -1) = 4
      ZI(AHEXA  + 6 -1) = 4
      ZI(AHEXA  + 7 -1) = 4
      ZI(AHEXA  + 8 -1) = 4
      ZI(AHEXA  + 9 -1) = 1
      ZI(AHEXA  + 10-1) = 1
      ZI(AHEXA  + 11-1) = 1
      ZI(AHEXA  + 12-1) = 5
      ZI(AHEXA  + 13-1) = 2
      ZI(AHEXA  + 14-1) = 3
      ZI(AHEXA  + 15-1) = 4
      ZI(AHEXA  + 16-1) = 5
      ZI(AHEXA  + 17-1) = 2
      ZI(AHEXA  + 18-1) = 6
      ZI(AHEXA  + 19-1) = 3
      ZI(AHEXA  + 20-1) = 4
      ZI(AHEXA  + 21-1) = 3
      ZI(AHEXA  + 22-1) = 8
      ZI(AHEXA  + 23-1) = 6
      ZI(AHEXA  + 24-1) = 7
      ZI(AHEXA  + 25-1) = 7
      ZI(AHEXA  + 26-1) = 8
      ZI(AHEXA  + 27-1) = 2
      ZI(AHEXA  + 28-1) = 4
      ZI(AHEXA  + 29-1) = 5
      ZI(AHEXA  + 30-1) = 8
      ZI(AHEXA  + 31-1) = 6
      ZI(AHEXA  + 32-1) = 7
      ZI(AHEXA  + 33-1) = 7
      ZI(AHEXA  + 34-1) = 7
      ZI(AHEXA  + 35-1) = 7
      ZI(AHEXA  + 36-1) = 3
      ZI(AHEXA  + 37-1) = 8
      ZI(AHEXA  + 38-1) = 5
      CALL JEDEMA()
      END
