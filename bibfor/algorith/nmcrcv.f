      SUBROUTINE NMCRCV(SDCRIT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/01/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*19 SDCRIT
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (STRUCTURES DE DONNES)
C
C CREATION SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
C
C ----------------------------------------------------------------------
C
C
C OUT SDCRIT : SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
C                (1) NOMBRE ITERATIONS NEWTON
C                (2) NOMBRE ITERATIONS RECHERCHE LINEAIRE
C                (3) RESI_GLOB_RELA
C                (4) RESI_GLOB_MAXI
C                (5) PARAMETRE DE PILOTAGE ETA
C                (6) CHARGEMENT EXTERIEUR
C                (9) RESI_COMP_RELA
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      NRESI
      PARAMETER    (NRESI=4)
C
      INTEGER JCRR,JCRK,JCRP
      REAL*8  R8MAEM
      INTEGER IRESI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- CREATION
C
      CALL WKVECT(SDCRIT(1:19)//'.CRTR','V V R8' ,9      ,JCRR)
      CALL WKVECT(SDCRIT(1:19)//'.CRDE','V V K16',9      ,JCRK)
      ZK16(JCRK+1-1) = 'ITER_GLOB'
      ZK16(JCRK+2-1) = 'ITER_LINE'
      ZK16(JCRK+3-1) = 'RESI_GLOB_RELA'
      ZK16(JCRK+4-1) = 'RESI_GLOB'
      ZK16(JCRK+5-1) = 'ETA_PILOTAGE'
      ZK16(JCRK+6-1) = 'CHAR_MINI'
      ZK16(JCRK+7-1) = 'RESI_GLOB_MOINS'
      ZK16(JCRK+8-1) = 'RESI_REFE'
      ZK16(JCRK+9-1) = 'RESI_COMP'
C     
C --- SD POUR TYPE DE CONVERGENCE EN PLATEAU
C  
      CALL WKVECT(SDCRIT(1:19)//'.PLAT','V V R8' ,4*NRESI,JCRP) 
      DO 15 IRESI = 1,NRESI
        ZR(JCRP+4*(IRESI-1)+1-1) = R8MAEM()
        ZR(JCRP+4*(IRESI-1)+2-1) = -R8MAEM() 
        ZR(JCRP+4*(IRESI-1)+3-1) = 0.D0
        ZR(JCRP+4*(IRESI-1)+4-1) = 0.D0
 15   CONTINUE    
C
      CALL JEDEMA()

      END
