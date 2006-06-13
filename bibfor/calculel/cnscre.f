      SUBROUTINE CNSCRE(MAZ,NOMGDZ,NCMP,LICMP,BASEZ,CNSZ)
C RESPONSABLE VABHHTS J.PELLET
C A_UTIL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/01/2000   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT NONE
      CHARACTER*(*) MAZ,NOMGDZ,CNSZ,BASEZ
      INTEGER NCMP
      CHARACTER*(*) LICMP(NCMP)
C ------------------------------------------------------------------
C BUT : CREER UN CHAM_NO_S
C ------------------------------------------------------------------
C     ARGUMENTS:
C MAZ     IN/JXIN  K8  : SD MAILLAGE DE CNSZ
C NOMGDZ  IN       K8  : NOM DE LA GRANDEUR DE CNSZ
C NCMP    IN       I   : NOMBRE DE CMPS VOULUES DANS CNSZ
C LICMP   IN       L_K8: NOMS DES CMPS VOULUES DANS CNSZ
C BASEZ   IN       K1  : BASE DE CREATION POUR CNSZ : G/V/L
C CNSZ    IN/JXOUT K19 : SD CHAM_NO_S A CREER
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C     ------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*1 KBID,BASE
      CHARACTER*3 TSCA
      CHARACTER*8 MA,NOMGD
      CHARACTER*19 CNS
      INTEGER IBID,NBNO,JCNSK,JCNSD
      INTEGER JCNSC,K,JCNSL,JCNSV,IRET
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CNS = CNSZ
      BASE = BASEZ
      NOMGD = NOMGDZ
      MA = MAZ

      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNO,KBID,IBID)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)


C     -- SI CNS EXISTE DEJA, ON LE DETRUIT :
      CALL DETRSD('CHAM_NO_S',CNS)

C------------------------------------------------------------------
C     1- QUELQUES VERIFS :
C     ------------------------
      IF (NCMP.EQ.0) CALL UTMESS('F','CNSCRE','NCMP DOIT ETRE >0')
      CALL VERIGD(NOMGD,LICMP,NCMP,IRET)
      IF (IRET.GT.0) CALL UTMESS('F','CNSCRE',
     &                           'ARRET SUITE A L''ALARME PRECEDENTE.')


C------------------------------------------------------------------
C     2- CREATION DE CNS.CNSK:
C     ------------------------
      CALL WKVECT(CNS//'.CNSK',BASE//' V K8',2,JCNSK)
      ZK8(JCNSK-1+1) = MA
      ZK8(JCNSK-1+2) = NOMGD

C------------------------------------------------------------------
C     3- CREATION DE CNS.CNSD:
C     ------------------------
      CALL WKVECT(CNS//'.CNSD',BASE//' V I',2,JCNSD)
      ZI(JCNSD-1+1) = NBNO
      ZI(JCNSD-1+2) = NCMP

C------------------------------------------------------------------
C     4- CREATION DE CNS.CNSC:
C     ------------------------
      CALL WKVECT(CNS//'.CNSC',BASE//' V K8',NCMP,JCNSC)
      DO 10,K = 1,NCMP
        ZK8(JCNSC-1+K) = LICMP(K)
   10 CONTINUE

C------------------------------------------------------------------
C     5- CREATION DE CNS.CNSL:
C     ------------------------
      CALL WKVECT(CNS//'.CNSL',BASE//' V L',NBNO*NCMP,JCNSL)
      CALL JEUNDF(CNS//'.CNSL')

C------------------------------------------------------------------
C     6- CREATION DE CNS.CNSV:
C     ------------------------
      CALL WKVECT(CNS//'.CNSV',BASE//' V '//TSCA,NBNO*NCMP,JCNSV)
      CALL JEUNDF(CNS//'.CNSV')

      CALL JEDEMA()
      END
