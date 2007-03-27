      SUBROUTINE VRCIN3(MODELE,NCHAR,LCHAR,TIME,CHTEMP,IRET)
      IMPLICIT NONE
      INTEGER NCHAR,IRET
      REAL*8 TIME
      CHARACTER*(*) MODELE,LCHAR(*)
      CHARACTER*19 CHTEMP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
C     BUT :
C   TROUVER DANS UNE LISTE DE CHARGES LE CHAMP DE TEMPERATURE
C   CORRESPONDANT A UN INSTANT
C     ------------------------------------------------------------------
C IN  : MODELE : MODELE
C IN  : NCHAR  : NOMBRE DE CHARGES
C IN  : LCHAR  : LISTE DES CHARGES
C IN  : TIME   : INSTANT DE CALCUL
C IN/JXOUT : CHTEMP : NOM DU CHAMP DE TEMPERATURE
C OUT : IRET : CODE RETOUR :
C              0 : CHTEMP EXISTE
C              1 : IL N'Y A PAS DE CHARGE THERMIQUE DANS LCHAR
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8 K8B,NOMO,TEMPE,REPK
      CHARACTER*16 TYSD
      CHARACTER*19 CH19
      INTEGER JTEMP,ICHA,IER,IBID,ICORET,NBCHAM,IERD

      CALL JEMARQ()


C     1- Y-A-T-IL UN CHARGEMENT THERMIQUE ?
C     -------------------------------------
      TEMPE = ' '
      DO 10 ICHA = 1,NCHAR
        CALL JEEXIN(LCHAR(ICHA) (1:8)//'.CHME.TEMPE.TEMP',IER)
        IF (IER.NE.0) THEN
          CALL JEVEUO(LCHAR(ICHA) (1:8)//'.CHME.TEMPE.TEMP','L',JTEMP)
          TEMPE = ZK8(JTEMP)
          GOTO 20

        ENDIF
   10 CONTINUE
   20 CONTINUE
      IF (TEMPE.EQ.' ') THEN
        IRET = 1
        GOTO 30
      ENDIF


C     2- RECHERCHE DU CHAMP DE TEMPERATURE :
C     ---------------------------------------
      CALL DETRSD('CHAMP_GD',CHTEMP)
      CALL GETTCO(TEMPE,TYSD)


      IF (TYSD(1:9).EQ.'EVOL_THER') THEN
C     ----------------------------
        CALL DISMOI('F','NB_CHAMP_UTI',TEMPE,'RESULTAT',NBCHAM,K8B,IERD)
        IF (NBCHAM.GT.0) THEN
          CALL RSINCH(TEMPE,'TEMP','INST',TIME,CHTEMP,'EXCLU','EXCLU',2,
     &                'V',ICORET)
          IRET = 0
        ELSE
          IRET = 1
        ENDIF

      ELSEIF (TYSD(1:7).EQ.'CHAM_NO') THEN
C     -------------------------------------
        CH19 = TEMPE
        CALL COPISD('CHAMP','V',CH19,CHTEMP)
        IRET = 0


      ELSEIF (TYSD(1:5).EQ.'CARTE') THEN
C     -------------------------------------
        CH19 = TEMPE
        CALL COPISD('CHAMP','V',CH19,CHTEMP)
        IRET = 0

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


   30 CONTINUE
      CALL JEDEMA()
      END
