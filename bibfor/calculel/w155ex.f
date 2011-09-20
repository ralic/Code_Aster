      SUBROUTINE W155EX(NOMRES,RESU,NBORDR,LIORDR)
C ----------------------------------------------------------------------
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
C     COMMANDE :  POST_CHAMP /EXTR_XXXX
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*8 NOMRES,RESU
      INTEGER NBORDR,LIORDR(NBORDR)
C   ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER IFM,NIV,NUCOU,NUFIB,NANGL
      INTEGER IRET,I,NUORDR,IBID,N1
      CHARACTER*8 MODELE,CARELE,MATE
      CHARACTER*8 MODEAV
      CHARACTER*3 NICOU
      CHARACTER*16 MOTFAC,NOMSYM
      CHARACTER*19 CHIN,CHEXTR,EXCIT,LIGREL,RESU19
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      RESU19=RESU



C     -- 1. : QUEL MOTFAC ?
C     -------------------------
      CALL GETFAC('EXTR_COQUE',N1)
      IF (N1.EQ.1) THEN
        MOTFAC='EXTR_COQUE'
      ELSE
        CALL GETFAC('EXTR_TUYAU',N1)
        IF (N1.EQ.1) THEN
          MOTFAC='EXTR_TUYAU'
        ELSE
          CALL GETFAC('EXTR_PMF',N1)
          IF (N1.EQ.1) THEN
            MOTFAC='EXTR_PMF'
          ELSE
C           -- IL N'Y A RIEN A FAIRE :
            GOTO 20

          ENDIF
        ENDIF
      ENDIF


C     -- 2.  : NOMSYM, NUCOU, NICOU, NANGL, NUFIB
C     --------------------------------------------------
      CALL GETVTX(MOTFAC,'NOM_CHAM',1,1,1,NOMSYM,IBID)
      IF (MOTFAC.EQ.'EXTR_COQUE' .OR. MOTFAC.EQ.'EXTR_TUYAU') THEN
        CALL GETVIS(MOTFAC,'NUME_COUCHE',1,1,1,NUCOU,IBID)
        CALL GETVTX(MOTFAC,'NIVE_COUCHE',1,1,1,NICOU,IBID)
        IF (MOTFAC.EQ.'EXTR_TUYAU') THEN
          CALL GETVIS(MOTFAC,'ANGLE',1,1,1,NANGL,IBID)
        ENDIF
      ELSEIF (MOTFAC.EQ.'EXTR_PMF') THEN
        CALL GETVIS(MOTFAC,'NUME_FIBRE',1,1,1,NUFIB,IBID)
      ENDIF


C     -- 3. : BOUCLE SUR LES CHAMPS
C     --------------------------------------------------
      MODEAV=' '
      DO 10,I=1,NBORDR
        NUORDR=LIORDR(I)
        CALL RSEXCH(RESU19,NOMSYM,NUORDR,CHIN,IRET)
        IF (IRET.EQ.0) THEN

C         -- 3.1 : MODELE, CARELE, LIGREL :
          CALL RSLESD(RESU,NUORDR,MODELE,MATE,CARELE,EXCIT,IBID)
          IF (MODELE.NE.MODEAV) THEN
            CALL EXLIMA(' ',1,'G',MODELE,LIGREL)
            MODEAV=MODELE
          ENDIF

          CALL RSEXCH(NOMRES,NOMSYM,NUORDR,CHEXTR,IRET)
          CALL ASSERT(IRET.EQ.100)
          CALL W155CH(CHIN,CARELE,LIGREL,CHEXTR,MOTFAC,NUCOU,NICOU,
     &                NANGL,NUFIB)
          CALL RSNOCH(NOMRES,NOMSYM,NUORDR,' ')
        ENDIF
   10 CONTINUE


   20 CONTINUE
      CALL JEDEMA()
      END
