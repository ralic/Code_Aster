      SUBROUTINE W155CE(NOMRES,RESU,NBORDR,LIORDR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/10/2011   AUTEUR DELMAS J.DELMAS 
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
C     COMMANDE :  POST_CHAMP / COQU_EXCENT
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
      INTEGER IFM,NIV
      INTEGER IRET,I,NUORDR,IBID,NOCC,IOCC
      CHARACTER*8 MODELE,CARELE,MATE,MPLAN
      CHARACTER*8 MODEAV,LPAIN(2),LPAOUT(1)
      CHARACTER*4 TSCA
      CHARACTER*16 MOTFAC,NOMSYM
      CHARACTER*19 CHIN,CHEXTR,LIGREL,RESU19,LCHIN(2),LCHOUT(1),EXCIT
      INTEGER IARG
      LOGICAL LREEL
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      RESU19=RESU



C     -- 1. : Y-A-T-IL QUELQUE CHOSE A FAIRE ?
C     ----------------------------------------
      CALL GETFAC('COQU_EXCENT',NOCC)
      IF (NOCC.EQ.0)GOTO 30
      CALL ASSERT(NOCC.LT.10)


      DO 20,IOCC=1,NOCC

C     -- 2.  : NOMSYM, MPLAN :
C     --------------------------------------------------
        MOTFAC='COQU_EXCENT'
        CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC,IARG,1,NOMSYM,IBID)
        CALL ASSERT(NOMSYM.EQ.'EFGE_ELNO')
        CALL GETVTX(MOTFAC,'MODI_PLAN',IOCC,IARG,1,MPLAN,IBID)
        CALL ASSERT(MPLAN.EQ.'OUI')


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

            CALL JELIRA(CHIN//'.CELV','TYPE',IBID,TSCA)
            IF (TSCA.EQ.'R') THEN
              LREEL=.TRUE.
            ELSEIF (TSCA.EQ.'C') THEN
              LREEL=.FALSE.
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF

            IF (LREEL) THEN
              LPAIN(1)='PEFFONR'
              LPAOUT(1)='PEFFOER'
            ELSE
              LPAIN(1)='PEFFONC'
              LPAOUT(1)='PEFFOEC'
            ENDIF
            LCHIN(1)=CHIN
            LCHOUT(1)=CHEXTR

            LPAIN(2)='PCACOQU'
            LCHIN(2)=CARELE//'.CARCOQUE'

            CALL CALCUL('F','EFGE_ELNO_EXCENT',LIGREL,2,LCHIN,LPAIN,1,
     &                  LCHOUT,LPAOUT,'G','OUI')

            CALL RSNOCH(NOMRES,NOMSYM,NUORDR,' ')
          ENDIF
   10   CONTINUE
   20 CONTINUE


   30 CONTINUE
      CALL JEDEMA()
      END
