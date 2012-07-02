      SUBROUTINE NTOPTC(TEMPER,MODELE,MATE,CARELE,CHARGE,INFOCH,LISOPT,
     &                  NOPT,EPSI)
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE                            DURAND C.DURAND

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NOPT
      REAL*8 EPSI
      CHARACTER*24 TEMPER,MODELE,MATE,CARELE,CHARGE,INFOCH
      CHARACTER*24 LISOPT

C ----------------------------------------------------------------------

C COMMANDE THER_NON_LINE  : CALCUL DES OPTIONS THERMIQUES.




      INTEGER IBID,I,IRET,IRET2,IORD,JOPT,JINF,JCHA,JINST,LORDR
      INTEGER IFM,IUNIFI,NH,NBORDR,NBTROU,NUMORD
      REAL*8       RBID,ALPHA
      REAL*8       RUNDF,R8VIDE
      COMPLEX*16   CBID,CALPHA
      CHARACTER*1  BASE
      CHARACTER*8  K8BID,CRIT
      CHARACTER*16 TYSD,OPTION,K16BID,NOMCMD
      CHARACTER*19 LIGREL
      CHARACTER*24 CHAMGD,CHGEOM,CHCARA(18),CHTEMP,CHTREF,CHTIME,CHNUMC
      CHARACTER*24 VALK(2)
      CHARACTER*24 CHHARM,CHSIG,CHEPS,CHFREQ,CHMASS,CHDYNR,SOP,CHELEM
      CHARACTER*24 K24B

C ----------------------------------------------------------------------

      CALL JEMARQ()
      RUNDF=R8VIDE()
      IFM = IUNIFI('MESSAGE')

      BASE = 'G'

      IF (NOPT.EQ.0) GO TO 30

C --- CALCUL DES OPTIONS

      CALL JEVEUO(LISOPT,'L',JOPT)
      LIGREL = MODELE(1:8)//'.MODELE'
      CALL GETTCO(TEMPER,TYSD)
      NH = 0
      CHDYNR = ' '
      CHFREQ = ' '
      CHMASS = ' '
      CHTREF = ' '
      CHAMGD = ' '
      CHNUMC = ' '
      CHSIG = ' '
      CHEPS = ' '
      SOP = ' '
      K24B = ' '
      ALPHA = 1.D0
      CALPHA = (1.D0,1.D0)


      DO 20 I = 1,NOPT
        OPTION = ZK16(JOPT+I-1)
        CALL JEVEUO(INFOCH,'L',JINF)
        CALL JEVEUO(CHARGE,'L',JCHA)
        CALL MECHAM(OPTION,MODELE,ZI(JINF),ZK24(JCHA),CARELE,NH,CHGEOM,
     &              CHCARA,CHHARM,IRET)
        IF (IRET.NE.0) GO TO 30
        CALL RSORAC(TEMPER,'LONUTI',IBID,RBID,K8BID,CBID,EPSI,CRIT,
     &              NBORDR,1,NBTROU)
        CALL WKVECT('&&NTOPTC.NUME_ORDR','V V I',NBORDR,LORDR)
        CALL RSORAC(TEMPER,'TOUT_ORDRE',IBID,RBID,K8BID,CBID,EPSI,CRIT,
     &              ZI(LORDR),NBORDR,NBTROU)

        DO 10 IORD = 1,NBORDR
          NUMORD = ZI(LORDR+IORD-1)
          CALL RSEXCH(TEMPER,'TEMP',NUMORD,CHTEMP,IRET)
          IF (IRET.GT.0) GO TO 20
          CALL RSEXCH(TEMPER,OPTION,NUMORD,CHELEM,IRET2)
          IF (IRET2.GT.100) THEN
            CALL GETRES(K8BID,K16BID,NOMCMD)
            VALK(1) = TYSD
            VALK(2) = OPTION
            CALL U2MESK('F','CALCULEL3_27', 2 ,VALK)
          END IF
          CALL RSADPA(TEMPER,'L',1,'INST',NUMORD,0,JINST,K8BID)
          CALL MECHTI(CHGEOM(1:8),ZR(JINST),RUNDF,RUNDF,CHTIME)
          IBID = 0
          CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,CHTEMP,
     &                CHTREF,CHTIME,CHNUMC,CHHARM,CHSIG,CHEPS,CHFREQ,
     &                CHMASS,K24B,K24B,K24B,ALPHA,CALPHA,CHDYNR,SOP,
     &                CHELEM,K24B,LIGREL,BASE,K24B,K24B,K24B,K24B,K24B,
     &                  K24B, K8BID, IBID, K24B, IRET )
          CALL RSNOCH(TEMPER,OPTION,NUMORD)
          WRITE (IFM,1020) OPTION,NUMORD,ZR(JINST)
   10   CONTINUE

        CALL JEDETR('&&NTOPTC.NUME_ORDR')


   20 CONTINUE
 1020 FORMAT (1P,3X,'CHAMP    STOCKE   :',1X,A14,' NUME_ORDRE:',I8,
     &       ' INSTANT:',D12.5)

C-----------------------------------------------------------------------
   30 CONTINUE
      CALL JEDEMA()
      END
