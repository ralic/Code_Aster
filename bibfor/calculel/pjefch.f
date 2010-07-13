      SUBROUTINE PJEFCH(CORRES,CH1,CH2,PRFCHN,PROL0,LIGREL,BASE,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/07/2010   AUTEUR BERARD A.BERARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE
C-------------------------------------------------------------------
C     BUT : PROJETER UN CHAMP "CH1" SUIVANT "CORRES"
C           POUR CREER "CH2" SUR LA BASE "BASE"
C-------------------------------------------------------------------
C  IRET (OUT)  : = 0    : OK
C                = 1    : PB : ON N' A PAS PU PROJETER LE CHAMP
C                = 10   : ON NE SAIT PAS ENCORE FAIRE
C-------------------------------------------------------------------
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
      CHARACTER*19 CH1,CH2,CH0S,CH1S,CH2S,PRFCHN,LIGREL
      CHARACTER*16 OPTION,CORRES
      CHARACTER*8 NOMPAR
      CHARACTER*4 TYCH,TYCHV
      CHARACTER*1 BASE
      CHARACTER*(*) PROL0
      INTEGER IRET,IBID,JNOLI,JCELK,NNCP


      CH0S = '&&PJEFCH'//'.CH0S'
      CH1S = '&&PJEFCH'//'.CH1S'
      CH2S = '&&PJEFCH'//'.CH2S'
      IRET = 0

      CALL DISMOI('F','TYPE_CHAMP',CH1,'CHAMP',IBID,TYCH,IBID)

C     L'UTILISATEUR VEUT-IL DES CHAM_NO ?  TYCHV
      IF (CORRES.EQ.' ') THEN
        TYCHV = 'NOEU'

      ELSE
        CALL GETVTX(' ','TYPE_CHAM',1,1,1,TYCHV,IBID)
        IF (IBID.EQ.0) TYCHV = ' '
      ENDIF

      CALL ASSERT(TYCHV.EQ.' ' .OR. TYCHV.EQ.'NOEU')

C     1 : TRANSFORMATION DE CH1 EN CHAMP SIMPLE : CH1S
C     -------------------------------------------------
      IF (TYCH.EQ.'NOEU') THEN
        CALL CNOCNS(CH1,'V',CH1S)

      ELSEIF ((TYCH.EQ.'ELEM') .OR. (TYCH.EQ.'ELNO')) THEN
        IF (TYCHV.EQ.' ') THEN
          CALL CELCES(CH1,'V',CH1S)

        ELSEIF (TYCHV.EQ.'NOEU') THEN
          TYCH = 'NOEU'
          CALL CELCES(CH1,'V',CH0S)
          CALL CESCNS(CH0S,' ','V',CH1S)
          CALL DETRSD('CHAM_ELEM_S',CH0S)
        ENDIF

      ELSE
C          -- ON NE SAIT PAS ENCORE TRAITER LES CART ET ELGA:
        IRET = 10
        GOTO 10

      ENDIF


C     2 : PROJECTION DU CHAMP SIMPLE : CH1S -> CH2S
C     -------------------------------------------------
      IF (CORRES.EQ.' ') THEN
C CAS MODIFICATION STRUCTURALE : PROJECTION SUR MAILLAGE MESURE
        CALL CNSPRM(CH1S,'V',CH2S,IRET)

      ELSE
        IF (TYCH.EQ.'NOEU') THEN
          CALL CNSPRJ(CH1S,CORRES,'V',CH2S,IRET)

        ELSEIF ((TYCH.EQ.'ELEM') .OR. (TYCH.EQ.'ELNO')) THEN
          CALL CESPRJ(CH1S,CORRES,'V',CH2S,IRET)
        ENDIF
      ENDIF
      IF (IRET.GT.0) GOTO 10


C     3 : TRANSFORMATION DE CH2S EN CHAMP : CH2
      IF (TYCH.EQ.'NOEU') THEN
        CALL CNSCNO(CH2S,PRFCHN,PROL0,BASE,CH2,'A',IRET)
        CALL DETRSD('CHAM_NO_S',CH1S)
        CALL DETRSD('CHAM_NO_S',CH2S)

      ELSEIF ((TYCH.EQ.'ELEM') .OR. (TYCH.EQ.'ELNO')) THEN
        CALL JEVEUO(CH1//'.CELK','L',JCELK)
        OPTION = ZK24(JCELK-1+2)
        CALL CESCEL(CH2S,LIGREL,OPTION,' ',PROL0,NNCP,BASE,CH2,'A',IRET)
        CALL DETRSD('CHAM_ELEM_S',CH1S)
        CALL DETRSD('CHAM_ELEM_S',CH2S)
      ENDIF


   10 CONTINUE
      END
