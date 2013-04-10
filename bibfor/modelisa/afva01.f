      SUBROUTINE  AFVA01(TYPSD,NOMSD,NOMSYM,LAUTR)
      IMPLICIT   NONE
      CHARACTER*16 TYPSD,NOMSD,NOMSYM
      LOGICAL LAUTR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C BUT : DIRE SI DANS LA SD NOMSD DE TYPE TYPSD=CHAMP/EVOL+NOMSYM
C       ON TROUVE DES COMPOSANTES AUTRES QUE 'TEMP' ET 'LAGR'
C ----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      INTEGER NCMP,NCMPMX,K,J1,JORDR,J,IRET,NBORDR,IBID
      CHARACTER*19 CH19,KBID,RES19
      CHARACTER*24 CORR1,CORR2,NOMCMP
      REAL*8 R8B
      COMPLEX*16 C16B
C ----------------------------------------------------------------------

      CALL JEMARQ()
      NOMCMP='&&AFVAUT.NOMCMP'
      CORR1='&&AFVAUT.CORR1'
      CORR2='&&AFVAUT.CORR2'


      IF (TYPSD.EQ.'CHAMP') THEN
C     -----------------------------
        CH19=NOMSD
        CALL CMPCHA(CH19,NOMCMP,CORR1,CORR2,NCMP,NCMPMX)
        CALL JEVEUO(NOMCMP,'L',J1)
        DO 1, K=1,NCMP
          IF (ZK8(J1-1+K).NE.'TEMP'.AND.ZK8(J1-1+K).NE.'LAGR') GOTO 7
1       CONTINUE
        GOTO 8


      ELSEIF (TYPSD.EQ.'EVOL') THEN
C     -----------------------------
        RES19=NOMSD
        CALL RSORAC(RES19,'LONUTI',IBID,R8B,KBID,C16B,R8B,
     &            KBID,NBORDR,1,IBID)
        CALL WKVECT('&&AFVA01.NUME_ORDRE','V V I',NBORDR,JORDR)
        CALL RSORAC(RES19,'TOUT_ORDRE',IBID,R8B,KBID,C16B,
     &            R8B,KBID,ZI(JORDR),NBORDR,IBID)

        DO 20 J=1,NBORDR
          CALL RSEXCH('F',RES19,NOMSYM,ZI(JORDR-1+J),CH19,IRET)
          IF (IRET.NE.0) GOTO 20

          CALL CMPCHA(CH19,NOMCMP,CORR1,CORR2,NCMP,NCMPMX)
          CALL JEVEUO(NOMCMP,'L',J1)
          DO 2, K=1,NCMP
            IF (ZK8(J1-1+K).NE.'TEMP'.AND.ZK8(J1-1+K).NE.'LAGR') GOTO 7
2         CONTINUE
          CALL JEDETR(NOMCMP)
          CALL JEDETR(CORR1)
          CALL JEDETR(CORR2)
 20     CONTINUE
        CALL JEDETR('&&AFVA01.NUME_ORDRE')
        LAUTR=.FALSE.
        GOTO 8


      ELSE
        WRITE(6,*) TYPSD,NOMSD,NOMSYM
        CALL ASSERT(.FALSE.)
      ENDIF

7     CONTINUE
      LAUTR=.TRUE.
      GOTO 8


8     CONTINUE

      CALL JEDETR(NOMCMP)
      CALL JEDETR(CORR1)
      CALL JEDETR(CORR2)

      CALL JEDEMA()
      END
