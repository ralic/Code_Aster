      SUBROUTINE SLECOL(IMOD,NBMAIL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 03/10/2006   AUTEUR ALEXEI A.MIKCHEVITCH 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     =================
CA PRESUPER
C
C     ================================================================
C     !                                                              !
C     !  FONCTION: ECRITURE DES GROUPES DE MAILLES ASSOCIES          !
C     !            AUX COULEURS                                      !
C     !                                                              !
C     ================================================================
C     !                                                              !
C     !  ROUTINES APPELES : CODENT                                   !
C     !                   : CODNOP                                   !
C     !                                                              !
C     !  ROUTINE APPELANTE : PRESUP                                  !
C     !                                                              !
C     ================================================================
C
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER      ZI
      REAL*8       ZR
      COMPLEX*16   ZC
      LOGICAL      ZL,EXISDG
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM,JEXATR
      CHARACTER*80 ZK80
C
      CHARACTER*1 PRFMAI
      CHARACTER*4 KBID
      CHARACTER*8 CHMAIL,CHGROU
      LOGICAL     LOGIQ(256)
      INTEGER     JPO(256),JNOMB(256),JMAX(256)
C  ------------ FIN DECLARATION -------------
C
C  -->N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
      CALL JEMARQ()
C
      PRFMAI = 'M'
      ICMAX = 256
      DO 10 I=1,ICMAX
       LOGIQ(I) = .FALSE.
       JPO(I) = 0
       JNOMB(I) = 0
       JMAX(I) = 1000
  10  CONTINUE
C
      NBMAX = 1000
      CALL JEVEUO('&&PRESUP.INFO.MAILLE','L',JINFO)
      DO 100 I=1,NBMAIL
        INUM = ZI(JINFO-1+(I-1)*4+1)
        CALL CODNOP(CHMAIL,PRFMAI,1,1)
        CALL CODENT(INUM,'G',CHMAIL(2:8))
        ICOL = ZI(JINFO-1+(I-1)*4+4)
        IPOS = ICOL + 1
        IF (IPOS.GT.ICMAX) THEN
           CALL U2MESS('A','STBTRIAS_2')
           GO TO 100
        ENDIF
        IF(.NOT.LOGIQ(IPOS)) THEN
          LOGIQ(IPOS)= .TRUE.
          CALL CODENT(ICOL,'G',KBID)
          CALL WKVECT('&&PRESUP.COUL'//KBID,'V V K8',NBMAX+1,JPO(IPOS))
        ENDIF
        NBMA = JNOMB(IPOS)
        NBTOT= JMAX(IPOS)
        IF(NBMA.GE.NBTOT) THEN
          CALL CODENT(ICOL,'G',KBID)
          NBTOT = NBTOT + NBMAX
          JMAX(IPOS) = NBTOT
          CALL JUVECA('&&PRESUP.COUL'//KBID,NBTOT+1)
          CALL JEVEUO('&&PRESUP.COUL'//KBID,'E',JPO(IPOS))
        ENDIF
        JNOMB(IPOS) = NBMA + 1
        ZK8(JPO(IPOS)-1+NBMA+1) = CHMAIL
 100  CONTINUE
C
C --> ECRITURE DES GROUPES DE MAILLES PAR COULEUR
C
      DO 110 IC=1,ICMAX
        IF(LOGIQ(IC)) THEN
          CALL CODENT((IC-1),'G',KBID)
          CHGROU = 'COUL_'//KBID
          WRITE(IMOD,'(A,4X,2A)')'GROUP_MA','NOM=',CHGROU
          NBMA = JNOMB(IC)
          WRITE (IMOD,'(8(2X,A))') (ZK8(JPO(IC)-1+J),J=1,NBMA)
          WRITE (IMOD,'(A)') 'FINSF'
          WRITE (IMOD,'(A)') '%'
          CALL JEDETR('&&PRESUP.COUL'//KBID)
        ENDIF
 110  CONTINUE
C
      CALL JEDEMA()
      END
