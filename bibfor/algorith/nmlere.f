      SUBROUTINE NMLERE(SDDISC,ACTION,INFZ,ITERAT,VALR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/07/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*19  SDDISC
      CHARACTER*1   ACTION
      CHARACTER*(*) INFZ
      INTEGER       ITERAT
      REAL*8        VALR(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES - SD DISCRETISATION)
C
C LECTURE/ECRITURE DANS SD STOCKAGE DES RESIDUS
C
C ----------------------------------------------------------------------
C
C IN  SDDISC : SD DISCRETISATION
C IN  ACTION : 'L' OU 'E'
C IN  ITERAT : NUMERO ITERATION NEWTON
C IN  INFO   : TYPE D'INFO A STOCKER OU A LIRE
C I/O VALR   : REEL   A ECRIRE OU A LIRE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
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
      CHARACTER*24 INFORE
      INTEGER      JIFRE
      CHARACTER*24 INFO
      INTEGER      ITER
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      INFORE = SDDISC(1:19)//'.IFRE'
      CALL JEVEUO(INFORE,'E',JIFRE)
      INFO   = INFZ
C
      CALL ASSERT( (ACTION.EQ.'E').OR.(ACTION.EQ.'L') )
C
      IF (INFO.EQ.'VRELA') THEN
        IF (ACTION.EQ.'E') THEN
          ZR(JIFRE+3*ITERAT+1-1) = VALR(1)
        ELSE
          VALR(1) = ZR(JIFRE+3*ITERAT+1-1)
        ENDIF
      ELSEIF (INFO.EQ.'VMAXI') THEN
        IF (ACTION.EQ.'E') THEN
          ZR(JIFRE+3*ITERAT+2-1) = VALR(1)
        ELSE
          VALR(1) = ZR(JIFRE+3*ITERAT+2-1)
        ENDIF      
      ELSEIF (INFO.EQ.'VCHAR') THEN  
        IF (ACTION.EQ.'E') THEN
          ZR(JIFRE+3*ITERAT+3-1) = VALR(1)
        ELSE
          VALR(1) = ZR(JIFRE+3*ITERAT+3-1)
        ENDIF
C
      ELSEIF (INFO.EQ.'VRELA_TOUS') THEN
        IF (ACTION.EQ.'L') THEN
          DO 10 ITER = 0, ITERAT
            VALR(ITER+1) = ZR(JIFRE+3*ITER+1-1)
10        CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF (INFO.EQ.'VRELA_TOUS') THEN
        IF (ACTION.EQ.'L') THEN
          DO 15 ITER = 0, ITERAT
            VALR(ITER+1) = ZR(JIFRE+3*ITER+2-1)
15        CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        ELSEIF (INFO.EQ.'VRELA_TOUS') THEN
        IF (ACTION.EQ.'L') THEN
          DO 20 ITER = 0, ITERAT
            VALR(ITER+1) = ZR(JIFRE+3*ITER+3-1)
20        CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF         
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
C
      END
