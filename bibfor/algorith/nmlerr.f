      SUBROUTINE NMLERR(SDDISC,ACTION,INFZ  ,ITERAT,VALR  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/07/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8        VALR
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES - SD DISCRETISATION)
C
C LECTURE/ECRITURE DANS SD STOCKAGE DES INFOS EN COURS DE CALCUL
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION
C IN  ACTION : 'L' OU 'E'
C IN  INFO   : TYPE D'INFO A STOCKER OU A LIRE
C   PAS_MINI_ELAS      : PAS_MINI_ELAS
C   RESI_GLOB_RELA     : RESI_GLOB_RELA DONNE
C   RESI_GLOB_MAXI     : RESI_GLOB_MAXI
C   INIT_NEWTON_KRYLOV : RESIDU INITIAL POUR NEWTON KRYLOV
C   ITER_NEWTON_KRYLOV : RESIDU COURANT POUR NEWTON KRYLOV
C   VRELA              : RESI_GLOB_RELA A L'ITERATION
C   VMAXI              : RESI_GLOB_MAXI A L'ITERATION
C IN  ITERAT : ITERATION DE NEWTON
C I/O VALR   : VALEUR A ECRIRE OU A LIRE
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
      CHARACTER*24 TPSERR
      INTEGER      JERRE
      CHARACTER*24 INFO
C     
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      TPSERR = SDDISC(1:19)//'.ERRE'
      CALL JEVEUO(TPSERR,'E',JERRE)
      INFO   = INFZ
C
      IF (INFO.EQ.'PAS_MINI_ELAS') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+4-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+4-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF (INFO.EQ.'RESI_GLOB_RELA') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+5-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+5-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF        
      ELSEIF (INFO.EQ.'RESI_GLOB_MAXI') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+6-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+6-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF         
      ELSEIF (INFO.EQ.'INIT_NEWTON_KRYLOV') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+8-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+8-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF 
      ELSEIF (INFO.EQ.'ITER_NEWTON_KRYLOV') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+9-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+9-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF (INFO.EQ.'VRELA') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+9+2*(ITERAT)+1-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+9+2*(ITERAT)+1-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF        
      ELSEIF (INFO.EQ.'VMAXI') THEN
        IF (ACTION.EQ.'L') THEN
          VALR  = ZR(JERRE+9+2*(ITERAT)+2-1)
        ELSEIF (ACTION.EQ.'E') THEN
          ZR(JERRE+9+2*(ITERAT)+2-1) = VALR
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF                               
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C       
      CALL JEDEMA()

      END
