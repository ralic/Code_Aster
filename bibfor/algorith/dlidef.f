      SUBROUTINE DLIDEF()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C
C     COMMANDE : DEFI_LIST_ENTI/OPERATION='DEFI'
C
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
      INTEGER IBID,IDEBUT,IREST,III,IPDT
      INTEGER VALI(2)
      CHARACTER*8 RESU
      CHARACTER*16 NOMCMD,CONCEP
      INTEGER I,J,NDIM,NBVALE,NV,JVAL,JBOR,NBOCC,JNBP,N1,NBVAL
      INTEGER KVAL,ICO,NP,JPAS,IOCC
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      NBVAL = 1
C
      CALL GETRES(RESU,CONCEP,NOMCMD)
      CALL GETVIS(' ','VALE',0,IARG,0,IBID,NV)
      CALL GETVIS(' ','DEBUT',0,IARG,1,IDEBUT,N1)
      CALL GETFAC('INTERVALLE',NBOCC)
C
      IF (NV.NE.0) THEN

      ELSE
        CALL WKVECT('&&DLIDEF.BORNE','V V I',NBOCC+1,JBOR)
        ZI(JBOR) = IDEBUT
        DO 10 IOCC = 1,NBOCC
          CALL GETVIS('INTERVALLE','JUSQU_A',IOCC,IARG,1,
     &                ZI(JBOR+IOCC),N1)
          III = ZI(JBOR+IOCC) - ZI(JBOR-1+IOCC)
          IF (III.LE.0) THEN
            VALI(1) = ZI(JBOR+IOCC-1)
            VALI(2) = ZI(JBOR+IOCC)
            CALL U2MESG('F','ALGORITH13_78',0,' ',2,VALI,0,0.D0)
          ENDIF
          CALL GETVIS('INTERVALLE','PAS',IOCC,IARG,0,IBID,NP)
          IF (NP.NE.0) THEN
            CALL GETVIS('INTERVALLE','PAS',IOCC,IARG,1,JPAS,N1)
            JNBP = INT(III/JPAS)
            IREST = III - JNBP*JPAS
            IF (IREST.NE.0) THEN
              VALI(1) = JPAS
              VALI(2) = IOCC
              CALL U2MESG('F','ALGORITH13_79',0,' ',2,VALI,0,0.D0)
            ENDIF

          ELSE
            CALL GETVIS('INTERVALLE','NOMBRE',IOCC,IARG,1,JNBP,N1)
            IF (JNBP.GT.0) THEN
              IPDT = INT(III/JNBP)
              IREST = III - JNBP*IPDT
              IF (IREST.NE.0) THEN
                VALI(1) = JNBP
                VALI(2) = IOCC
                CALL U2MESG('F','ALGORITH13_80',0,' ',2,VALI,0,0.D0)
              ENDIF
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
C
C
      IF (NV.NE.0) THEN
        NBVALE = -NV
        NDIM = MAX(1,NBVALE-1)
        CALL WKVECT(RESU//'           .LPAS','G V I',NDIM,JPAS)
        CALL WKVECT(RESU//'           .NBPA','G V I',NDIM,JNBP)
        CALL WKVECT(RESU//'           .BINT','G V I',NBVALE,JBOR)
        CALL WKVECT(RESU//'           .VALE','G V I',NBVALE,JVAL)
        CALL WKVECT('&&DLIDEF.VALE','V V I',NBVALE,KVAL)
        CALL GETVIS(' ','VALE',0,IARG,NBVALE,ZI(KVAL),NV)
        DO 20 I = 1,NBVALE - 1
          IF (ZI(KVAL+I-1).GE.ZI(KVAL+I)) THEN
            VALI(1) = ZI(KVAL+I-1)
            VALI(2) = ZI(KVAL+I)
            CALL U2MESG('F','ALGORITH13_81',0,' ',2,VALI,0,0.D0)
          ENDIF
          ZI(JPAS+I-1) = ZI(KVAL+I) - ZI(KVAL+I-1)
          ZI(JNBP+I-1) = 1
          ZI(JBOR+I-1) = ZI(KVAL+I-1)
          ZI(JVAL+I-1) = ZI(KVAL+I-1)
   20   CONTINUE
        ZI(JBOR+NBVALE-1) = ZI(KVAL+NBVALE-1)
        ZI(JVAL+NBVALE-1) = ZI(KVAL+NBVALE-1)

      ELSE
C
        CALL WKVECT(RESU//'           .LPAS','G V I',MAX(1,NBOCC),JPAS)
        CALL WKVECT(RESU//'           .NBPA','G V I',MAX(1,NBOCC),JNBP)
        CALL WKVECT(RESU//'           .BINT','G V I',NBOCC+1,JBOR)
C
        ZI(JBOR) = IDEBUT
        DO 30 IOCC = 1,NBOCC
          CALL GETVIS('INTERVALLE','JUSQU_A',IOCC,IARG,1,
     &                ZI(JBOR+IOCC),N1)
          III = ZI(JBOR+IOCC) - ZI(JBOR-1+IOCC)
          CALL GETVIS('INTERVALLE','PAS',IOCC,IARG,0,IBID,NP)
          IF (NP.NE.0) THEN
            CALL GETVIS('INTERVALLE','PAS',IOCC,IARG,1,
     &                  ZI(JPAS+IOCC-1),N1)
            ZI(JNBP+IOCC-1) = III/ZI(JPAS+IOCC-1)

          ELSE
            CALL GETVIS('INTERVALLE','NOMBRE',IOCC,IARG,1,
     &                  ZI(JNBP+IOCC-1),
     &                  N1)
            ZI(JPAS+IOCC-1) = III/ZI(JNBP+IOCC-1)
          ENDIF
          NBVAL = NBVAL + ZI(JNBP+IOCC-1)
   30   CONTINUE
C
C        --- ALLOCATION DE .VALE ET REMPLISSAGE DE CE DERNIER ---
        CALL WKVECT(RESU//'           .VALE','G V I',NBVAL,JVAL)
        ZI(JVAL) = ZI(JBOR)
        ICO = 0
        DO 50 I = 1,NBOCC
          IPDT = ZI(JPAS-1+I)
          DO 40 J = 1,ZI(JNBP-1+I) - 1
            ICO = ICO + 1
            ZI(JVAL+ICO) = ZI(JVAL+ICO-1) + IPDT
   40     CONTINUE
          ICO = ICO + 1
          ZI(JVAL+ICO) = ZI(JBOR+I)
   50   CONTINUE
      ENDIF
C
      CALL JEDEMA()
      END
