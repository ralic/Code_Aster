      SUBROUTINE TE0041(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/03/2000   AUTEUR CIBHHLV L.VIVAN 
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
C     CALCUL DES MATRICES DE RAIDEUR, MASSE, AMORTISSEMENT POUR
C     LES ELEMENTS DISCRETS : MECA_DIS_T_N      MECA_DIS_T_L
C                             MECA_DIS_TR_N     MECA_DIS_TR_L
C                             MECA_2D_DIS_T_N   MECA_2D_DIS_T_L
C                             MECA_2D_DIS_TR_N  MECA_2D_DIS_TR_L
C     ------------------------------------------------------------------
C IN  : OPTION : NOM DE L'OPTION A CALCULER
C IN  : NOMTE  : NOM DU TYPE_ELEMENT
C     ------------------------------------------------------------------
C       --- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                             ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C       ---  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      REAL*8     ETA, PGL(3,3), MATV(78), MATP(12,12)
      COMPLEX*16 HYST
      CHARACTER*1 K1BID
C     ------------------------------------------------------------------
      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
         N    = 78
         NNO  = 2
         NC   = 6
         NDIM = 3
      ELSEIF (NOMTE.EQ.'MECA_DIS_T_L') THEN
         N    = 21
         NNO  = 2
         NC   = 3
         NDIM = 3
      ELSEIF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
         N    = 21
         NNO  = 1
         NC   = 6
         NDIM = 3
      ELSEIF (NOMTE.EQ.'MECA_DIS_T_N' ) THEN
         N   = 6
         NNO = 1
         NC  = 3
         NDIM = 3
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
         N    = 21
         NNO  = 2
         NC   = 3
         NDIM = 2
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
         N    = 10
         NNO  = 2
         NC   = 2
         NDIM = 2
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
         N    = 6
         NNO  = 1
         NC   = 3
         NDIM = 2
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_T_N' ) THEN
         N    = 3
         NNO  = 1
         NC   = 2
         NDIM = 2
      ENDIF
      NDDL = NNO * NC
C
      IF (OPTION.EQ.'RIGI_MECA_HYST') THEN
         CALL JEVECH('PCADISK','L',JDC)
         CALL JEVECH('PRIGIEL','L',JDR)
         CALL JEVECH('PMATUUC','E',JDM)
         ETA  = ZR(JDC+N+1)
         HYST = DCMPLX(1.D0,ETA)
         DO 10 I = 1,N
            ZC(JDM+I-1) = ZR(JDR+I-1) * HYST
 10      CONTINUE
         GOTO 9999
      ENDIF
C
      IF (OPTION.EQ.'RIGI_MECA'.OR.OPTION.EQ.'RIGI_MECA_TANG'
     .     .OR.OPTION.EQ.'RIGI_FLUI_STRU') THEN
         CALL JEVECH('PCADISK','L',JDC)
         CALL JEVECH('PMATUUR','E',JDM)
      ELSEIF (OPTION.EQ.'MASS_MECA'.OR. OPTION.EQ.'MASS_MECA_DIAG'
     .         .OR. OPTION.EQ.'MASS_FLUI_STRU') THEN
         CALL JEVECH('PCADISM','L',JDC)
         CALL JEVECH('PMATUUR','E',JDM)
      ELSEIF (OPTION.EQ.'AMOR_MECA') THEN
         CALL JEVECH('PCADISA','L',JDC)
         CALL JEVECH('PMATUUR','E',JDM)
      ELSEIF (OPTION.EQ.'M_GAMMA') THEN
         CALL JEVECH('PCADISM','L',JDC)
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
      ELSE
         CALL UTMESS('F','ELEMENT DISCRET (TE0041)',
     +                   '"'//OPTION//'"    : OPTION NON TRAITEE')
      ENDIF
      CALL JEVECH('PCAORIE','L',LORIEN)
      IREP = NINT(ZR(JDC+N))
      IF (IREP.EQ.1) THEN
C
C        --- REPERE GLOBAL ==> PAS DE ROTATION ---
         IF (OPTION.EQ.'M_GAMMA') THEN
            CALL VECMA(ZR(JDC),N,MATP,NDDL)
            CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
         ELSE
            DO 20 I = 1,N
               ZR(JDM+I-1) = ZR(JDC+I-1)
 20         CONTINUE
         ENDIF
C
      ELSE IF (IREP.EQ.2)THEN
C
C        --- LOCAL ==> GLOBAL ---
         IF ( ZR(LORIEN).EQ.0.D0 .AND. ZR(LORIEN+1).EQ.0.D0
     +                           .AND. ZR(LORIEN+2).EQ.0.D0 )THEN
C           --- ANGLES NULS  ===>  PAS DE ROTATION ---
            IF (OPTION.EQ.'M_GAMMA') THEN
               CALL VECMA(ZR(JDC),N,MATP,NDDL)
               CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
            ELSE
               DO 30 I = 1,N
                  ZR(JDM+I-1) = ZR(JDC+I-1)
 30            CONTINUE
            ENDIF
         ELSE
C           --- ANGLES NON NULS  ===>  ROTATION ---
            CALL MATROT ( ZR(LORIEN) , PGL )
            IF (OPTION.EQ.'M_GAMMA') THEN
               IF (NDIM.EQ.3) THEN
                 CALL UTPSLG ( NNO, NC, PGL, ZR(JDC), MATV )
               ELSEIF (NDIM.EQ.2) THEN
                 CALL UT2MLG ( NNO, NC, PGL, ZR(JDC), MATV )
               ENDIF
               CALL VECMA(MATV,N,MATP,NDDL)
               CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
            ELSE
              IF (NDIM.EQ.3) THEN
                CALL UTPSLG ( NNO, NC, PGL, ZR(JDC), ZR(JDM) )
               ELSEIF (NDIM.EQ.2) THEN
                CALL UT2MLG ( NNO, NC, PGL, ZR(JDC), ZR(JDM) )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
 9999 CONTINUE
      END
