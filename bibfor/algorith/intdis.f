      SUBROUTINE INTDIS(COINT,NNOINT,NODDLI,DDLSST,NBSST)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/06/2010   AUTEUR CORUS M.CORUS 
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
C-----------------------------------------------------------------------
C    M. CORUS     DATE 05/02/10
C-----------------------------------------------------------------------
C
C  BUT:      < DETERMINATION DES PARTIES D'INTERFACES DISJOINTES >
C
C-----------------------------------------------------------------------
C  IN  : COINT  : DEFINITION DE LA CONNECTIVITE DE L'INTERFACE
C  IN  : NNOINT  : NOMBRE DE NOEUD A L'INTERFACE
C  IN  : NODDLI : DEFINITION DES DDL PORTES PAR LES NOEUDS D'INTERFACE
C  OUT : DDLSST   : DEFINITION DES DDL POUR CHAQUE PARTIE D'INTERFACE
C  OUT : NBSST    : NOMBRE DE PARTIE D'INTERFACE DISJOINTES
C-----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32  JEXNOM,JEXNUM      
C      
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------

C-- VARIABLES EN ENTREES / SORTIE
      INTEGER      NNOINT,NBSST
      CHARACTER*24 COINT,NODDLI,DDLSST
     
C-- VARIABLES DE LA ROUTINE  
      INTEGER      I1,J1,K1,M1,N1,L1,LINDNO,LDEFIN,LCONNC,LNOEU,
     &             LVEC,LIND,LSST,NZ0,NZ1,NBIND,LINDIN,DECAL,NBNO,
     &             NBVOIS,NO,LNDDLI
      COMPLEX*16   CBID
      CHARACTER*1  LISTYP(2),KBID
      CHARACTER*8  NOMCMP(6)
      CHARACTER*19 LISMAT(2),IMPED,SOLVEU,NUME91,NUME
      CHARACTER*24 MAKRY,VTEMP,VTEMP2,INDDDL,INDLAG,MATINT,VALK 

C-----------C
C--       --C     
C-- DEBUT --C
C--       --C
C-----------C

      CALL JEMARQ()

C-- CONSTRUCTION DE LA CONNECTIVITE REDUITE
     
      CALL JEVEUO('&&MOIN93.IND_NOEUD','L',LINDNO)
      CALL WKVECT('&&INTDIS.DEFI_SS_LIB','V V R',NNOINT**2,LDEFIN)
      CALL JEVEUO(COINT,'L',LCONNC)

      DO 10 I1=1,NNOINT
        NBVOIS=ZI(LCONNC+I1-1)
        DO 20 K1=1,NBVOIS
          NO=ZI(LCONNC+NNOINT*K1+I1-1)
          J1=ZI(LINDNO+NO-1)
          ZR(LDEFIN+(J1-1)*NNOINT+I1-1)=1.D0
          ZR(LDEFIN+(J1-1)*NNOINT+J1-1)=1.D0
          ZR(LDEFIN+(I1-1)*NNOINT+I1-1)=1.D0
          ZR(LDEFIN+(I1-1)*NNOINT+J1-1)=1.D0
  20    CONTINUE      
  10  CONTINUE     
  
      CALL WKVECT('&&INTDIS.NUMERO_NOEUDS','V V I',NNOINT,LNOEU)
      
      DO 30 I1=1,NNOINT
        ZI(LNOEU+I1-1)=I1
  30  CONTINUE    

      CALL WKVECT('&&INTDIS.VECT_TEMP','V V R',NNOINT,LVEC)
      CALL WKVECT('&&INTDIS.VECT_IND_MAT','V V I',NNOINT,LIND)
      CALL WKVECT('&&INTDIS.VECT_INDSST','V V I',NNOINT,LSST)
      
C-- INITIALISATION      

      DECAL=0
      NBSST=0
      NBNO=0
      ZI(LSST)=1

C-- RECHERCHE DES PARTIES DISJOINTES

C      DO WHILE (NBNO .LT. NNOINT)
  666   CONTINUE
        NZ0=0
        K1=1
C        DO WHILE (NZ0 .EQ. 0)
  667     CONTINUE
          IF (ZI(LNOEU+K1-1) .GT. 0) THEN
            NZ0=1
            ZI(LIND+DECAL)=K1
          ENDIF
          K1=K1+1
          IF (NZ0 .EQ. 0) THEN
            GOTO 667
          ENDIF  
C        END DO
        
        NZ1=1
C        DO WHILE (NZ1 .GT. NZ0)
  668     CONTINUE
          NZ0=NZ1
          DO 40 J1=1,NZ1
            DO 50 I1=1,NNOINT
              ZR(LVEC+I1-1)=ZR(LVEC+I1-1)+
     &           ZR(LDEFIN+(ZI(LIND+DECAL+J1-1)-1)*NNOINT+I1-1)
  50        CONTINUE
  40      CONTINUE
      
          NZ1=0
          DO 60 I1=1,NNOINT
            IF ( ZR(LVEC+I1-1) .GT. 0.D0 ) THEN
              NZ1=NZ1+1
              ZI(LIND+DECAL+NZ1-1)=I1
              ZI(LNOEU+I1-1)=0
              ZR(LVEC+I1-1)=0.D0
            ENDIF
  60      CONTINUE

          IF (NZ1 .GT. NZ0) THEN
            GOTO 668
          ENDIF  
C        END DO

        NBSST=NBSST+1
        DECAL=DECAL+NZ1
        NBNO=NBNO+NZ1
        ZI(LSST+2*NBSST-1)=DECAL
        ZI(LSST+2*NBSST)=NBNO+1 

      IF (NBNO .LT. NNOINT) THEN
        GOTO 666
      ENDIF
C      END DO
        
      CALL JEVEUO(NODDLI,'L',LNDDLI)
      CALL WKVECT(DDLSST,'V V I',NBSST*6*NNOINT,
     &            LINDIN)
      DO 70 I1=1,NBSST
        K1=ZI(LSST+2*(I1-1))
        L1=ZI(LSST+2*(I1-1)+1)

        DO 80 J1=K1,L1      
          DO 90 N1=1,6
            ZI(LINDIN+6*NNOINT*(I1-1)+  
     &         6*(ZI(LIND+J1-1)-1)+N1-1 ) = 1
  90      CONTINUE  
  80    CONTINUE
  70  CONTINUE

C----------------------------------------C
C--                                    --C
C-- DESTRUCTION DES OBJETS TEMPORAIRES --C
C--                                    --C
C----------------------------------------C

      CALL JEDETR('&&INTDIS.DEFI_SS_LIB')
      CALL JEDETR('&&INTDIS.NUMERO_NOEUDS')
      CALL JEDETR('&&INTDIS.VECT_TEMP')
      CALL JEDETR('&&INTDIS.VECT_IND_MAT')
      CALL JEDETR('&&INTDIS.VECT_INDSST')

C---------C
C--     --C
C-- FIN --C
C--     --C
C---------C

      CALL JEDEMA()
      END
