      SUBROUTINE NMARET(NBARVZ,NNO,NDIM,NLISEQ,NBNO,NUMNOD,
     &                  GRO1,GRO2)

      IMPLICIT NONE
      INTEGER NBARVZ,NNO,NDIM,NBNO
      CHARACTER*19 NLISEQ
      CHARACTER*24 GRO1,GRO2,NUMNOD
       
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/02/2011   AUTEUR MASSIN P.MASSIN 
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
                
C
C ----------------------------------------------------------------------
C
C INITIALISATION DU PILOTAGE DDL_IMPO OU LONG_ARC - FORMULATION XFEM
C
C RENVOIE L'ENSEMBLE DES ARETES PILOTEES A PARTIR DES NOEUDS ENTRES PAR
C L'UTILISATEUR ET D'UNE LISTE D'ARETES VITALES
C SI L'UTILISATEUR N'A RIEN ENTRE, RENVOIE UN ENSEMBLE D'ARETES
C INDEPENDANTES
C
C ----------------------------------------------------------------------
C
C
C IN  NBARVI : NOMBRE D ARETES VITALES
C IN  NNO    : NOMBRE DE NOEUDS ENTRES PAR L'UTILISATEUR (EVT 0)
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NLISEQ : LISTE DES RELATIONS D EGALITE
C IN  NUMNOD : LISTE DES NUMEROS DES NOEUDS UTILISATEUR
C OUT  NBNO  : NOMBRE D ARETES FINALEMENT PILOTEES
C OUT  GRO1  : LISTE DES NOEUDS EXTREMITES 1 DES ARETES PILOTEES
C OUT  GRO1  : LISTE DES NOEUDS EXTREMITES 2 DES ARETES PILOTEES
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C TOLE CRS_1404
C
      CHARACTER*32       JEXNOM,JEXATR,JEXNUM
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER NBG
      INTEGER TABNOZ(3,NBARVZ),TABUT(3,NBARVZ),NBARVI
      REAL*8 TABCOZ(NDIM,NBARVZ),TABCRZ(NBARVZ)
      INTEGER JGRO1,JGRO2,J,JGRO,I,L,NRELEQ,REPERE,JLIS1
      INTEGER EFFAC,NAR,NARM,IRET,VALI(2)
      LOGICAL EXILI,NOEUAD
      CHARACTER*8 K8BID
      CHARACTER*19 NLISCO,NLISRL,NLISE2
C
C ----------------------------------------------------------------------
C     
      NBARVI=NBARVZ 
      CALL JEVEUO(NLISEQ,'L',JLIS1)
      CALL JEEXIN(NUMNOD,IRET)
      IF(NNO.GE.1.AND.IRET.NE.0)
     &     CALL JEVEUO(NUMNOD,'L',JGRO)      
      DO 10 I=1,NBARVI
         TABNOZ(1,I)=ZI(JLIS1-1+2*(I-1)+1)
         TABNOZ(2,I)=ZI(JLIS1-1+2*(I-1)+2)
         TABNOZ(3,I)=I
         DO 11 J=1,NDIM
            TABCOZ(J,I)=0.D0
11       CONTINUE
         TABCRZ(I)=I
10    CONTINUE


      IF(NNO.GE.1.AND.IRET.NE.0) THEN
         DO 20 I=1,NNO
            TABUT(1,I)=ZI(JGRO-1+I)
20       CONTINUE
         IF(NNO.GT.NBARVI) CALL U2MESS('F','PILOTAGE_62')
         DO 40 J=1,NNO
            NOEUAD=.FALSE.
            DO 41 I=1,NBARVI

               IF(TABUT(1,J).EQ.TABNOZ(1,I)) THEN
                  DO 42 L=1,NNO
                     IF((L.NE.J).AND.
     &                  (TABUT(1,L).EQ.TABNOZ(2,I))) THEN
                       VALI(1)=TABUT(1,J)
                       VALI(2)=TABUT(1,L)
                       CALL U2MESI('F','PILOTAGE_63',2,VALI)
                     ENDIF
42                CONTINUE
                  TABUT(2,J)=TABNOZ(2,I)
                  TABUT(3,J)=TABNOZ(3,I)
                  NOEUAD=.TRUE.
               ELSE IF(TABUT(1,J).EQ.TABNOZ(2,I)) THEN
                  DO 43 L=1,NNO
                     IF((L.NE.J).AND.
     &                  (TABUT(1,L).EQ.TABNOZ(1,I))) THEN
                       VALI(1)=TABUT(1,J)
                       VALI(2)=TABUT(1,L)                       
                       CALL U2MESI('F','PILOTAGE_63',2,VALI)
                     ENDIF
43                CONTINUE
                  TABUT(2,J)=TABNOZ(1,I)
                  TABUT(3,J)=TABNOZ(3,I)
                  NOEUAD=.TRUE.                  
               ENDIF
41          CONTINUE
            IF(.NOT.NOEUAD)
     &        CALL U2MESI('A','PILOTAGE_61',1,TABUT(1,J))
40       CONTINUE 
         DO 50 I=1,NNO
            DO 51 J=1,3
               TABNOZ(J,I)=TABUT(J,I)
51          CONTINUE
50       CONTINUE
         NBARVI=NNO 
      ENDIF

      EXILI=.FALSE.
       NLISE2 = '&&NMARET.LISEQ'
       NLISRL = '&&NMARET.LISRL'
       NLISCO = '&&NMARET.LISCO'
      CALL XRELL2(TABNOZ,NDIM,NBARVI,TABCOZ,TABCRZ,
     &                  EXILI,NLISE2,NLISRL,NLISCO)
      NAR=NBARVI
      CALL JEEXIN(NLISE2,IRET)
      IF(IRET.NE.0) THEN
      CALL JEVEUO(NLISE2,'L',JLIS1)
      CALL JELIRA(NLISE2,'LONMAX',NRELEQ,K8BID)
      IF(NRELEQ.GT.0) THEN
         DO 60 I=1,NRELEQ
            REPERE=0
            EFFAC=ZI(JLIS1-1+2*(I-1)+2)
            DO 61 J=1,NAR
               IF(EFFAC.EQ.TABNOZ(3,J)) THEN
                  REPERE=J
               ENDIF
61          CONTINUE
            IF(REPERE.GT.0) THEN
             IF(REPERE.LT.NAR) THEN
               NARM = NAR-1
               DO 220 L=REPERE,NARM
                  TABNOZ(1,L) = TABNOZ(1,L+1)
                  TABNOZ(2,L) = TABNOZ(2,L+1)
                  TABNOZ(3,L) = TABNOZ(3,L+1)
 220           CONTINUE
             ENDIF
             TABNOZ(1,NAR)=0
             TABNOZ(2,NAR)=0
             TABNOZ(3,NAR)=0
             NAR=NAR-1
            ENDIF
60       CONTINUE
      ENDIF
      ENDIF
      NBNO = NAR
      CALL WKVECT(GRO1,'V V I',NBNO,JGRO1)     
      CALL WKVECT(GRO2,'V V I',NBNO,JGRO2)

      DO 30 I=1,NBNO
         ZI(JGRO1-1+I)=TABNOZ(1,I)
         ZI(JGRO2-1+I)=TABNOZ(2,I)
30    CONTINUE
       CALL JEDETR(NLISE2)
       CALL JEDETR(NLISRL)      
       CALL JEDETR(NLISCO)
       CALL JEDETR('&&NMARET.CONNECTANT')
       CALL JEDETR('&&NMARET.CONNECTES')     
       END
