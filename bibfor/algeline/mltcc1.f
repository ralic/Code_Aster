      SUBROUTINE MLTCC1(NBLOC,NCBLOC,DECAL,FILS,FRERE,SEQ,
     +     LGSN,LFRONT,ADRESS,LOCAL,ADPILE,NBASS,PILE,LGPILE,ADPER,
     +     T1,T2,FACTOL,FACTOU,TYPSYM,AD,EPS,IER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
C RESPONSABLE JFBHHUC C.ROSE
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
C     TOLE CRP_21
C     VERSION MODIFIEE POUR L' APPEL A CGEMV (PRODUITS MATRICE-VECTEUR)
C     LE STOCKAGE DES COLONNES DE LA FACTORISEE EST MODIFIE, ET AINSI
C      ADPER LES COLONNES FORMENT UN BLOC RECTANGULAIRE
      IMPLICIT NONE
      INTEGER PMIN
      PARAMETER (PMIN=10)
      INTEGER NBLOC,NCBLOC(*),DECAL(*)
      INTEGER LGSN(*),LFRONT(*),LGPILE,TYPSYM
      INTEGER LOCAL(*),NBASS(*),ADPILE(*),FILS(*)
      INTEGER ADRESS(*),FRERE(*),SEQ(*),AD(*),IER
      COMPLEX*16 PILE(*)
      REAL*8 EPS
      CHARACTER*32 JEXNUM
      CHARACTER*24 FACTOL,FACTOU
C
      COMPLEX*16 T1(*),T2(*)
      INTEGER ADPER(*),IFACL,IFACU,LMATF
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
      CHARACTER*6 PGC
      COMMON /NOMAJE/PGC
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ITEMP,I,J,ISND,SNI,SN,N,M,P,NL,NC,MEM,ADFACL,IB,NB
      INTEGER LM1,LM2,ADFACU
      CALL JEMARQ()
      ITEMP = 1
      MEM = 0
      IER = 0
      ISND = 0
      DO 110 I = 1,LGPILE
         PILE(I) = 0.D0
 110  CONTINUE

      DO 170 IB = 1,NBLOC
         CALL JEVEUO(JEXNUM(FACTOL,IB),'E',IFACL)
         IF( TYPSYM.EQ.0 ) THEN
            CALL JEVEUO(JEXNUM(FACTOU,IB),'E',IFACU)
         ENDIF
         DO 160 NC = 1,NCBLOC(IB)
            ISND = ISND + 1
            SNI = SEQ(ISND)
            P = LGSN(SNI)
            M = LFRONT(SNI)
            N = M + P
            LMATF = (M* (M+1))/2
            LM1 =LMATF
            IF( TYPSYM.EQ.0 )     LMATF = 2*LMATF
C         CHANGTPOUR L' APPEL A CGEMV
          DO 19 I=1,P
           ADPER(I) = (I-1)*N+I
 19          CONTINUE
            DO 120 I = P,N - 1
               ADPER(I+1) = 1 + (N+ (N-I+1))*I/2
 120        CONTINUE
            SN = FILS(SNI)
            DO 130 J = 1, LMATF
               PILE(ITEMP+J-1) = 0.D0
 130        CONTINUE
            ADFACL = IFACL - 1 + DECAL(SNI)
            IF( TYPSYM.EQ.0) ADFACU = IFACU - 1 + DECAL(SNI)

            IF (SN.EQ.0) THEN
               IF (P.LE.PMIN.AND.TYPSYM.NE.0) THEN
                  CALL MLTC21(P,ZC(ADFACL),PILE(ITEMP),N,T1,T2,EPS,
     +                 IER)
               ELSE
                  IF( TYPSYM.EQ.0) THEN
                     CALL MLNCLM(N,P,ZC(ADFACL),ZC(ADFACU),ADPER,
     +                    T1,T2,AD,EPS,IER)
                     CALL MLNCMJ(N,P,ZC(ADFACL),ZC(ADFACU),PILE(ITEMP),
     +                    PILE(ITEMP+LM1),ADPER,T1,T2,AD)
                  ELSE
                     CALL MLTCLM(N,P,ZC(ADFACL),ADPER,
     +                    T1,AD,EPS,IER)
                     CALL MLTCMJ(N,P,ZC(ADFACL),PILE(ITEMP),ADPER,
     +                    T1,AD)
                  ENDIF
               END IF
               ADPILE(SNI) = ITEMP
               ITEMP = ITEMP + LMATF
            ELSE
C     DO WHILE (SN.NE.0)
 140           CONTINUE
               IF (SN.NE.0) THEN
                  NL = LGSN(SN)
                  NB = NBASS(SN)
                  LM2 = ( LFRONT(SN)*(LFRONT(SN)+1))/2
                  CALL MLTACP(LFRONT(SN),NB,ADPER,ZC(ADFACL),
     +                 PILE(ADPILE(SN)),LOCAL(ADRESS(SN)+NL))

                  CALL MLTACF(LFRONT(SN),NB,ADPER,PILE(ITEMP),
     +                 PILE(ADPILE(SN)),LOCAL(ADRESS(SN)+NL),P)
                  IF( TYPSYM.EQ.0) THEN
                     CALL MLTACP(LFRONT(SN),NB,ADPER,ZC(ADFACU),
     +                    PILE(ADPILE(SN)+LM2),LOCAL(ADRESS(SN)+NL))

                     CALL MLTACF(LFRONT(SN),NB,ADPER,PILE(ITEMP+LM1),
     +                    PILE(ADPILE(SN)+LM2),LOCAL(ADRESS(SN)+NL),P)
                  ENDIF
                  SN = FRERE(SN)
                  GO TO 140
C     FIN DO WHILE
               END IF
               IF (P.LE.PMIN.AND.TYPSYM.NE.0) THEN
                  CALL MLTC21(P,ZC(ADFACL),PILE(ITEMP),N,T1,T2,EPS,
     +                 IER)
               ELSE
                  IF( TYPSYM.EQ.0) THEN
                     CALL MLNCLM(N,P,ZC(ADFACL),ZC(ADFACU),ADPER,
     +                    T1,T2,AD,EPS,IER)
                     CALL MLNCMJ(N,P,ZC(ADFACL),ZC(ADFACU),PILE(ITEMP),
     +                    PILE(ITEMP+LM1),ADPER,T1,T2,AD)
                  ELSE
                     CALL MLTCLM(N,P,ZC(ADFACL),ADPER,
     +                    T1,AD,EPS,IER)
                     CALL MLTCMJ(N,P,ZC(ADFACL),PILE(ITEMP),ADPER,
     +                    T1,AD)
                  ENDIF
               END IF
               IF (IER.EQ.1) THEN
                  CALL UTDEBM('E','FACTORISATION (MULFR8)',
     +                 'MATRICE SINGULIERE.')
                  CALL UTIMPI('L','PIVOT NUL AU SUPERNOEUD',1,SNI)
                  CALL UTIMPI('L','BLOC NO ',1,IB)
                  CALL UTIMPI('L','SND RELATIF NO ',1,NC)
                  CALL UTFINM()
               END IF
               MEM = MAX(MEM, (ITEMP+ LMATF-1))
CRAY    DIR$ IVDEP
               DO 150 J = 1, LMATF
                  PILE(ADPILE(FILS(SNI))+J-1) = PILE(ITEMP+J-1)
 150           CONTINUE
               ADPILE(SNI) = ADPILE(FILS(SNI))
               ITEMP = ADPILE(SNI) + LMATF
            END IF
            MEM = MAX(MEM,ITEMP)
 160     CONTINUE

         CALL JELIBE(JEXNUM(FACTOL,IB))
         IF( TYPSYM.EQ.0 ) CALL JELIBE(JEXNUM(FACTOU,IB))
 170  CONTINUE
      CALL JEDEMA()
      END
