      SUBROUTINE MLTCCB(NBLOC,NCBLOC,DECAL,LGBLOC,NBSN,FILS,FRERE,SEQ,
     +     LGSN,LFRONT,ADRESS,LOCAL,LGPILE,NBASS,ADPER,T1,
     +     T2,FACTOL,FACTOU,TYPSYM,AD,NOMPIL,NMPILU,EPS,IER,SBLOC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 07/01/2002   AUTEUR JFBHHUC C.ROSE 
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
C TOLE CRP_21
C VERSION COMPLEXE DE MLTFCB
C     VERSION MODIFIEE POUR L' APPEL A CGEMV (PRODUITS MATRICE-VECTEUR)
C     LE STOCKAGE DES COLONNES DE LA FACTORISEE EST MODIFIE, ET AINSI
C      ADPER LES COLONNES FORMENT UN BLOC RECTANGULAIRE
      IMPLICIT NONE
      INTEGER PMIN
      PARAMETER (PMIN=10)
      INTEGER NBLOC,NCBLOC(*),DECAL(*),LGBLOC(*)
      INTEGER NBSN,LGSN(*),LFRONT(*),TYPSYM
      INTEGER LOCAL(*),NBASS(*),LGPILE(*),FILS(*)
      INTEGER ADRESS(*),FRERE(*),SEQ(*),AD(*)
      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*24 FACTOL,FACTOU,NOMPIL,NMPILU
C
      COMPLEX*16 T1(*),T2(*)
      REAL*8 EPS
      INTEGER ADPER(*),IFACL,IFACU,PPERE,PFILS,IER,SBLOC,PPEREU,PFILSU
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8,CARACI
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*6 PGC
      COMMON /NOMAJE/PGC
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER I,ISND,SNI,SN,N,M,P,NL,NC,ADFACL,IB,NB,ADFACU
      CALL JEMARQ()
      IF(SBLOC.EQ.1) THEN
C=======================================================================
C     CREATION D'UNE COLLECTION DISPERSEE
         CALL JECREC(NOMPIL,' V V C ','NO','DISPERSE','VARIABLE',NBSN)
         DO 120 I = 1,NBSN
            WRITE(CARACI,'(I8)') I
            CALL JECROC(JEXNOM(NOMPIL,CARACI))
            CALL JEECRA(JEXNOM(NOMPIL,CARACI),'LONMAX',LGPILE(I),' ')
 120     CONTINUE
         IF( TYPSYM.EQ.0) THEN
C     CREATION D'UNE AUTRE COLLECTION DISPERSEE ( CAS NON SYMETRIQUE)
            CALL JECREC(NMPILU,' V V C ','NO','DISPERSE','VARIABLE',
     +           NBSN)

            DO 121 I = 1,NBSN
               WRITE(CARACI,'(I8)') I
               CALL JECROC(JEXNOM(NMPILU,CARACI))
               CALL JEECRA(JEXNOM(NMPILU,CARACI),'LONMAX',LGPILE(I),' ')
 121        CONTINUE
         ENDIF
      ENDIF
      ISND = 0
      DO 159 IB = 1,SBLOC-1
         DO 159 NC = 1,NCBLOC(IB)
            ISND = ISND + 1
 159     CONTINUE
         DO 160 IB = SBLOC ,NBLOC
            DO 150 NC = 1,NCBLOC(IB)
               ISND = ISND + 1
               SNI = SEQ(ISND)
               P = LGSN(SNI)
               M = LFRONT(SNI)
               N = M + P
               IF( M.NE.0)  THEN
                  CALL JEVEUO(JEXNUM(NOMPIL,SNI),'E',PPERE)
                  IF( TYPSYM.EQ.0) THEN
                     CALL JEVEUO(JEXNUM(NMPILU,SNI),'E',PPEREU)
                  ENDIF
               ENDIF
               DO 19 I=1,P
                  ADPER(I) = (I-1)*N+I
 19            CONTINUE
               DO 130 I = P,N - 1
                  ADPER(I+1) = 1 + (N+ (N-I+1))*I/2
 130           CONTINUE
               SN = FILS(SNI)
C     BOUCLE D ASSEMBLAGE
C------------------------------------------------------------------
C     ASSEMBLAGE POUR LA PARTIE INFERIEURE
C     C
 140           CONTINUE
               IF (SN.NE.0) THEN
                  CALL JEVEUO(JEXNUM(NOMPIL,SN),'L',PFILS)
                  NL = LGSN(SN)
                  NB = NBASS(SN)
C     ASSEMBLAGE FILS -> PERE ( INFERIEURE)
C
                  CALL MLTACF(LFRONT(SN),NB,ADPER,ZC(PPERE),
     +                 ZC(PFILS),LOCAL(ADRESS(SN)+NL),P)
                  CALL JELIBE(JEXNUM(NOMPIL,SN))
                  SN = FRERE(SN)
                  GO TO 140
               END IF
               IF( M.NE.0 )  CALL JELIBE(JEXNUM(NOMPIL,SNI))
C
               CALL JEVEUO(JEXNUM(FACTOL,IB),'E',IFACL)
               ADFACL = IFACL - 1 + DECAL(SNI)
               SN = FILS(SNI)
 145           CONTINUE
C     ASSEMBLAGE FILS -> FACTOR ( INFERIEURE)
               IF (SN.NE.0) THEN
                  CALL JEVEUO(JEXNUM(NOMPIL,SN),'L',PFILS)
                  NL = LGSN(SN)
                  NB = NBASS(SN)
                  CALL MLTACP(LFRONT(SN),NB,ADPER,ZC(ADFACL),
     +                 ZC(PFILS),LOCAL(ADRESS(SN)+NL))
C     ON DETRUIT LA MATRICE FRONTALE QUI NE SERA PLUS UTILISEE
                  CALL JEDETR(JEXNUM(NOMPIL,SN))
                  SN = FRERE(SN)
                  GO TO 145
               END IF
C------------------------------------------------------------------
C     ASSEMBLAGE POUR LA PARTIE SUPERIEURE
C
               IF( TYPSYM.EQ.0) THEN
                  CALL JELIBE( JEXNUM(FACTOL,IB))
                  SN = FILS(SNI)
 141              CONTINUE
                  IF (SN.NE.0) THEN
                     CALL JEVEUO(JEXNUM(NMPILU,SN),'L',PFILSU)
                     NL = LGSN(SN)
                     NB = NBASS(SN)
C     ASSEMBLAGE FILS -> PERE ( SUPERIEURE)
                     CALL MLTACF(LFRONT(SN),NB,ADPER,ZC(PPEREU),
     +                    ZC(PFILSU),LOCAL(ADRESS(SN)+NL),P)
                     CALL JELIBE(JEXNUM(NMPILU,SN))
                     SN = FRERE(SN)
                     GO TO 141
                  END IF

                  IF( M.NE.0 )  CALL JELIBE(JEXNUM(NMPILU,SNI))
                  CALL JEVEUO(JEXNUM(FACTOU,IB),'E',IFACU)
                  ADFACU = IFACU - 1 + DECAL(SNI)
                  SN = FILS(SNI)
 146              CONTINUE
                  IF (SN.NE.0) THEN
                     CALL JEVEUO(JEXNUM(NMPILU,SN),'L',PFILSU)
                     NL = LGSN(SN)
                     NB = NBASS(SN)
C     ASSEMBLAGE FILS -> FACTOR ( SUPERIEURE)
                     CALL MLTACP(LFRONT(SN),NB,ADPER,ZC(ADFACU),
     +                    ZC(PFILSU),LOCAL(ADRESS(SN)+NL))
C     ON DETRUIT LA MATRICE FRONTALE QUI NE SERA PLUS UTILISEE
                     CALL JEDETR(JEXNUM(NMPILU,SN))
                     SN = FRERE(SN)
                     GO TO 146
                  END IF
               END IF
C
C     FIN DE L'ASSEMBLAGE POUR LA PARTIE SUPERIEURE
C------------------------------------------------------------------
C
C     FACTORISATION
            IF( TYPSYM.EQ.1) THEN
               IF( M.NE.0)  CALL JEVEUO(JEXNUM(NOMPIL,SNI),'E',PPERE)
               IF (P.LE.PMIN) THEN
                  CALL MLTC21(P,ZC(ADFACL),ZC(PPERE),N,T1,T2,EPS,IER)
               ELSE
                  CALL MLTCLM(N,P,ZC(ADFACL),ADPER,T1,AD,EPS,IER)
                  CALL MLTCMJ(N,P,ZC(ADFACL),ZC(PPERE),ADPER,T1,AD)
               END IF
               IF( M.NE.0 )  CALL JELIBE(JEXNUM(NOMPIL,SNI))
            ELSE
               CALL JEVEUO(JEXNUM(FACTOL,IB),'E',IFACL)
               ADFACL = IFACL - 1 + DECAL(SNI)
               CALL MLNCLM(N,P,ZC(ADFACL),ZC(ADFACU),ADPER,
     +              T1,T2,AD,EPS,IER)
               IF( M.NE.0)  CALL JEVEUO(JEXNUM(NOMPIL,SNI),'E',PPERE)
               CALL MLNCMG(N,P,ZC(ADFACL),ZC(ADFACU),ZC(PPERE),
     +              ADPER,T1,AD)
               IF( M.NE.0)  CALL JELIBE(JEXNUM(NOMPIL,SNI))
               IF( M.NE.0)  CALL JEVEUO(JEXNUM(NMPILU,SNI),'E',PPEREU)
               CALL MLNCMG(N,P,ZC(ADFACU),ZC(ADFACL),ZC(PPEREU),
     +              ADPER,T1,AD)
               IF( M.NE.0)  CALL JELIBE(JEXNUM(NMPILU,SNI))
               CALL JELIBE(JEXNUM(FACTOU,IB))
            ENDIF
            CALL JELIBE(JEXNUM(FACTOL,IB))
C
            IF (IER.EQ.1) THEN
               CALL UTDEBM('E','FACTORISATION (MULFR8)',
     +              'MATRICE SINGULIERE.')
               CALL UTIMPI('L','PIVOT NUL AU SUPERNOEUD',1,SNI)
               CALL UTIMPI('L','BLOC NO ',1,IB)
               CALL UTIMPI('L','SND RELATIF NO ',1,NC)
               CALL UTFINM()
            END IF
C
 150     CONTINUE
 160  CONTINUE
C     CALL JEIMPM('MESSAGE',' AV DESTRUCTION DE LA PILE ')
      CALL JEDETR(NOMPIL)
      CALL JEDEMA()

      END
