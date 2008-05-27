      SUBROUTINE WP4VEC (NBFREQ,NBVECT,NEQ,SHIFT,
     +                   VP,VECP,MXRESF,
     +                   RESUFI,RESUFR,LAGR,VAUC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER       NBFREQ,NBVECT,NEQ,LAGR(*),
     &              RESUFI(MXRESF,*),MXRESF
      COMPLEX*16    VECP(NEQ,*),SHIFT,VAUC(2*NEQ,*),VP(*)
      REAL*8        RESUFR(MXRESF,*)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/05/2008   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     RESTITUTION DES VALEURS PROPRES ET DES MODES DU PB QUADRATIQUE
C     AVEC TRI SUIVANT LES PARTIES IMAGINAIRES CROISANTES
C     ET NORMALISATION A LA PLUS GRANDE CMP NON LAGRANGE
C
C     -----------------------------------------------------------------
C IN  NBFREQ : I : NOMBRE DE MODES DEMANDES
C IN  NBVECT : I : NOMBRE DE VECTEURS DE LANCZOS
C IN  NEQ    : I : TAILLE DES MATRICES DU PB QUADRATIQUE
C IN  SHIFT  : C : VALEUR DU DECALAGE
C IN  LAGR   : I : INDICATEUR DES NON-LAGRANGE
C VAR VP     : C : VALEURS PROPRE DU PB QUADRATIQUE
C OUT VECP   : C : MODES DU PB QUADRATIQUE
C IN  VAUC   : C : MODES DU PB QUADRATIQUE COMPLET
C OUT RESUFR : C : TABLEAU DE POST-TRAITEMENT
C     -----------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      REAL*8     A,B,AM,OM,NMABP,NMABM,PREC,R8PREM,C,SEUILP,
     &           SEUILR,C1,C2,VALR(5),AUXRJ,AUXIJ,AUXRK,AUXIK,A1,A2,B1,
     &           B2,D
      INTEGER    I,J,K,AV1,AV2,IADIND,NBREEL,NBCMPP,NBCMPC,NBFRGA,
     &           VALI(5),IFM,NIV
      LOGICAL    TROUVE,LCONJ
C     -----------------------------------------------------------------
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- 1. PARTITION ,TEST DE CONJUGAISON, ELIMINATION DES CONJUGUES
C        REMARQUE : SI QR N' A PAS EU DE PB ALORS LES MODES REDUITS :
C                   * COMPLEXES ET 2 A 2 CONJUGUES,LES CONJUGUES
C                     APPARAISSENT LES UNS A LA SUITE DES AUTRES
C                   * REELS
C
C        IMPLEMENTATION : DANS UN TABLEAU D' ENTIER (ZI(IADIND))
C        INVARIANT      : T(I) = -2 VP(I) NON CLASSEE
C                         T(I) =  1 <=> VP(I) COMPLEXE AVEC CONJUGUEE
C                                             SELECTIONNEE
C                         T(I) = -1 <=> VP(I) COMPLEXE AVEC CONJUGUEE
C                                             ELIMINEE
C                         T(I) =  0 <=> VP(I) COMPLEXE SANS CONJUGUEE
C                                             OU REELLE
C                                             ELIMINEE
C     -----------------------------------------------------------------
C --- 1.1. PARTITION (OPERATEUR REEL)
      NBCMPP = 0
      NBCMPC = 0
      NBREEL = 0
C*****************************************************************
C     PRECISION MACHINE COMME DANS ARPACK
      PREC=(R8PREM()*0.5D0)**(2.0D+0/3.0D+0)
C     SI IM(VP)<SEUILR, VP EST CONSIDEREE COMME REELLE
      SEUILR=1.D-7
C     SI MAX(DELTA_RELATIF RE(VPJ-VPK), IDEM MIN(PARTIE REELLE,IMAG), 
C     VPK = CONJUGEE DE VPJ
      SEUILP=1.D-6
      CALL WKVECT('&&WP4VEC.INDIC.PART.VP','V V I',NBVECT,IADIND)
      DO 1 J = 1, NBVECT
         ZI(IADIND + J-1) = -2
1     CONTINUE
      DO 2 J = 1, NBVECT
         AUXRJ=DBLE(VP(J))
         AUXIJ=DIMAG(VP(J))
         IF ( ZI(IADIND + J-1).EQ.-2 ) THEN
            IF ( ABS(AUXIJ).LT.SEUILR ) THEN
               ZI(IADIND+J-1) = -3
               NBREEL=NBREEL+1
            ELSE
               IF (ABS(AUXRJ).LT.SEUILR) AUXRJ=0.D0
               K      = J + 1
               TROUVE = .FALSE.
3              CONTINUE
               IF ((.NOT. TROUVE ) .AND. ( K .LE. NBVECT) ) THEN
                  AUXRK=DBLE(VP(K))
                  AUXIK=DIMAG(VP(K))
                  IF (ABS(AUXRK).LT.SEUILR) AUXRK=0.D0
                  IF (ABS(AUXIK).LT.SEUILR) AUXIK=0.D0
                  C1=2.D0*SQRT((AUXRJ-AUXRK)**2+(AUXIJ+AUXIK)**2)
                  C2=SQRT(AUXRJ**2+AUXRK**2+AUXIJ**2+AUXIK**2)
                  IF (C2.LT.PREC) THEN
                    C=C1
                  ELSE
                    C=C1/C2
                  ENDIF
                  A1=2.D0*SQRT((AUXRJ-AUXRK)**2)
                  A2=SQRT(AUXRJ**2+AUXRK**2)
                  IF (A2.LT.PREC) THEN
                    A=A1
                  ELSE
                    A=A1/A2
                  ENDIF
                  B1=2.D0*SQRT((AUXIJ+AUXIK)**2)
                  B2=SQRT(AUXIJ**2+AUXIK**2)
                  IF (B2.LT.PREC) THEN
                    B=B1
                  ELSE
                    B=B1/B2
                  ENDIF
                  D=MIN(A,B)
                  D=MAX(D,C)
                  IF (D.LT.SEUILP) THEN
                    LCONJ=.TRUE.
                  ELSE
                    LCONJ=.FALSE.
                  ENDIF
C POUR DEBUG
C                  IF (J.EQ.3) THEN
C                   WRITE(IFM,*)'J/K/A/B/C/D ',J,K,A,B,C,D
C                   WRITE(IFM,*)'VPJ/VPK',AUXRJ,AUXIJ,AUXRK,AUXIK
C                 ENDIF
C FIN DEBUG
                  IF ((ZI(IADIND+K-1).EQ.-2).AND.LCONJ.AND.
     &                (AUXIJ*AUXIK.LT.0.D0)) THEN
                      TROUVE = .TRUE.
                      NBCMPC = NBCMPC + 1
C                 PB ALGORITHMIQUE, SANS DOUTE DES SEUILS A MODIFIER
C                   IF (AUXIJ*AUXIK.GT.0.D0) THEN                    
C                     WRITE(IFM,*)'J/K/A/B/C/D ',J,K,A,B,C,D
C                     WRITE(IFM,*)'VPJ/VPK',AUXRJ,AUXIJ,AUXRK,AUXIK
C                      CALL ASSERT(.FALSE.)
C                    ENDIF
                      IF ( AUXIJ.GT.0.D0) THEN
                         ZI(IADIND + J-1) =  1
                         ZI(IADIND + K-1) = -1
                      ELSE
                         ZI(IADIND + J-1) = -1
                         ZI(IADIND + K-1) =  1
                      ENDIF
                   ELSE
                      K = K + 1
                   ENDIF
C
                   GOTO 3
                ENDIF
                IF ( .NOT. TROUVE ) THEN
                   NBCMPP           = NBCMPP + 1
                   ZI(IADIND + J-1) = 0
                ENDIF
             ENDIF
          ENDIF
2     CONTINUE
C
      IF ( ZI(IADIND + NBVECT-1) .EQ. -2) THEN
         ZI(IADIND + NBVECT-1) = 0
         NBCMPP                = NBCMPP +1
      ENDIF
C POUR DEBUG
C       DO J=1,NBVECT
C         WRITE(6,*)J,ZI(IADIND+J-1),VP(J),-DIMAG(VP(J))/6.28
C       ENDDO
C       WRITE(6,*)NBREEL,NBCMPC,NBCMPP
C FIN DEBUG
      IF ( NBCMPP .GT. 0 ) THEN
         VALI (1) = NBREEL
         VALI (2) = NBCMPC
         VALI (3) = NBCMPP
         CALL U2MESG('A', 'ALGELINE4_87',0,' ',3,VALI,0,0.D0)
      ENDIF
C
      IF ( NBREEL .GT. 0 ) THEN
         VALI (1) = NBREEL
         VALI (2) = NBCMPC
         VALI (3) = NBCMPP
         CALL U2MESG('I', 'ALGELINE4_88',0,' ',3,VALI,0,0.D0)
      ENDIF

C
C --- 1.2. DETERMINATION DE NB FREQUENCES GARDEES
      NBFRGA = NBCMPC
C
C --- 1.3. ELIMINATION DES CONJUGUES (OPERATEUR REEL) -- COMPACTAGE --
      K = 1
      DO 44 J = 1, NBVECT
         IF ( ZI(IADIND + J-1) .GT. 0 ) THEN
            IF ( K .NE. J ) THEN
               VP(K) = VP(J)
               ZI(IADIND + K-1) = ZI(IADIND + J-1)
               DO 55, I = 1, NEQ, 1
                  VECP(I,K) = VECP(I,J)
                  VAUC(I,K) = VAUC(I,J)
                  VAUC(I+NEQ,K) = VAUC(I+NEQ,J)
55             CONTINUE
            ENDIF
            K = K + 1
         ENDIF
44     CONTINUE
       NBFRGA=K-1

C
C     ---------- FIN DE PARTITION TEST ET ELIMINATION -----------------
C     ----------    AU NIVEAU DE L' OPERATEUR REEL    -----------------
C
C --- 2. CALCUL DES SOLUTIONS PROPRES DU PB QUADRATIQUE ---
      CALL WKVECT('&&WP4VEC.VEC.AUX.C1','V V C',NEQ,AV1)
      CALL WKVECT('&&WP4VEC.VEC.AUX.C2','V V C',NEQ,AV2)
      DO 10 J = 1, NBFRGA
       IF (ZI(IADIND + J-1).GT.0) THEN
         CALL WPTEST(LAGR,VAUC(1,J),VAUC(NEQ+1,J),VP(J),NEQ,NMABP)
         IF (NMABP.GT.1.D-5) THEN
            ZI(IADIND + J-1)=0
            NBFRGA=NBFRGA-1
         ENDIF
       ENDIF
10    CONTINUE


C --- 1.3. ELIMINATION DES VALEURS FAUSSES -- RECOMPACTAGE --
      K = 1
      DO 4 J = 1, NBFRGA
         IF ( ZI(IADIND + J-1) .GT. 0 ) THEN
            IF ( K .NE. J ) THEN
               VP(K) = VP(J)
               ZI(IADIND + K-1) = ZI(IADIND + J-1)

               DO 5, I = 1, NEQ, 1
                  VECP(I,K) = VECP(I,J)
                  VAUC(I,K) = VAUC(I,J)
                  VAUC(I+NEQ,K) = VAUC(I+NEQ,J)
5              CONTINUE
            ENDIF
            K = K + 1
         ENDIF
4     CONTINUE
      NBFRGA=K-1

C --- 3. SELECTION DES VALEURS PROPRES (PB QUADRATIQUE)
      DO 20, J = 1, NBFRGA, 1
      IF ( (ZI(IADIND + J-1).EQ.1 ).AND.( DIMAG(VP(J)).LT.0.D0) ) THEN
            VP(J) = DCONJG(VP(J))
            DO 21 I = 1, NEQ
               VECP(I,J) = DCONJG(VECP(I,J))
               VAUC(I,J) = DCONJG(VAUC(I,J))
               VAUC(I+NEQ,J) = DCONJG(VAUC(I+NEQ,J))
21          CONTINUE
         ENDIF
20    CONTINUE

C
C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
C      CALL WPORDC(1,SHIFT,VP,VECP,NBFRGA,NEQ)
C      CALL WPORDC(0,SHIFT,VP,VECP,NBFRGA,NEQ)
C
C --- 5. PREPARATION DE RESUFR

       IF (NBFREQ.GT.NBFRGA) THEN
         VALI(1)=NBFREQ
         VALI(2)=NBFRGA
         CALL U2MESG('A','ALGELINE5_67',0,' ',2,VALI,0,VALR)
         NBFREQ=NBFRGA
       ENDIF  

C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
C          - TRI DANS LE SPECTRE : SUIVANT ABS(SHIFT - VPQ)
      CALL WPORDC(1,SHIFT,VP,VECP,NBFRGA,NEQ)
C          - TRI DE PRESNTATION  : SUIVANT IM(VPQ) - IM(SHIFT)
      CALL WPORDC(0,SHIFT,VP,VECP,NBFREQ,NEQ)

C --- 5. PREPARATION DE RESUFR
      DO 30 J = 1, NBFREQ 
         AM          = DBLE(VP(J))*DBLE(VP(J))
         OM          = DIMAG(VP(J))*DIMAG(VP(J))
         RESUFI(J,1) = J
         RESUFR(J,2) = OM
         RESUFR(J,3) = -DBLE(VP(J))/SQRT(OM + AM)
30    CONTINUE
C
C --- 6. DESTRUCTION DES OJB TEMPORAIRES
C
      CALL JEDETR('&&WP4VEC.VEC.AUX.C1')
      CALL JEDETR('&&WP4VEC.VEC.AUX.C2')
      CALL JEDETR('&&WP4VEC.INDIC.PART.VP')
      CALL JEDETR('&&WP4VEC.VECTEUR.AUX.U1C')
      CALL JEDETR('&&WP4VEC.VECTEUR.AUX.U2C')
      CALL JEDETR('&&WP4VEC.VECTEUR.AUX.U3C')
      CALL JEDETC('V','&&WP4VEC',1)
C
      CALL JEDEMA()
      END
