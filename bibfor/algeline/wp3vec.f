      SUBROUTINE WP3VEC (APPR,OPT,NBFREQ,NBVECT,NEQ,SHIFT,
     +                   VPR,VPI,VECP,LMASSE,MXRESF,
     +                   RESUFI,RESUFR,LAGR,VAUC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1   APPR
      CHARACTER*(*) OPT
      INTEGER       NBFREQ,NBVECT,NEQ,LMASSE,LAGR(*),
     &              RESUFI(MXRESF,*),MXRESF
      COMPLEX*16    VECP(NEQ,*),SHIFT,VAUC(2*NEQ,*)
      REAL*8        RESUFR(MXRESF,*),VPR(*),VPI(*)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/02/2003   AUTEUR NICOLAS O.NICOLAS 
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
C     AVEC TRI SUIVANT LES PARTIES IMMAGINAIRES CROISANTES
C     ET NORMALISATION A LA PLUS GRANDE CMP NON LAGRANGE
C
C --> EN PUS ON MET UN TEST DE CONJUGAISON
C --> STOCKAGE D'UN SEUL COUPLE PARMI 2 CONJUGUES
C     -----------------------------------------------------------------
C IN  APPR   : K : INDICATEUR D' APPROCHE 'R' OU 'I'
C IN  OPT    : K : OPTION : 'CENTRE' OU 'PLUS_PETITE'
C IN  NBFREQ : I : NOMBRE DE MODES DEMANDES
C IN  NBVECT : I : NOMBRE DE VECTEURS DE LANCZOS
C IN  NEQ    : I : TAILLE DES MATRICES DU PB QUADRATIQUE
C IN  SHIFT  : C : VALEUR DU DECALAGE
C IN  LMASSE : I : POINTEUR SUR LE DESCRIPTEUR DE LA MATRICE DE MASSE
C IN  LAGR   : I : INDICATEUR DES NON-LAGRANGE
C IN  VAUC   : C : MODES DU PB QUADRATIQUE COMPLET
C VAR VPR    : R : IN  : PARTIE REELLE DES VALEURS PROPRE DU PB REDUIT
C            :   : OUT : PARTIE REELLE DES VALEURS PROPRE DU PB QUAD
C VAR VPI    : R : IN  : PARTIE IMMAGI DES VALEURS PROPRE DU PB REDUIT
C            :   : OUT : PARTIE IMMAGI DES VALEURS PROPRE DU PB QUAD
C OUT VECP   : C : MODES DU PB QUADRATIQUE
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
      REAL*8     SI,MOD2,A,B,NMABP,NMABM,AM,OM
      INTEGER    I,J,K,AV1,AV2,IADIND,NBREEL,NBCMPP,
     +           NBCMPC,NBFRGA
      COMPLEX*16 DES,VPQ,MHU,VPP,VPM
      LOGICAL    TROUVE
C     -----------------------------------------------------------------
      CALL JEMARQ()
      SI    = DIMAG(SHIFT)
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
      
      CALL WKVECT('&&WP3VEC.INDIC.PART.VP','V V I',NBVECT,IADIND)
      DO 1 J = 1, NBVECT
         ZI(IADIND + J-1) = -2
1     CONTINUE
      DO 2 J = 1, NBVECT
         IF ( ZI(IADIND + J-1) .EQ. -2 ) THEN
            IF ( VPI(J) .EQ. 0.D0 ) THEN
               ZI(IADIND + J-1) = 0
               NBREEL = NBREEL + 1
            ELSE
               K      = J + 1
               TROUVE = .FALSE.
3              CONTINUE
               IF ( (.NOT. TROUVE ) .AND. ( K .LE. NBVECT) ) THEN
                  IF ( ( ZI(IADIND + K-1) .EQ. -2 ) .AND.
     +                 ( DCMPLX(VPR(J),VPI(J)) .EQ.
     +              DCONJG(DCMPLX(VPR(K),VPI(K))) ) ) THEN
                      TROUVE = .TRUE.
                      NBCMPC = NBCMPC + 1
                      IF ( VPI(J) .GE. 0.D0 ) THEN
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
C
      IF ( NBCMPP .GT. 0 ) THEN
         CALL UTDEBM('A','OP0045.WP3VEC : PROBLEME QUADRATIQUE',
     +               'POUR LE PROBLEME REDUIT')
         CALL UTIMPI('L','VALEUR(S) PROPRE(S) REELLE(S)        '//
     +               '          : ',1,NBREEL)
         CALL UTIMPI('L','VALEUR(S) PROPRE(S) COMPLEXE(S) AVEC '//
     +               'CONJUGUEE : ',1,NBCMPC)
         CALL UTIMPI('L','VALEUR(S) PROPRE(S) COMPLEXE(S) SANS '//
     +               'CONJUGUEE : ',1,NBCMPP)
         CALL UTFINM()
      ENDIF
C
      IF ( NBREEL .GT. 0 ) THEN
         CALL UTDEBM('I','OP0045.WP3VEC : PROBLEME QUADRATIQUE',
     +                   'VOTRE PROBLEME EST FORTEMENT AMORTI.')
         CALL UTIMPI('L','VALEUR(S) PROPRE(S) REELLE(S)        '//
     +               '          : ',1,NBREEL)
         CALL UTIMPI('L','VALEUR(S) PROPRE(S) COMPLEXE(S) AVEC '//
     +               'CONJUGUEE : ',1,NBCMPC)
         CALL UTIMPI('L','VALEUR(S) PROPRE(S) COMPLEXE(S) SANS '//
     +               'CONJUGUEE : ',1,NBCMPP)
         CALL UTFINM()
      ENDIF
C
C --- 1.2. DETERMINATION DE NB FREQUENCES GARDEES
      NBFRGA = NBREEL + NBCMPP + NBCMPC
C
C --- 1.3. ELIMINATION DES CONJUGUES (OPERATEUR REEL) -- COMPACTAGE --
      K = 1
      DO 4 J = 1, NBVECT
         IF ( ZI(IADIND + J-1) .GT. 0 ) THEN
            IF ( K .NE. J ) THEN
               VPR(K)           = VPR(J)
               VPI(K)           = VPI(J)
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

C
C     ---------- FIN DE PARTITION TEST ET ELIMINATION -----------------
C     ----------    AU NIVEAU DE L' OPERATEUR REEL    -----------------
C
C --- 2. CALCUL DES SOLUTIONS PROPRES DU PB QUADRATIQUE ---
      IF ( OPT .EQ. 'CENTRE' ) THEN
         CALL WKVECT('&&WP3VEC.VEC.AUX.C1','V V C',NEQ,AV1)
         CALL WKVECT('&&WP3VEC.VEC.AUX.C2','V V C',NEQ,AV2)
      ENDIF
      DO 10 J = 1, NBFRGA
         A    = VPR(J)
         B    = VPI(J)
         MHU  = DCMPLX(A,B)
         MOD2 = A*A + B*B
         MOD2 = 1.D0/MOD2
         IF ( OPT .EQ. 'PLUS_PETITE' ) THEN
            A      =  A*MOD2
            B      = -B*MOD2
         ELSE IF ( OPT .EQ. 'CENTRE' ) THEN
            IF ( APPR .EQ. 'R' ) THEN
               DES = DCMPLX(1.D0,0.D0)-DCMPLX(4.D0*SI*SI,0.D0)*MHU*MHU
               DES = SQRT(DES)
               VPQ = .5D0*(DCMPLX(1.D0,0.D0)-DCMPLX(0.D0,2.D0*SI)*MHU +
     &                                                         DES)/MHU
               VPP = VPQ + SHIFT               
               CALL WPTEST(LAGR,LMASSE,VAUC(1,J),VAUC(NEQ+1,J),VPP,NEQ,
     +                     NMABP,ZC(AV1),ZC(AV2))
               VPQ = .5D0*(DCMPLX(1.D0,0.D0)-DCMPLX(0.D0,2.D0*SI)*MHU -
     &                                                         DES)/MHU
               VPM = VPQ + SHIFT
               CALL WPTEST(LAGR,LMASSE,VAUC(1,J),VAUC(NEQ+1,J),VPM,NEQ,
     +                  NMABM,ZC(AV1),ZC(AV2))
            ELSE
               DES = -DCMPLX(SI*SI,0.D0)*MHU*MHU + DCMPLX(SI,0.D0)*MHU
               DES =  SQRT(DES)
               VPQ = -DCMPLX(0.D0,SI) + DES/MHU
               VPP =  VPQ + SHIFT
               CALL WPTEST(LAGR,LMASSE,VAUC(1,J),VAUC(NEQ+1,J),VPP,NEQ,
     +                  NMABP,ZC(AV1),ZC(AV2))
               VPQ = -DCMPLX(0.D0,SI) - DES/MHU
               VPM =  VPQ + SHIFT
               CALL WPTEST(LAGR,LMASSE,VAUC(1,J),VAUC(NEQ+1,J),VPM,NEQ,
     +                  NMABM,ZC(AV1),ZC(AV2))
            ENDIF
            IF (NMABM .LT. NMABP ) THEN
               A = DBLE (VPM)
               B = DIMAG(VPM)
            ELSE
               A = DBLE (VPP)
               B = DIMAG(VPP)
            ENDIF
         ENDIF
         VPR(J) = A
         VPI(J) = B
10    CONTINUE
C
C --- 3. SELECTION DES VALEURS PROPRES (PB QUADRATIQUE)
      DO 20, J = 1, NBFRGA, 1
      IF ( (ZI(IADIND + J-1).EQ.1 ).AND.( VPI(J).LT.0.D0) ) THEN
            VPI(J) = -VPI(J)
            DO 21 I = 1, NEQ
               VECP(I,J) = DCONJG(VECP(I,J))
21          CONTINUE
         ENDIF
20    CONTINUE
C
C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
      CALL WPORDO(1,SHIFT,VPR,VPI,VECP,NBFRGA,NEQ)
      CALL WPORDO(0,SHIFT,VPR,VPI,VECP,NBFREQ,NEQ)
C
C --- 5. PREPARATION DE RESUFR
      DO 30 J = 1, NBFREQ 
         AM          = VPR(J)*VPR(J)
         OM          = VPI(J)*VPI(J)
         RESUFI(J,1) = J
         RESUFR(J,2) = OM
         RESUFR(J,3) = -VPR(J)/SQRT(OM + AM)
30    CONTINUE
C
C --- 6. DESTRUCTION DES OJB TEMPORAIRES
C
      IF ( OPT .EQ. 'CENTRE' ) THEN
         CALL JEDETR('&&WP3VEC.VEC.AUX.C1')
         CALL JEDETR('&&WP3VEC.VEC.AUX.C2')
C         CALL JEDETR('&&WP3VEC.VEC.AUX.C ')
      ENDIF
      CALL JEDETR('&&WP3VEC.INDIC.PART.VP')
      CALL JEDETR('&&WP3VEC.VECTEUR.AUX.U1C')
      CALL JEDETR('&&WP3VEC.VECTEUR.AUX.U2C')
      CALL JEDETR('&&WP3VEC.VECTEUR.AUX.U3C')
      CALL JEDETC('V','&&WP3VEC',1)
C
      CALL JEDEMA()
      END
