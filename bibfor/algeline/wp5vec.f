      SUBROUTINE WP5VEC (APPR,OPT,NBFREQ,NBVECT,NEQ,SHIFT,
     +                   VP,VECP,LMASSE,MXRESF,
     +                   RESUFI,RESUFR,LAGR,VAUC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1   APPR
      CHARACTER*(*) OPT
      INTEGER       NBFREQ,NBVECT,NEQ,LMASSE,LAGR(*),
     &              RESUFI(MXRESF,*),MXRESF
      COMPLEX*16    VECP(NEQ,*),SHIFT,VAUC(2*NEQ,*),VP(*)
      REAL*8        RESUFR(MXRESF,*)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/03/2003   AUTEUR NICOLAS O.NICOLAS 
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
C     AVEC MATRICE DE RAIDEUR COMPLEXE 
C     -----------------------------------------------------------------
C IN  APPR   : K : INDICATEUR D' APPROCHE 'R', 'I' OU 'C'
C IN  OPT    : K : OPTION : 'CENTRE' OU 'PLUS_PETITE'
C IN  NBFREQ : I : NOMBRE DE MODES DEMANDES
C IN  NBVECT : I : NOMBRE DE VECTEURS DE LANCZOS
C IN  NEQ    : I : TAILLE DES MATRICES DU PB QUADRATIQUE
C IN  SHIFT  : C : VALEUR DU DECALAGE
C IN  LMASSE : I : POINTEUR SUR LE DESCRIPTEUR DE LA MATRICE DE MASSE
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
      REAL*8     AM,OM
      INTEGER    I,J,K,AV1,AV2,IADIND
C     -----------------------------------------------------------------
      CALL JEMARQ()
C
      CALL WKVECT('&&WP5VEC.INDIC.PART.VP','V V I',NBVECT,IADIND)

C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-

      DO 1 J = 1, NBVECT
         ZI(IADIND + J-1) = -2
1     CONTINUE
      DO 2 J = 1, NBVECT
         IF ( ZI(IADIND + J-1) .EQ. -2 ) THEN
            IF ( DIMAG(VP(J)) .GT. 0.D0 ) THEN
               ZI(IADIND + J-1) = 0
            ELSE
               ZI(IADIND + J-1) =  1
            ENDIF
          ENDIF
2     CONTINUE
C
      IF ( ZI(IADIND + NBVECT-1) .EQ. -2) THEN
         ZI(IADIND + NBVECT-1) = 0
         NBCMPP                = NBCMPP +1
      ENDIF
C
C
C --- 1.3. ELIMINATION DES CONJUGUES (OPERATEUR REEL) -- COMPACTAGE --
      K = 1
      DO 4 J = 1, NBVECT
         IF (ZI(IADIND + J-1) .EQ. 0 ) THEN
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

C
C     ---------- FIN DE PARTITION TEST ET ELIMINATION -----------------
C     ----------    AU NIVEAU DE L' OPERATEUR REEL    -----------------
C
C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
      CALL WPORDC(1,DCMPLX(0.D0,0.D0),VP,VECP,NBFREQ,NEQ)
      
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
      IF ( OPT .EQ. 'CENTRE' ) THEN
         CALL JEDETR('&&WP5VEC.VEC.AUX.C1')
         CALL JEDETR('&&WP5VEC.VEC.AUX.C2')
C         CALL JEDETR('&&WP5VEC.VEC.AUX.C ')
      ENDIF
      CALL JEDETR('&&WP5VEC.INDIC.PART.VP')
      CALL JEDETR('&&WP5VEC.VECTEUR.AUX.U1C')
      CALL JEDETR('&&WP5VEC.VECTEUR.AUX.U2C')
      CALL JEDETR('&&WP5VEC.VECTEUR.AUX.U3C')
      CALL JEDETC('V','&&WP5VEC',1)
C
      CALL JEDEMA()
      END
