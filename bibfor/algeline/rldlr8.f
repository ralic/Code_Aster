      SUBROUTINE RLDLR8 ( NOMMAT, HCOL, ADIA , ABLO ,
     &                     NEQ   , NBBLOC , XSOL , NBSOL )
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*(*)       NOMMAT
      INTEGER                     HCOL(*),ADIA(*),ABLO(*)
      REAL*8                 XSOL (NEQ,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
C COMPIL PARAL
C     ------------------------------------------------------------------
C     RESOLUTION DU SYSTEME A COEFFICIENTS REELS:  A * X = B
C     LA MATRICE EST SYMETRIQUE ET A ETE FACTORISEE SOUS FORME L*D*LT
C     LA RESOLUTION EST EN PLACE
C
C     ON PEUT RESOUDRE SUR UNE SOUS-MATRICE DE A :
C     ON PREND LES NEQ PREMIERES LIGNES ET COLONNES (NEQ PEUT ETRE
C     INFERIEUR A LA DIMENSION DE LA MATRICE).
C
C     ON PEUT RESOUDRE NBSOL SYSTEMES D'UN COUP A CONDITION
C     QUE LES VECETURS SOIENT CONSECUTIFS EN MEMOIRE :
C     XSOL EST UN VECTEUR DE NBSOL*NEQ REELS
C     ------------------------------------------------------------------
C
C IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
C IN  HCOL    : IS : HCOL DE LA MATRICE
C             HCOL(I) RENVOIE LA HAUTEUR DE LA I-EME COLONNE
C IN  ADIA    : IS : ADRESSE DU TERME DIAGONALE DANS SON BLOC
C             ADIA(I) RENVOIE L'ADRESSE DE LA I-EME LIGNE DANS SON BLOC
C IN  ABLO    :  :   POINTEUR DE BLOC
C             ABLO(I+1) RENVOIE LE NO DE LA DERNIERE LIGNE DU I-EME BLOC
C
C IN  NEQ     : IS : NOMBRE D'EQUATIONS PRISES EN COMPTE
C                    C'EST AUSSI LA DIMENSION DES VECTEURS XSOL.
C IN  NBBLOC  : IS : NOMBRE DE BLOC DE LA MATRICE
C VAR XSOL    : R8 : EN ENTREE LES SECONDS MEMBRES
C                    EN ENTREE LES SOLUTIONS
C IN  NBSOL   : IS : NOMBRE DE SOLUTIONS / SECONDS MEMBRES
C     ------------------------------------------------------------------
C
C     --- RAPPEL SOMMAIRE DE L'ALGORITHME ------------------------------
C
C     POUR I = 1,2, ... ,N
C     !  ACC = 0
C     !  POUR K = 1,2, ... ,I-1
C     !  !  ACC = ACC +  K(I,K)* X(K)
C     !  FIN_POUR
C     !  X(I) = X(I)-ACC
C     FIN_POUR
C
C     POUR I = 1,2, ... ,N
C     !  X(I) = X(I)/K(I,I)
C     FIN_POUR
C
C     POUR I = N,N-1,... ,1
C     !  ACC = 0
C     !  POUR K = I+1, ... ,N
C     !  !  ACC = ACC +  K(K,I)* X(K)
C     !  FIN_POUR
C     !  X(I) = X(I) - ACC
C     FIN_POUR
C     ------------------------------------------------------------------
C
C     REFERENCE (HISTORIQUE) :
C     (1) P.D. CROUT,
C         A SHORT METHOD FOR EVALUATING DETERMINANTS AND SOLVING SYSTEMS
C         OF LINEAR EQUATIONS WITH REAL OR COMPLEX COEFFICIENTS.
C         AIEE TRANSACTION VOL 60, PP 1235-1240  (1941)
C     ------------------------------------------------------------------
C
C
C
C     ------------------------------------------------------------------
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
      CHARACTER*6      PGC, PGCANC
      COMMON  /NOMAJE/ PGC
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      REAL*8           R8VAL
      CHARACTER*24     NOMDIA, VALE
      CHARACTER*32     JEXNUM
C     ------------------------------------------------------------------
      DATA  VALE  /'                   .VALE'/
      DATA  NOMDIA/'                   .&VDI'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      PGCANC       = PGC
      PGC          = 'RLDLR8'
      VALE(1:19)   = NOMMAT
      NOMDIA(1:19) = NOMMAT
C
C     --- CREATION/RAPPEL D'UN TABLEAU POUR STOCKER LA DIAGONALE -------
      CALL JEEXIN( NOMDIA , IER )
      IF ( IER .EQ. 0 ) THEN
          CALL JECREO(NOMDIA,'V V R')
          CALL JEECRA(NOMDIA,'LONMAX',NEQ,'  ')
      ENDIF
      CALL JEVEUO( NOMDIA , 'E', LDIAG )
C
C     ------------------------------------------------------------------
C     --- PREMIERE  PARTIE : RESOLUTION DESCENDANTE ---
C     --- ET REMPLI LE TABLEAU DIAGONAL POUR L'ETAPE SUIVANTE
C
      DO 100 IBLOC = 1 , NBBLOC
         IF (ABLO(IBLOC).GE.NEQ) GO TO 101
C
C        -- IDERBL EST LE NUMERO DU DERNIER BLOC A TRAITER:
         IDERBL=IBLOC
         CALL JEVEUO( JEXNUM(VALE,IBLOC),'L',LMAT )
         DO 110 IEQUA = ABLO(IBLOC)+1 ,ABLO(IBLOC+1)
            IF (IEQUA.GT.NEQ) GO TO 111
            ILONG = HCOL (IEQUA)
            IADIA = LMAT + ADIA(IEQUA) - 1
            IDE   = IADIA - ILONG + 1
            IXX   = IEQUA - ILONG + 1
CMIC$ DO ALL SHARED (NBSOL,ILONG,IXX,IDE,IEQUA,XSOL,ZR)
CMIC$*       PRIVATE(ISOL,R8VAL,I)
            DO 120 ISOL = 1, NBSOL
               R8VAL = 0
               DO 130 I = 0, ILONG - 2
                  R8VAL = R8VAL + XSOL(IXX+I,ISOL) * ZR(IDE+I)
  130          CONTINUE
               XSOL(IEQUA,ISOL) = XSOL(IEQUA,ISOL) - R8VAL
  120       CONTINUE

            ZR(LDIAG+IEQUA-1) = ZR(IADIA)
  110    CONTINUE
  111    CONTINUE
         CALL JELIBE(JEXNUM(VALE,IBLOC))
  100 CONTINUE
  101 CONTINUE
C
C     ---  DEUXIEME PARTIE : RESOLUTION DIAGONALE
CMIC$ DO ALL SHARED (NBSOL,NEQ,LDIAG,XSOL,ZR)
CMIC$*       PRIVATE(ISOL,IEQUA)
      DO 200 ISOL = 1 , NBSOL
         DO 210 IEQUA = 1 , NEQ
            XSOL(IEQUA,ISOL) = XSOL(IEQUA,ISOL) / ZR(LDIAG+IEQUA-1)
 210     CONTINUE
 200  CONTINUE
C
C     --- TROISIEME  PARTIE : RESOLUTION REMONTANTE ---
      DO 300 IBLOC = IDERBL , 1 , -1
         CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',LMAT)
         DO 310 IEQUA = ABLO(IBLOC+1), ABLO(IBLOC)+1, -1
            IF (IEQUA.GT.NEQ) GO TO 310
            ILONG  = HCOL(IEQUA)
            IADIA  = LMAT + ADIA(IEQUA) - 1
            IDE   = IADIA - ILONG + 1
            IXX   = IEQUA - ILONG + 1

CMIC$ DO ALL SHARED (NBSOL,ILONG,IXX,IDE,IEQUA,XSOL,ZR)
CMIC$*       PRIVATE(ISOL,R8VAL,I)
            DO 320 ISOL=1,NBSOL
               R8VAL   = - XSOL(IEQUA,ISOL)
               IF (R8VAL .NE. 0 ) THEN
                 DO 330 I = 0, ILONG - 2
                    XSOL(IXX+I,ISOL)=XSOL(IXX+I,ISOL)+R8VAL*ZR(IDE+I)
 330             CONTINUE
               ENDIF
 320        CONTINUE
 310     CONTINUE
         CALL JELIBE(JEXNUM(VALE,IBLOC))
 300  CONTINUE
C
      CALL JELIBE(NOMDIA)
      PGC = PGCANC
C
      CALL JEDEMA()
      END
