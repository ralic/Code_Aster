      SUBROUTINE VPDICH(LRAIDE,LMASSE,LDYNAM,TOL,MXDICH,MXFREQ,NPREC,
     +                  NFREQ,VALP,IEME,DET,IDET,NBPAS,TYPRES,NBLAGR )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER          LRAIDE,LMASSE,LDYNAM
      REAL*8                                TOL
      INTEGER          MXFREQ,NFREQ,    IEME(*),IDET(*),NBPAS(*),NBLAGR
      REAL*8                                 VALP(*),    DET(*)
      CHARACTER*16 TYPRES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/04/2000   AUTEUR D6BHHBQ B.QUINNEZ 
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
C     RECHERCHE DE VALEURS PROPRES PAR DICHOTOMIE
C     ------------------------------------------------------------------
C IN  LRAIDE : IS : DESCRIPTEUR DE LA MATRICE DE RAIDEUR
C IN  LMASSE : IS : DESCRIPTEUR DE LA MATRICE DE MASSE
C IN  TOL    : R8 : TOLERANCE D'ADMISSION POUR SEPARATION PAR DICHOTOMIE
C IN  MXFREQ : IS : NOMBRE MAXIMUM DE FREQUENCES A SEPARER
C IN  NPREC  : IS : NOMBRE DE DECIMALES AUTORISEES LORS
C                   DE LA FACTORISATION
C OUT NFREQ  : IS : NOMBRE DE VALEURS DANS LES TABLEAUX RESULATS
C OUT VALP   : R8 : TABLEAU DES BORNES  DES INTERVALLES (VALEUR PROPRE)
C OUT IEME   : IS : TABLEAU DES POSITIONS MODALES DE LA FREQUENCE
C OUT DET    : R8 :  )   DET * 10**IDET VALEUR DU DETERMINANT DE LA
C OUT IDET   : IS :  )   MATRICE SHIFTEE A LA FREQUENCE I
C OUT NBPAS  : IS : NOMBRE DE DICHOTOMIES EFFECTUEES
C     ------------------------------------------------------------------
C      A UN PAS DONNE ON CALCULE F(I+1/2) = (F(I)+F(I+1))/2
C
C      SI F(I+1/2) EST A LA MEME POSITION MODALE QUE F(I-1)
C         ALORS ON REMPLACE F(I) PAR F(I+1/2)
C      SINONSI F(I+1/2) EST A LA MEME POSITION MODALE QUE F(I+2)
C         ALORS ON REMPLACE F(I+1) PAR F(I+1/2)
C      SINON
C         ON INSERE F(I+1/2) ENTRE F(I) ET F(I+1)
C      FINSI
C
C      CET ALGORITHME AVEC RECUPERATION DE LA PLACE NECESSITE DES
C      TABLEAUX DE TRAVAIL  DE LONGUEUR  MAXIMUM 2*NFREQB + NFREQ
C      OU  NFREQB EST LE NOMBRE DE FREQUENCES DANS LA BANDE DE RECHERCHE
C          NFREQ  EST LE NOMBRE DE FREQUENCES DONNEES MARQUANT LA BANDE
C     ------------------------------------------------------------------
C
      REAL*8    DX ,    VALPX
      INTEGER   IDX,    NBPASX, IX
      REAL*8    FREQOM, FREQ1,  FREQ2
C     ------------------------------------------------------------------
C
      DO 10 I = 2, NFREQ-1
        CALL VPSTUR(LRAIDE,VALP(I),LMASSE,LDYNAM,NPREC,DET(I),IDET(I),
     +              IEME(I),IER)
        IEME(I) = IEME(I) - NBLAGR
        IF (TYPRES .NE. 'DYNAMIQUE') THEN
          IF (VALP(I).LT.0.D0) THEN 
            IEME(I) = - IEME(I)
          ENDIF
        ENDIF
   10 CONTINUE
C
      DO 100 IPAS = 2, MXDICH
         IEME0 = - 1
         IENCOR = 0
         INTERV = NFREQ -1
         IBORN1 = 0
         DO 110 IP  = 1,  INTERV
            IBORN1 = IBORN1 + 1
            IBORN2 = IBORN1 + 1
            IEME0  = IEME(MAX(IBORN1-1,1))
            IEME1  = IEME(IBORN1)
            IEME2  = IEME(IBORN2)
            IEME3  = IEME(MIN(IBORN2+1,NFREQ))
C
            IF (ABS(IEME1-IEME(1)).LE.MXFREQ) THEN
               IF ( ABS(IEME2-IEME1).GT.1) THEN
C
                  FREQ1 = VALP(IBORN1)
                  FREQ2 = VALP(IBORN2)
                  VALPX = (FREQ1+FREQ2) * 0.5D0
                  IF ( ABS(FREQ2-FREQ1) .GE. TOL*VALPX ) THEN
                     IENCOR = 1
                     CALL VPSTUR(LRAIDE,VALPX,LMASSE,LDYNAM,NPREC,
     +                           DX,IDX,IX,IER)
                     IX = IX-NBLAGR
                     IF (TYPRES .NE. 'DYNAMIQUE') THEN
                       IF (VALPX.LT.0.D0) THEN 
                         IX = - IX
                       ENDIF
                     ENDIF
                     IF ( IX .EQ. IEME0 ) THEN
C                       --- ECRASER LA BORNE INF ---
                        IPLACE = IBORN1
                     ELSEIF ( IX .EQ. IEME3 ) THEN
C                       --- ECRASER LA BORNE SUP ---
                        IPLACE = IBORN2
                     ELSE
C                       --- INSERER ENTRE LES DEUX BORNES ---
                        NFREQ  = NFREQ + 1
                        IBORN1 = IBORN1 + 1
                        DO 120 JDEC = NFREQ , IBORN2 , -1
                           DET(  JDEC) = DET (JDEC-1)
                           IDET( JDEC) = IDET (JDEC-1)
                           VALP( JDEC) = VALP (JDEC-1)
                           IEME( JDEC) = IEME (JDEC-1)
                           NBPAS(JDEC) = NBPAS(JDEC-1)
  120                   CONTINUE
                        IPLACE = IBORN2
                     ENDIF
C
                     DET  (IPLACE) = DX
                     IDET (IPLACE) = IDX
                     VALP (IPLACE) = VALPX
                     IEME (IPLACE) = IX
                     NBPAS(IPLACE) = IPAS
C
                  ENDIF
               ENDIF
            ELSE
               NFREQ = NFREQ - (INTERV - IP + 1)
               GOTO 100
            ENDIF
C
  110    CONTINUE
         IF (IENCOR.EQ.0) GOTO 9999
  100 CONTINUE
C
 9999 CONTINUE
C
      END
