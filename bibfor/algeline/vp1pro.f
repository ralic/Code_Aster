      SUBROUTINE VP1PRO(OPTIOM,LRAIDE,LMASSE,LDYNAM,NEQ,NFREQ,NFREQB,
     +                  TOLV,NITV,IEXCL,FCORIG,VEC,RESUFI,RESUFR,RESUFK,
     +                  NPREC,NBRSSA,NBPARI,NBPARR,NBPARK,TYPRES,OPTIOF)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/02/2001   AUTEUR BOITEAU O.BOITEAU 
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
C     CALCUL DES VECTEURS ET VALEURS PROPRES PAR LA METHODE D'ITERATION
C     INVERSE.
C     ------------------------------------------------------------------
C IN  VALP : R8 : TABLEAU DES VALEURS PROPRES INITIALES
C
C     ------------------------------------------------------------------
C     ASTER INFORMATIONS:
C      30/01/2001 TOILETTAGE FORTRAN, IMPLICIT NONE
C        EN FLAMBEMENT ON REORDONNE LES MODES PAR ORDRE ALGEBRIQUE 
C        CROISSANT (AU LIEU DU MODULE COMME CELA RESTE LE CAS EN DYN)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C PARAMETRES D'APPEL
      INTEGER       NBPARI,NBPARR,NBPARK,NITV,IEXCL(*),NPREC,NBRSSA,
     &              LRAIDE,LMASSE,LDYNAM,NEQ,NFREQ,NFREQB,
     &              RESUFI(NFREQB,NBPARI)
      REAL*8        TOLV,VEC(NEQ,*),RESUFR(NFREQB,NBPARR),FCORIG
      CHARACTER*(*) OPTIOM,RESUFK(NFREQB,NBPARK)
      CHARACTER*16  TYPRES,OPTIOF
      
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

C VARIABLES LOCALES
      INTEGER       IDET,PLACE,IRPERM,LMX,LX0,IQUOTI,IPREC,IFREQ,IER,
     &              IMODE,JFREQ,ITER,NBESSA,I,IPERM,J,KL,NAUX
      REAL*8        DET,ERR,FREQOM,EPS,VALEUR,OMEGA2,RPERM
      CHARACTER*24  KPERM,CMULTI,CVECT0
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CMULTI = '&&VP1PRO.MODES.MULTIPLES'
      CVECT0 = '&&VP1PRO.VECTEUR.TAMPON '

C     --- SAUVEGARDE DES TERMES MX POUR ORTHOGONALISER MODES MULTIPLES -
      CALL WKVECT(CMULTI,'V V R',NEQ*NFREQ, LMX )
      CALL WKVECT(CVECT0,'V V R',NEQ      , LX0 )

      IQUOTI = 0
      IF ( OPTIOM(1:8) .EQ. 'RAYLEIGH') IQUOTI = 1
C     INITIALISATION A UNE VALEUR NON ATTEINTE      
      IPREC  = -( NFREQB + 1 )

      DO 10 IFREQ = 1,NFREQ
         VALEUR = RESUFR(IFREQ,2)
 20      CONTINUE
         CALL VPSTUR(LRAIDE,VALEUR,LMASSE,LDYNAM,NPREC,
     +               DET,IDET,PLACE,IER)
         IF ( IER.GT.1 ) THEN
            VALEUR = 1.01D0 * VALEUR
            GOTO 20
         ENDIF
         IF ( RESUFI(IFREQ,1) .EQ. 0 ) THEN
            IPREC = -( NFREQB + 1 )
            IMODE = 1
            JFREQ = IFREQ
         ELSE IF ( RESUFI(IFREQ,1) .EQ. IPREC ) THEN
            IMODE = IMODE + 1
         ELSE
            IPREC = RESUFI(IFREQ,1)
            IMODE = 1
            JFREQ = IFREQ
         ENDIF
         CALL VP1ITE(LMASSE,LRAIDE,LDYNAM,VEC(1,JFREQ),IMODE,VALEUR,
     +               NEQ,NITV,TOLV,ITER,
     +               ZR(LX0),ZR(LMX),ERR,IEXCL,PLACE,IQUOTI,NPREC)
         IF ( RESUFI(IFREQ,1) .EQ. 0 ) THEN
            RESUFI(IFREQ,1) = PLACE
         ELSEIF ( IMODE .GT. 1 ) THEN
             PLACE = RESUFI(IFREQ,1) - IMODE + 1
             RESUFI(IFREQ,1) = PLACE
         ENDIF
         RESUFR(IFREQ,2)  = VALEUR
         RESUFI(IFREQ,4)  = ITER
         RESUFR(IFREQ,15) = ERR
   10 CONTINUE

C RECALCUL DU NUME_MODE POUR CHAQUE FREQUENCE

      IF ((TYPRES .EQ. 'DYNAMIQUE').AND.(OPTIOF.NE.'PROCHE')) THEN 
      
      IFREQ = 1
 30   CONTINUE
      VALEUR = RESUFR(IFREQ,2)
      IF (ABS(VALEUR).LE. OMEGA2(FCORIG)) THEN
        IFREQ = IFREQ + 1
        IF (IFREQ .GT. NFREQ) THEN
          CALL UTMESS('A','VP1PRO.01','TOUTES LES FREQUENCES'//
     &                'SONT DES FREQUENCES DE CORPS RIGIDE' )
        ELSE
          GOTO 30
        ENDIF
      ENDIF
      NBESSA = 0
      IF (VALEUR .GE. 0.D0) THEN
        VALEUR = 0.95D0 * VALEUR
      ELSE
        VALEUR = 1.05D0 * VALEUR
      ENDIF
 40   CONTINUE
      CALL VPSTUR(LRAIDE,VALEUR,LMASSE,LDYNAM,NPREC,
     +            DET,IDET,PLACE,IER)
      IF ( IER.NE.0 ) THEN
        NBESSA = NBESSA + 1
        IF (NBESSA .GT. NBRSSA) THEN
          CALL UTMESS('F','VP1PRO.02','CALCUL DES NUME_MODE'//
     &    'MATRICE NON INVERSIBLE POUR LA FREQUENCE CONSIDEREE')
        ELSE
          IF (VALEUR .GE. 0.D0) THEN
            VALEUR = 0.95D0 * VALEUR
          ELSE
            VALEUR = 1.05D0 * VALEUR
          ENDIF
          GOTO 40
        ENDIF
      ENDIF
      DO 50 IFREQ = 1, NFREQ
        RESUFR(IFREQ,2) = RESUFR(IFREQ,2) - VALEUR
 50   CONTINUE
 
      ENDIF
      
C TRI DES VALEURS PROPRES SUIVANT ORDRE CROISSANT 
C EN DYNAMIQUE: EN VALEUR ABSOLUE
C EN FLAMBEMENT: EN VALEUR ALGEBRIQUE
       EPS = 1.D-7
       DO 310  I=1,NFREQ
        IPERM = I
        IF (TYPRES .EQ. 'DYNAMIQUE') THEN
          RPERM = ABS(RESUFR(I,2))
        ELSE
          RPERM = RESUFR(I,2)
        ENDIF
        DO 312  J=I+1,NFREQ
          IF (TYPRES .EQ. 'DYNAMIQUE') THEN  
            IF (ABS(RESUFR(J,2)) .LT. (RPERM *(1.D0 -EPS))) THEN
              IPERM = J
              RPERM = ABS(RESUFR(IPERM,2))
            ENDIF          
            IF ((ABS(RESUFR(J,2))-RPERM) .LE. (EPS*RPERM)) THEN
              IF (((RESUFR(J,2)*RESUFR(IPERM,2)).GE. 0.D0) .AND. 
     &             (ABS(RESUFR(J,2)) .LT. RPERM )) THEN
                IPERM = J
                RPERM = ABS(RESUFR(IPERM,2))
              ENDIF
              IF (((RESUFR(J,2)*RESUFR(IPERM,2)).LT. 0.D0) .AND. 
     &             ( RESUFR(J,2) .LT. 0.D0 )) THEN
                IPERM = J
                RPERM = ABS(RESUFR(IPERM,2))
              ENDIF
            ENDIF
          ELSE
            IF (RESUFR(J,2) .LT. RPERM) THEN
              IPERM = J
              RPERM = RESUFR(IPERM,2)
            ENDIF          
          ENDIF     
 312    CONTINUE

C PERMUTATION DES DONNEES LIEES AUX VALEURS PROPRES
        IF (IPERM.NE.I) THEN
          DO 320 KL = 1, NBPARR
            RPERM            = RESUFR(IPERM,KL)
            RESUFR(IPERM,KL) = RESUFR(I,KL)
            RESUFR(I,KL)     = RPERM
  320     CONTINUE    
          DO 321 KL = 1, NBPARI
            IRPERM           = RESUFI(IPERM,KL)
            RESUFI(IPERM,KL) = RESUFI(I,KL)
            RESUFI(I,KL)     = IRPERM
  321     CONTINUE 
          DO 322 KL = 1, NBPARK
            KPERM            = RESUFK(IPERM,KL)
            RESUFK(IPERM,KL) = RESUFK(I,KL)
            RESUFK(I,KL)     = KPERM
  322     CONTINUE            
          DO 330 J = 1,NEQ
            RPERM        = VEC(J,I)
            VEC(J,I)     = VEC(J,IPERM)
            VEC(J,IPERM) = RPERM
  330     CONTINUE
        ENDIF
  310 CONTINUE
 
       IF ((TYPRES.EQ.'DYNAMIQUE').AND.(OPTIOF.NE.'PROCHE')) THEN 
       
        NAUX = 0
        CALL RECTFR(NFREQ,NFREQ,NEQ,VALEUR,PLACE,NAUX,RESUFR(1,2),
     +              NFREQB,RESUFI,RESUFR,NFREQB)
     
      ELSE
        DO 51 IFREQ = 1,NFREQ
          RESUFI(IFREQ,1) = IFREQ
 51     CONTINUE  
      ENDIF
            
      DO 52 IFREQ = 1,NFREQ
         RESUFR(IFREQ,1) = FREQOM(RESUFR(IFREQ,2))
         RESUFK(IFREQ,2) = 'INVERSE_R'
 52   CONTINUE

C     --- DESTRUCTION ZONE DE TRAVAIL ---
      CALL JEDETR( CMULTI )
      CALL JEDETR( CVECT0 )

      CALL JEDEMA()
      END
