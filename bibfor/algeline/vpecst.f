      SUBROUTINE VPECST (IFM,TYPRES,OMGMIN,OMGMAX,NBFRE1,NBFRE2,
     +                   NBFREQ,NBLAGR)
      IMPLICIT NONE
      INTEGER             IUNIT,IFM,NBFRE1,NBFRE2,NBFREQ,NBLAGR
      REAL*8              OMGMIN, OMGMAX
      CHARACTER *16   TYPRES
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
C     ECRITURE DU NOMBRE DE FREQUENCE DANS UNE BANDE DONNEE
C     ------------------------------------------------------------------
C IN  IUNIT  : IS : UNITE LOGIQUE D'ECRITURE
C IN  OMGMIN : R8 : PULSATION MIN
C IN  OMGMAX : R8 : PULSATION MAX
C IN  NBFRE1 : IS : NB DE PULSATION INFERIEURE A OMGMIN
C IN  NBFRE2 : IS : NB DE PULSATION INFERIEURE A OMGMAX
C OUT NBFREQ : IS : NB DE PULSATION DANS LA BANDE OMGMIN OMGMAX
C IN  NBLAGR : IS : NB DE DDLS DE LAGARNGE
C IN  TYPRES : TX : TYPE DE CALCUL (DYNAMIQUE OU FLAMBEMENT)
C     ------------------------------------------------------------------
C     REMARQUE:  NBFRE1 ET NBFRE2  SONT CALCULES PAR VPSTUR
C     ------------------------------------------------------------------
      REAL*8      FREQOM,   FMIN,   FMAX
C     ------------------------------------------------------------------
C

C
C        --- NOMBRE DE FREQUENCE DANS LA BORNE ---
         IF (TYPRES .EQ. 'DYNAMIQUE') THEN
           NBFREQ = ABS( NBFRE2 - NBFRE1 )
         ELSE 
           IF ((OMGMIN *OMGMAX) .GE.0.D0) THEN
             NBFREQ = ABS( NBFRE2 - NBFRE1 )
           ELSE
             NBFREQ = ABS( NBFRE2 + NBFRE1 - 2 * NBLAGR )
           ENDIF
         ENDIF
         
         IF (NBFREQ .GT. 9999) THEN
             CALL UTMESS('A','VPECST','ON TROUVE PLUS DE 9999'//
     &            'VALEURS PROPRES DANS LA BANDE DEMANDEE')
             WRITE(IFM,*)' NOMBRE DE VALEURS PROPRES : ',NBFREQ
         ENDIF
         
         IF (TYPRES .EQ. 'DYNAMIQUE') THEN 
         
           FMIN   = FREQOM(OMGMIN)
           FMAX   = FREQOM(OMGMAX)
C
C        --- IMPRESSION ---
           WRITE (IFM,900)
           IF ( NBFREQ .EQ. 0 )  THEN
             WRITE (IFM,901) FMIN, FMAX
           ELSEIF ( FMIN .EQ. 0.D0 )  THEN
             WRITE (IFM,902) NBFREQ, FMAX
           ELSE
             WRITE (IFM,903) FMIN,FMAX,NBFREQ
           ENDIF
              
         ELSE  
         
           WRITE (IFM,800)
           IF ( NBFREQ .EQ. 0 )  THEN
             WRITE (IFM,801) OMGMIN, OMGMAX
           ELSEIF ( OMGMIN .EQ. 0.D0 )  THEN
             WRITE (IFM,802) NBFREQ, OMGMAX
           ELSE
             WRITE (IFM,803) OMGMIN,OMGMAX,NBFREQ
           ENDIF
           
         ENDIF  
         WRITE (IFM,904)
C     ------------------------------------------------------------------
  800 FORMAT(//,72('-'),/,'   VERIFICATION DU SPECTRE DE CHARGES ',
     +'CRITIQUES (SUITE DE STURM)',/)
  801 FORMAT(1X,'PAS DE CHARGES CRITIQUES DANS LA BANDE (',1PE12.5,',',
     +                                                 1PE12.5,') ')
  802 FORMAT(1X,I4,' CHARGES CRITIQUES INFERIEURES A ',1PE12.5,' HZ')
  803 FORMAT(1X,'LE NOMBRE DE CHARGES CRITIQUES DANS LA BANDE (',
     +                               1PE12.5,',',1PE12.5,') EST ',I4)

  900 FORMAT(//,72('-'),/,'   VERIFICATION DU SPECTRE DE FREQUENCES ',
     +'(SUITE DE STURM)',/)
  901 FORMAT(1X,'PAS DE FREQUENCES DANS LA BANDE (',1PE12.5,',',
     +                                                 1PE12.5,') ')
  902 FORMAT(1X,I4,' FREQUENCES PROPRES INFERIEURES A ',1PE12.5,' HZ')
  903 FORMAT(1X,'LE NOMBRE DE FREQUENCES DANS LA BANDE (',1PE12.5,
     +                                         ',',1PE12.5,') EST ',I4)
  904 FORMAT(72('-'),/)
C     ------------------------------------------------------------------
      END
