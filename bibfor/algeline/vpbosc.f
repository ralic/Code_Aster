      SUBROUTINE VPBOSC
     &  (TYPRES, NBMODE, NBVECT, OMESHI, VALPRO, NVPRO, VPINF, VPMAX,
     &   PRECDC, METHOD, OMECOR, STURM)
C-----------------------------------------------------------------------
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
C     RECTIFIE LES VALEURS PROPRES COMPLEXES
C-----------------------------------------------------------------------
C     IN  : TYPRES  : TYPE DE RESULTAT (DYNAMIQUE OU FLAMBEMENT)
C     IN  : NBMODE  : NOMBRE DE MODE DEMANDES
C     IN  : NBVECT  : NOMBRE DE VECTEURS UTILISES AU COURS DU CALCUL
C     IN  : OMESHI  : DECALAGE UTILISE POUR LE CALCUL
C     IN  : OMECOR  : OMEGA2 DE CORPS RIGIDE
C     IN  : VALPRO  : VALEURS PROPRES
C     IN  : NVPRO   : DIMENSION DU VECTEUR VALPRO
C     IN  : METHOD  : TYPE DE METHODE
C     IN  : STURM   : FLAG NOTIFIANT LA SITUATRION VPINF*VPMAX < 0
C     IN  : PRECDC  : POURCENTAGE D'AUGMENTATION DES BORNES
C     OUT : VPINF : PLUS PETIT OMEGA2 CALCULE ET RETENU
C     OUT : VPMAX : PLUS GRAND OMEGA2 CALCULE ET RETENU
C    ------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       INFNIV, FREQOM, UTMESS, UTDEBM, UTFINM, UTIMPR
C     FONCTIONS INTRINSEQUES:
C       ABS, SIGN.
C    ------------------------------------------------------------------
C     ASTER INFORMATIONS:
C      24/01/2000 TOILETTAGE FORTRAN, IMPLICIT NONE,
C                 RAJOUT DU PARAMETRE STURM POUR VERIFICATION ETENDUE,
C                 PRISE EN COMPTE DE LA METHODE DE SORENSEN,
C                 UTILISATION DE SIGN ET DEPLACEMENT DES LOGINF/MAX.
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER NBMODE, NBVECT, NVPRO
      REAL*8  PRECDC, OMECOR, VPINF, VPMAX
      COMPLEX*16 VALPRO(NVPRO), OMESHI
      CHARACTER*8 METHOD
      CHARACTER*16 TYPRES
      LOGICAL STURM
C     ------------------------------------------------------------------
      REAL*8 VPINF2, VPMAX2, TOLE, FREQOM, R8PREM
      LOGICAL LOGINF, LOGMAX
      INTEGER NIV, IFM, I
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     --------  RECTIFICATION DES FREQUENCES DUE AU SHIFT  -------------
C     --------     DETERMINATION DE LA POSITION MODALE     -------------
C     ------------------------------------------------------------------
C
C     ---RECUPERATION DU NIVEAU D'IMPRESSION----
      CALL INFNIV(IFM,NIV)
C     -------------------------------------------
C
      DO 5 I = 1, NBVECT
         VALPRO(I) = VALPRO(I) + OMESHI
 5    CONTINUE

      VPINF = DBLE(VALPRO(1))
      VPMAX = DBLE(VALPRO(1))
      DO 10 I = 2, NBMODE
         IF (DBLE(VALPRO(I)).LT.VPINF) THEN
            VPINF = DBLE(VALPRO(I))
         ENDIF
         IF (DBLE(VALPRO(I)).GT.VPMAX) THEN
            VPMAX = DBLE(VALPRO(I))
         ENDIF
 10   CONTINUE
      IF (NIV .GE. 1) THEN
         IF (TYPRES .EQ. 'DYNAMIQUE') THEN
           WRITE(IFM,1600)
           WRITE(IFM,1000)
           WRITE(IFM,1100)
           WRITE(IFM,1200) FREQOM(VPINF)
           WRITE(IFM,1300) FREQOM(VPMAX)
           WRITE(IFM,1000)
         ELSE
           WRITE(IFM,1600)
           WRITE(IFM,1000)
           WRITE(IFM,1101)
           WRITE(IFM,1201) VPINF
           WRITE(IFM,1301) VPMAX
           WRITE(IFM,1000)
         ENDIF
      ENDIF
      IF ((VPMAX*VPINF).LT.0.D0) THEN
        STURM = .TRUE.
      ELSE
        STURM = .FALSE.
      ENDIF

      IF (METHOD .EQ. 'SORENSEN') THEN
         IF (ABS(VPMAX) .LE. OMECOR) THEN
             VPMAX=OMECOR
         ENDIF
         IF (ABS(VPINF) .LE. OMECOR) THEN
             VPINF=-OMECOR
         ENDIF
         VPINF = VPINF * (1.D0 - SIGN(PRECDC,VPINF))
         VPMAX = VPMAX * (1.D0 + SIGN(PRECDC,VPMAX))
      ENDIF
C
C     -----POUR LES OPTIONS JACOBI ET LANCZOS---
C
      LOGINF = .FALSE.
      LOGMAX = .FALSE.
      IF (METHOD .NE. 'SORENSEN') THEN
       DO 20 I = NBMODE+1, NBVECT
         IF (DBLE(VALPRO(I)).LE.VPINF) THEN
            IF (.NOT.LOGINF) THEN
               LOGINF = .TRUE.
               VPINF2 = DBLE(VALPRO(I))
            ENDIF
         ENDIF
         IF (DBLE(VALPRO(I)).GE.VPMAX) THEN
            IF (.NOT.LOGMAX) THEN
               LOGMAX = .TRUE.
               VPMAX2 = DBLE(VALPRO(I))
            ENDIF
         ENDIF
 20   CONTINUE
C
C     ----ON REGARDE L'ECART QU'IL Y A ENTRE FREQMIN ET LA
C         FREQUENCE PRECEDENTE, PUIS ON RECALCULE FREQMIN-----
C
      IF (LOGINF) THEN
         IF (VPINF2.LT.VPINF) THEN
             IF(VPINF .GT. R8PREM()) THEN
                TOLE=(ABS(VPINF2-VPINF)/VPINF)
                IF (TOLE .LT. PRECDC) THEN
                    CALL UTMESS ('A','VPBOSC.01','IL Y A DES '//
     +                           'VALEURS PROPRES TRES PROCHES')
                    CALL UTDEBM ('A','VPBOSC.01','LA VAL. PRO. EST: ')
                    CALL UTIMPR ('S',' ',1,FREQOM(VPINF2))
                    CALL UTFINM()
                    VPINF = VPINF * (1.D0 - SIGN(PRECDC,VPINF))
                ENDIF
             ELSE
                TOLE=ABS(VPINF2-VPINF)
                IF (TOLE .LT. PRECDC) THEN
                  CALL UTMESS ('A','VPBOSC.01','IL Y A DES '//
     +                         'VALEURS PROPRES TRES PROCHES')
                 CALL UTDEBM ('A','VPBOSC.01','LA VAL. PRO. EST: ')
                 CALL UTIMPR ('S',' ',1,FREQOM(VPINF2))
                 CALL UTFINM()
                 VPINF = VPINF * (1.D0 - SIGN(PRECDC,VPINF))
                ENDIF
             ENDIF
             VPINF = 0.5D0 * (VPINF+VPINF2)
         ELSE
           VPINF = VPINF * (1.D0 - SIGN(PRECDC,VPINF))
         ENDIF
      ELSE
        VPINF = VPINF * (1.D0 - SIGN(PRECDC,VPINF))
      ENDIF
C
C     -----ON FAIT LES MEMES CALCULS AVEC FREQMAX------
C
      IF (LOGMAX) THEN
         IF (VPMAX2.GT.VPMAX) THEN
            IF(VPINF .GT. R8PREM()) THEN
               TOLE=(ABS(VPMAX2-VPMAX)/VPMAX)
               IF (TOLE .LT. PRECDC) THEN
                  CALL UTMESS ('A','VPBOSC.02','IL Y A DES VP '//
     +                                         'TRES PROCHES')
                  CALL UTDEBM ('A','VPBOSC.01','LA VP EST: ')
                  CALL UTIMPR ('S',' ',1,FREQOM(VPMAX2))
                  CALL UTFINM()
                  VPMAX = VPMAX * (1.D0 + SIGN(PRECDC,VPMAX))
               ENDIF
            ELSE
               TOLE=ABS(VPMAX2-VPMAX)
               IF (TOLE .LT. PRECDC) THEN
                   CALL UTMESS ('A','VPBOSC.02','IL Y A DES VP '//
     +                                          'TRES PROCHES')
                   CALL UTDEBM ('A','VPBOSC.01','LA VP EST: ')
                   CALL UTIMPR ('S',' ',1,FREQOM(VPMAX2))
                   CALL UTFINM()
                   VPMAX = VPMAX * (1.D0 + SIGN(PRECDC,VPMAX))
               ENDIF
            ENDIF
            VPMAX = 0.5D0 * (VPMAX+VPMAX2)
         ELSE
           VPMAX = VPMAX * (1.D0 + SIGN(PRECDC,VPMAX))
         ENDIF
      ELSE
        VPMAX = VPMAX * (1.D0 + SIGN(PRECDC,VPMAX))
      ENDIF
      ENDIF
C
C     -----DETERMINATION DE FREQMIN ET FREQMAX-----
C
      IF (ABS(VPMAX) .LE. OMECOR) THEN
         VPMAX=OMECOR
      ENDIF
      IF (ABS(VPINF) .LE. OMECOR) THEN
         VPINF=-OMECOR
      ENDIF
C
C      -----IMPRESSIONS-----
C
      IF (LOGINF) THEN
        IF (NIV .GE. 1) THEN
          IF (TYPRES .EQ. 'DYNAMIQUE') THEN
            WRITE(IFM,1400) FREQOM(VPINF2)
          ELSE
            WRITE(IFM,1401) VPINF2
          ENDIF
        ENDIF

      ENDIF
      IF (LOGMAX) THEN
        IF (NIV .GE. 1) THEN
          IF (TYPRES .EQ. 'DYNAMIQUE') THEN
            WRITE(IFM,1500) FREQOM(VPMAX2)
          ELSE
            WRITE(IFM,1501) VPMAX2
          ENDIF
        ENDIF
      ENDIF
      IF (NIV .GE. 1) THEN
        WRITE(IFM,1000)
        WRITE(IFM,1600)
      ENDIF
C

 1000 FORMAT (7X)
 1100 FORMAT (3X,'LES FREQUENCES CALCULEES INF. ET SUP. SONT: ')
 1200 FORMAT (6X,'FREQ_INF : ',1PE12.5)
 1300 FORMAT (6X,'FREQ_SUP : ',1PE12.5)
 1400 FORMAT (3X,'LA PREMIERE FREQUENCE INFERIEURE NON RETENUE EST: ',
     +        1PE12.5)
 1500 FORMAT (3X,'LA PREMIERE FREQUENCE SUPERIEURE NON RETENUE EST: ',
     +        1PE12.5)
 1600 FORMAT (72('-'))
 1101 FORMAT (3X,'LES CHARGES CRITIQUES CALCULEES INF. ET SUP. SONT: ')
 1201 FORMAT (6X,'CHARGE_CRITIQUE_INF : ',1PE12.5)
 1301 FORMAT (6X,'CHARGE_CRITIQUE_SUP : ',1PE12.5)
 1401 FORMAT (3X,'LA PREMIERE CHARGE CRITIQUE INFERIEURE NON RETENUE'//
     +         'EST: ',1PE12.5)
 1501 FORMAT (3X,'LA PREMIERE CHARGE CRITIQUE SUPERIEURE NON RETENUE'//
     +        'EST: ',1PE12.5)

      END
