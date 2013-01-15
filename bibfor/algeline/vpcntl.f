      SUBROUTINE VPCNTL
     &  (CTY, MODE, OPTION, OMEMIN, OMEMAX, SEUIL, NFREQ, IPOS, LMAT,
     &   OMECOR, PRECDC, IER, VPINF, VPMAX, FREQ, ERR, CHARGE,
     &   TYPRES, NBLAGR, SOLVEU, NBRSSA, PRECSH)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/01/2013   AUTEUR BRIE N.BRIE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CONTROLE DE VALIDITE DES MODES TROUVES
C-----------------------------------------------------------------------
C IN CTY   : K1 : COMPORTEMENT EN CAS D'ERREUR ('A' OU 'F')
C IN MODE  : K* : TYPE DE RESULTAT
C IN OPTION: K* : TYPE DE CALCUL. SI ' ' ON NE FAIT PAS STURM
C IN OMEMIN/MAX: R8 : BORNES DE L'INTERVALLE
C IN SEUIL  : R8 : POUR TEST DE VALIDITE DES MODES (VERI_MODE/SEUIL)
C IN NFREQ : IN : NBRE DE FREQS (CHAR_CRITS) CALCULEES
C IN IPOS(*) : IN(*) : VECTEUR DE POSITIONS MODALES
C IN LMAT(*) : IN(*) : VECTEUR DES DESCRIPTEURS DES MATRICES DU PB
C            LMAT(1)  : MATRICE DE RAIDEUR
C            LMAT(2)  : MATRICE DE MASSE
C            LMAT(3)  : RESULTAT DE LA MATRICE SHIFTEE FACTORISEE
C IN OMECOR : R8 : VALEUR MINIMALE ADMISSIBLE (SEUIL_FREQ)
C IN PRECDC : R8 : POURCENTAGE DE DECALAGE (VERI_MODE/PREC_SHIFT)
C IN NBRSSA : IN : NBRE DE DECALAGES ADMISSIBLES (NMAX_ITER_SHIFT)
C OUT IER   : IN : CODE RETOUR
C            0 TOUT C'EST BIEN PASSE
C            > 0 NOMBRE D'ERREURS TROUVEES
C IN VPINF/MAX: R8 : REDONDANT AVEC OMEMIN/MAX ?
C IN FREQ(*)/CHARGE(*)/ERR(*): R8(*) : LISTE DES FREQS/CHAR_CRITS ET
C            DES ERREURS ASSOCIEES
C IN TYPRES: K* : TYPE DE RESULTAT
C IN NBLAGR: IN : NBRE DE LAGRANGES
C IN SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C IN PRECSH  : R8 : POURCENTAGE DE DECALAGE (CALC_FREQ/PREC_SHIFT)
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'

      INTEGER       NFREQ,IPOS(*),LMAT(3),IER,NBLAGR,NBRSSA,IBID2(2)
      REAL*8        VPINF,VPMAX,OMEMIN,OMEMAX,SEUIL,PRECDC,OMECOR,
     &              CHARGE(NFREQ),FREQ(NFREQ),ERR(NFREQ),PRECSH
      CHARACTER*1   CTY
      CHARACTER*16  K16B, NOMCMD
      CHARACTER*19  SOLVEU
      CHARACTER*24  VALK
      CHARACTER*(*) MODE,OPTION,TYPRES

C     ------------------------------------------------------------------
      REAL*8  ZMIN,ZMAX,FREQOM,OMEGA2,OMEGA,VALR(2),RBID,DET(2)
      INTEGER IFM,NIV,IFREQ,NFREQT,VALI(2),IDET(2)
C     ------------------------------------------------------------------
      IER    = 0

C     ---RECUPERATION DU NIVEAU D'IMPRESSION----
      CALL INFNIV(IFM,NIV)
      IF (NIV.GE.1) THEN
        WRITE(IFM,1000)
        WRITE(IFM,1100)
        WRITE(IFM,1200)
      ENDIF

C     ------------------------------------------------------------------
C     ------------------ CONTROLE DES NORMES D'ERREURS -----------------
C     ------------------------------------------------------------------

      IF ( SEUIL .GT. 0.0D0 ) THEN

         DO 100 IFREQ = 1, NFREQ
            IF ( ERR(IFREQ) .GT. SEUIL ) THEN
               IER = IER + 1
               VALK = MODE
               VALI (1) = IPOS(IFREQ)
               CALL U2MESG(CTY//'+','ALGELINE5_15',1,VALK,1,VALI,0,0.D0)
               IF (TYPRES .EQ. 'DYNAMIQUE' ) THEN
                 VALR (1) = FREQ(IFREQ)
                 CALL U2MESR(CTY//'+','ALGELINE5_16',1,VALR)
               ELSE
                 VALR (1) = CHARGE(IFREQ)
                 CALL U2MESR(CTY//'+','ALGELINE5_17',1,VALR)
               ENDIF
              VALR (1) = ERR(IFREQ)
              VALR (2) = SEUIL
              CALL U2MESR(CTY//'+','ALGELINE5_18',2,VALR)

              CALL GETRES(K16B, K16B, NOMCMD)
              IF (TYPRES .EQ. 'DYNAMIQUE')  THEN
                VALK = 'FREQ'
              ELSE
                VALK = 'CHAR_CRIT'
              ENDIF
              IF (NOMCMD(1:16) .EQ. 'MODE_ITER_SIMULT')  THEN
                CALL U2MESK(CTY,'ALGELINE5_77',1,'NMAX_'//VALK)
              ELSE
                CALL U2MESK(CTY,'ALGELINE5_78',1,'CALC_'//VALK)
              ENDIF

            ENDIF
 100     CONTINUE
      ENDIF
C     ------------------------------------------------------------------
C     -- OPTION BANDE :                                              ---
C     -- VERIFICATION QUE LES FREQUENCES TROUVEES SONT DANS LA BANDE ---
C     ------------------------------------------------------------------
      IF ( OPTION .EQ. 'BANDE' ) THEN
         ZMAX = (1.D0 + SIGN(PRECDC,OMEMAX)) * OMEMAX
         ZMIN = (1.D0 - SIGN(PRECDC,OMEMIN)) * OMEMIN
         IF (ABS(OMEMIN).LE.OMECOR) ZMIN = - OMECOR
         DO 210 IFREQ = 1, NFREQ
            IF (TYPRES .EQ. 'DYNAMIQUE') THEN
              OMEGA = OMEGA2(FREQ(IFREQ))
            ELSE
              OMEGA = CHARGE(IFREQ)
            ENDIF
            IF ( OMEGA .LT.ZMIN .OR. OMEGA .GT. ZMAX ) THEN
               IER = IER + 1
               VALK = MODE
               VALI (1) = IPOS(IFREQ)
               CALL U2MESG(CTY//'+','ALGELINE5_15',1,VALK,1,VALI,0,0.D0)
               IF (TYPRES .EQ. 'DYNAMIQUE' ) THEN
                 VALR (1) = FREQ(IFREQ)
                 CALL U2MESR(CTY//'+','ALGELINE5_16',1,VALR(1))
                 VALR (1) = FREQOM(OMEMIN)
                 VALR (2) = FREQOM(OMEMAX)
                 CALL U2MESR(CTY,'ALGELINE5_20',2,VALR)
               ELSE
                 VALR (1) = CHARGE(IFREQ)
                 CALL U2MESR(CTY//'+','ALGELINE5_17',1,VALR(1))
                 VALR (1) = OMEMIN
                 VALR (2) = OMEMAX
                 CALL U2MESR(CTY,'ALGELINE5_20',2,VALR)
               ENDIF
            ENDIF
 210     CONTINUE
      ENDIF

C     ------------------------------------------------------------------
C     -- POUR TOUTES LES OPTIONS :                                   ---
C     -- VERIFICATION QU'ON A LE BON NOMBRE DE FREQUENCES            ---
C     ------------------------------------------------------------------

C        --- RECHERCHE DE LA PLUS PETITE ET DE LA PLUS GRANDE FREQUENCES

      IF ( OPTION .NE. ' ' ) THEN

C --- POUR OPTIMISER ON NE CALCULE PAS LE DET, ON NE GARDE PAS LA FACTO
C --- (SI MUMPS)
         K16B=TYPRES
         CALL VPFOPR('STURM',K16B,LMAT(2),LMAT(1),LMAT(3),VPINF,VPMAX,
     &               RBID,NFREQT,IBID2,OMECOR,PRECSH,NBRSSA,NBLAGR,
     &               SOLVEU,DET,IDET)

         IF (NFREQT.NE.NFREQ) THEN
           IER = IER + 1
           VALK = MODE
           CALL U2MESK(CTY//'+','ALGELINE5_23',1,VALK)
           IF (TYPRES .EQ. 'DYNAMIQUE') THEN
             VALR (1) = FREQOM(VPINF)
             VALR (2) = FREQOM(VPMAX)
             VALI (1) = NFREQT
             VALI (2) = NFREQ
             CALL U2MESG(CTY//'+','ALGELINE5_24',0,' ',2,VALI,2,VALR)
           ELSE
             VALR (1) = VPINF
             VALR (2) = VPMAX
             VALI (1) = NFREQT
             VALI (2) = NFREQ
             CALL U2MESG(CTY//'+','ALGELINE5_25',0,' ',2,VALI,2,VALR)
           ENDIF
           CALL U2MESS(CTY,'ALGELINE5_26')
         ELSE
           IF (NIV.GE.1) THEN
             IF (TYPRES .EQ. 'DYNAMIQUE') THEN
               WRITE(IFM,1300) FREQOM(VPINF), FREQOM(VPMAX)
               WRITE(IFM,1400) NFREQT
             ELSE
               WRITE(IFM,1300) VPINF, VPMAX
               WRITE(IFM,1401) NFREQT
             ENDIF
           ENDIF
         ENDIF
      ENDIF
      IF (NIV.GE.1) WRITE(IFM,1000)

 1000 FORMAT (72('-'),/)
 1100 FORMAT (10X,'VERIFICATION A POSTERIORI DES MODES')
 1200 FORMAT (3X)
 1300 FORMAT (3X,'DANS L''INTERVALLE (',1PE12.5,',',1PE12.5,') ')
 1400 FORMAT (3X,'IL Y A BIEN ',I4,' FREQUENCE(S) ')
 1401 FORMAT (3X,'IL Y A BIEN ',I4,' CHARGE(S) CRITIQUE(S) ')

      END
