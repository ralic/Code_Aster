      SUBROUTINE POIBIJ(NPV,VABS,GEOM,FSVR,NBM,VICOQ,TORCO,TCOEF,FREQ,
     &                  IMASSE,MAJ,VECPR)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
C CALCUL D'UN CRITERE DE POIDS DES TERMES EXTRADIAGONAUX DE LA MATRICE
C B(S) PAR RAPPORT AUX TERMES DIAGONAUX
C IMPRESSION DU CRITERE DANS LE FICHIER MESSAGE
C APPELANT : FLUST4
C-----------------------------------------------------------------------
C  IN : NPV    : NOMBRE DE VITESSES D'ECOULEMENT
C  IN : VABS   : VECTEUR DES VALEURS ABSOLUES DES VITESSES D'ECOULEMENT
C                (VITESSES DE L'ECOULEMENT MOYEN)
C  IN : GEOM   : VECTEUR DE GRANDEURS GEOMETRIQUES CARACTERISTIQUES
C  IN : FSVR   : OBJET .FSVR DU CONCEPT TYPE_FLUI_STRU
C  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
C  IN : VICOQ  : VECTEUR D'INDICES CARACTERISANT LA COQUE EN MOUVEMENT
C                POUR CHAQUE MODE
C                VICOQ(IMOD)=1 COQUE INTERNE EN MVT POUR LE MODE IMOD
C                VICOQ(IMOD)=2 COQUE EXTERNE EN MVT
C                VICOQ(IMOD)=3 COQUES INTERNE + EXTERNE EN MVT
C  IN : TORCO  : TABLEAU DES ORDRES DE COQUE ET DEPHASAGES
C  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
C  IN : FREQ   : LISTE DES FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX
C                PERTURBES PAR L'ECOULEMENT
C  IN : IMASSE : INDICE CARACTERISTIQUE DU CAS DE CALCUL
C                IMASSE=0  EFFETS DE MASSE AJOUTEE NON PRIS EN COMPTE
C                IMASSE=1  EFFETS DE MASSE AJOUTEE PRIS EN COMPTE
C  IN : MAJ    : MASSES AJOUTEES PAR LE FLUIDE (DANS LA BASE EN EAU)
C  IN : VECPR  : SI IMASSE=0 : INUTILE
C                SI IMASSE=1 : VECTEURS PROPRES DES MODES EN EAU
C                              DECOMPOSES SUR LA BASE DES MODES EN AIR
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER     NPV
      REAL*8      VABS(NPV),GEOM(9),FSVR(7)
      INTEGER     NBM,VICOQ(NBM)
      REAL*8      TORCO(4,NBM),TCOEF(10,NBM),FREQ(2*NBM*NPV)
      INTEGER     IMASSE
      REAL*8      MAJ(NBM),VECPR(*)
C
      REAL*8      R8PI
      REAL*8      MCF0,KSI
      COMPLEX*16  S
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C
C-----1.INITIALISATIONS ET FORMATS D'IMPRESSION
C
      PI = R8PI()
      HMOY = GEOM(1)
      VISC = FSVR(2)
      RUG  = FSVR(3)
C
      IFM = IUNIFI('MESSAGE')
C
 500  FORMAT('********************************************************')
 501  FORMAT('*                                                      *')
 502  FORMAT('* MATRICE DE TRANSFERT DES FORCES FLUIDELASTIQUES B(S) *')
 503  FORMAT('*         DIMINUEE DES TERMES DE MASSE AJOUTEE         *')
 504  FORMAT('*  CALCUL DU POIDS RELATIF DES TERMES EXTRADIAGONAUX   *')
 505  FORMAT('*           PAR RAPPORT AUX TERMES DIAGONAUX           *')
 510  FORMAT('VITESSE D ECOULEMENT NO ',I3)
 511  FORMAT('===========================')
 512  FORMAT('VITESSE D ECOULEMENT NULLE : LA MATRICE DE TRANSFERT ',
     &       'DES FORCES FLUIDELASTIQUES')
 513  FORMAT('EST EGALE A LA MATRICE DE MASSE AJOUTEE. TOUS LES ',
     &       'TERMES RESIDUELS SONT NULS')
 514  FORMAT('N.B. TOUS LES TERMES DE LA MATRICE DE MASSE AJOUTEE ',
     &       'ONT ETE PRIS EN COMPTE POUR')
 515  FORMAT('LE CALCUL DES MODES DE LA STRUCTURE EN EAU AU REPOS')
 520  FORMAT('FREQUENCE NO ',I3,' NON DETERMINEE')
 521  FORMAT('FREQUENCE SOLUTION NO ',I3,3X,'POIDS RELATIF DES BIJ : ',
     &        G13.6,' %')
 530  FORMAT(30X,'---/---')
C
C
C-----2.CALCUL DU CRITERE
C
      CALL WKVECT('&&POIBIJ.TEMP.MATB','V V C',NBM*NBM,IMATB)
C
      WRITE(IFM,500)
      WRITE(IFM,501)
      WRITE(IFM,502)
      WRITE(IFM,503)
      WRITE(IFM,501)
      WRITE(IFM,504)
      WRITE(IFM,505)
      WRITE(IFM,501)
      WRITE(IFM,500)
      WRITE(IFM,*)
C
C-----2.1.SI ON TRAVAILLE DIRECTEMENT DANS LA BASE MODALE EN EAU AU
C         REPOS DU SYSTEME : LES TERMES RESIDUELS SONT DONNES PAR
C                                           2
C                      R(S) = B(S) + MAJ * S
C
      IF (IMASSE.EQ.0) THEN
C
        DO 10 IV = 1,NPV
C
          WRITE(IFM,510) IV
          WRITE(IFM,511)
          WRITE(IFM,*)
          UMOY = VABS(IV)
C
C---------2.1.1.CAS VITESSE NULLE : B(S) = -MAJ => TERMES RESIDUELS NULS
C
          IF (UMOY .LT. 1.D-5) THEN
C
            WRITE(IFM,512)
            WRITE(IFM,513)
            WRITE(IFM,*)
            WRITE(IFM,514)
            WRITE(IFM,515)
C
C---------2.1.2.CAS VITESSE NON NULLE
C
          ELSE
C
            CALL CFROTT(VISC,RUG,HMOY,UMOY,CF0,MCF0)
C
            DO 20 IMOD = 1,NBM
C
              FI  = FREQ(2*NBM*(IV-1)+2*(IMOD-1)+1)
              KSI = FREQ(2*NBM*(IV-1)+2*(IMOD-1)+2)
C
              IF (FI.LT.0.D0 .OR. KSI.GT.1.D0) THEN
C
                WRITE(IFM,520) IMOD
C
              ELSE
C
                OMEGAI = 2.D0*PI*FI
                S1 = -1.D0*OMEGAI*KSI
                S2 = OMEGAI*DBLE(SQRT(1.D0-KSI*KSI))
                S = DCMPLX(S1,S2)
C
                CALL BMOCCA(UMOY,GEOM,CF0,MCF0,FSVR,NBM,VICOQ,TORCO,
     &                      TCOEF,S1,S2,ZC(IMATB))
C                                     2
C---------------B(S) -> B(S) + MAJ * S 
C
                DO 30 J = 1,NBM
                  ZC(IMATB+NBM*(J-1)+J-1) = ZC(IMATB+NBM*(J-1)+J-1)
     &                                    + DCMPLX(MAJ(J),0.D0)*S*S
  30            CONTINUE
C
                CALL CRIPOI(NBM,ZC(IMATB),CRIT)
                WRITE(IFM,521) IMOD,CRIT
C
              ENDIF
C
  20        CONTINUE
C
          ENDIF
C
          WRITE(IFM,*)
          WRITE(IFM,530)
          WRITE(IFM,*)
C
  10    CONTINUE
C
C-----2.2.SINON : ON DOIT PROJETER B(S) SUR LA BASE EN EAU AU REPOS DU
C         SYSTEME. LES TERMES RESIDUELS SONT DONNES PAR
C                        T                          2
C                 R(S) =  PHI * B(S) * PHI + MAJ * S
C
      ELSE
C
        CALL WKVECT('&&POIBIJ.TEMP.MAT2','V V C',NBM*NBM,IMAT2)
C
        DO 110 IV = 1,NPV
C
          WRITE(IFM,510) IV
          WRITE(IFM,511)
          WRITE(IFM,*)
          UMOY = VABS(IV)
C                                                       T
C---------2.2.1.CAS VITESSE NULLE : B(S) = - PHI * MAJ * PHI
C               => TERMES RESIDUELS NULS
C
          IF (UMOY .LT. 1.D-5) THEN
C
            WRITE(IFM,512)
            WRITE(IFM,513)
            WRITE(IFM,*)
            WRITE(IFM,514)
            WRITE(IFM,515)
C
C---------2.2.2.CAS VITESSE NON NULLE
C
          ELSE
C
            CALL CFROTT(VISC,RUG,HMOY,UMOY,CF0,MCF0)
C
            DO 120 IMOD = 1,NBM
C
              FI  = FREQ(2*NBM*(IV-1)+2*(IMOD-1)+1)
              KSI = FREQ(2*NBM*(IV-1)+2*(IMOD-1)+2)
C
              IF (FI.LT.0.D0 .OR. KSI.GT.1.D0) THEN
C
                WRITE(IFM,520) IMOD
C
              ELSE
C
                OMEGAI = 2.D0*PI*FI
                S1 = -1.D0*OMEGAI*KSI
                S2 = OMEGAI*DBLE(SQRT(1.D0-KSI*KSI))
                S = DCMPLX(S1,S2)
C
                CALL BMOCCA(UMOY,GEOM,CF0,MCF0,FSVR,NBM,VICOQ,TORCO,
     &                      TCOEF,S1,S2,ZC(IMATB))
C                *
C---------------B (S) = B(S) * PHI
C
                DO 130 J = 1,NBM
                  DO 131 I = 1,NBM
                    ZC(IMAT2+NBM*(J-1)+I-1) = DCMPLX(0.D0,0.D0)
                    DO 132 K = 1,NBM
                      ZC(IMAT2+NBM*(J-1)+I-1) = ZC(IMAT2+NBM*(J-1)+I-1)
     &                  + ZC(IMATB+NBM*(K-1)+I-1)
     &                    * DCMPLX(VECPR(NBM*(J-1)+K),0.D0)
 132                CONTINUE
 131              CONTINUE
 130            CONTINUE
C                       T       *
C---------------B(S) ->  PHI * B (S)
C
                DO 140 J = 1,NBM
                  DO 141 I = 1,NBM
                    ZC(IMATB+NBM*(J-1)+I-1) = DCMPLX(0.D0,0.D0)
                    DO 142 K = 1,NBM
                      ZC(IMATB+NBM*(J-1)+I-1) = ZC(IMATB+NBM*(J-1)+I-1)
     &                  + DCMPLX(VECPR(NBM*(I-1)+K),0.D0)
     &                    * ZC(IMAT2+NBM*(J-1)+K-1)
 142                CONTINUE
 141              CONTINUE
 140            CONTINUE
C                                     2
C---------------B(S) -> B(S) + MAJ * S 
C
                DO 150 J = 1,NBM
                  ZC(IMATB+NBM*(J-1)+J-1) = ZC(IMATB+NBM*(J-1)+J-1)
     &                                    + DCMPLX(MAJ(J),0.D0)*S*S
 150            CONTINUE
C
                CALL CRIPOI(NBM,ZC(IMATB),CRIT)
                WRITE(IFM,521) IMOD,CRIT
C
              ENDIF
C
 120        CONTINUE
C
          ENDIF
C
          WRITE(IFM,*)
          WRITE(IFM,530)
          WRITE(IFM,*)
C
 110    CONTINUE
C
      ENDIF
C
      CALL JEDETC('V','&&POIBIJ',1)
      CALL JEDEMA()
      END
