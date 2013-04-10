      SUBROUTINE LCOTAN(OPT,ANGMAS,ETATD,ETATF,FAMI,KPG,KSP,LOI,MOD,
     &                  IMAT,NMAT,MATERD,MATERF,EPSD,DEPS,SIGD,SIGF,
     &                  NVI,VIND,VINF,DRDY,
     &                  VP,VECP,THETA,DT,DEVG,DEVGII,
     &                  TIMED,TIMEF,COMP,NBCOMM,CPMONO,
     &                  PGL,NFS,NSG,TOUTMS,HSR,NR,ITMAX,TOLER,TYPMA,
     &                  DSDE,CODRET)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE PROIX J.M.PROIX
C TOLE CRP_21
C ======================================================================
C     
C     CALCUL DE L'OPERATEUR TANGENT = DS/DE(T+DT) OU DS/DE(T)
C     CONVENTION : 
C                 SUFFIXE D : DEBUT DU PAS DE TEMPS
C                 SUFFIXE F : FIN DU PAS DE TEMPS
C     ==================================================================
C     ARGUMENTS
C
C     IN FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C        KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C        IMAT    ADRESSE DU MATERIAU CODE
C        COMP    COMPORTEMENT DE L ELEMENT
C                COMP(1) = RELATION DE COMPORTEMENT (ROUSSELIER.)
C                COMP(2) = NB DE VARIABLES INTERNES
C                COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C        CRIT    CRITERES  LOCAUX
C                CRIT(1) = NOMBRE D ITERATIONS MAXI (ITER_INTE_MAXI)
C                CRIT(3) = TOLERANCE DE CONVERGENCE(RESI_INTE_RELA)
C                CRIT(4) = THETA
C                CRIT(5) = ITER_INTE_PAS (UTILISE PAR REDECE EN AMONT)
C                CRIT(6) = ALGO_INTE(NEWTON, NEWTON_PERT, NEWTON_RELI)
C        TIMED   INSTANT T
C        TIMEF   INSTANT T+DT
C        TEMPD   TEMPERATURE A T           POUR LA THM
C        TEMPF   TEMPERATURE A T+DT        POUR LA THM
C        TREF    TEMPERATURE DE REFERENCE  POUR LA THM
C        CES PARAMETRES DE TEMPERATURE NE SONT PAS PRIS EN COMPTE EN
C        MECANIQUE PURE (ON UTILISE LES VARIABLES DE COMMANDES)
C
C        EPSDT   DEFORMATION TOTALE A T
C        DEPST   INCREMENT DE DEFORMATION TOTALE
C        SIGD    CONTRAINTE A T
C        VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C        OPT     OPTION DE CALCUL
C                        'RIGI_MECA_TANG'> DSDE(T)
C                        'FULL_MECA'     > DSDE(T+DT), SIGF, VINF
C                        'RAPH_MECA'     > SIGF, VINF
C        ANGMAS  ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C                +  0 SI NAUTIQUIES OU 2 SI EULER
C                + LES 3 ANGLES D'EULER
C     OUT
C        SIGF    CONTRAINTE A T+DT
C        VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C        DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C        CODRET  CODE RETOUR =0 OK, =1 => REDECOUPAGE DU PAS DE TEMPS

      INTEGER      NMAT,NSG,NFS,NBCOMM(NMAT,3)
      
      CHARACTER*(*) FAMI
      CHARACTER*7   ETATD,ETATF
      CHARACTER*8   MOD,TYPMA
      CHARACTER*16  COMP(*),OPT,LOI
      CHARACTER*24  CPMONO(5*NMAT+1)
      
      INTEGER IMAT,NDT,NDI,NR,NVI,ITMAX,KPG,KSP,CODRET,K,L
      REAL*8 TOLER,MATERD(NMAT,2),MATERF(NMAT,2),ANGMAS(3)
      REAL*8 VIND(*),VINF(*),TIMED,TIMEF,EPSD(9),DEPS(9),SIGD(6),SIGF(6)
      REAL*8 THETA,DT,DEVG(6),DEVGII,VP(3),VECP(3,3),DSDE(6,*),PGL(3,3)
      REAL*8 TOUTMS(NFS,NSG,6),HSR(NSG,NSG),DRDY(*)
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------
C     OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
C     ----------------------------------------------------------------
C     EVALUATION DU JACOBIEN DSDE A (T+DT) POUR 'FULL_MECA'
C     ET CALCUL ELASTIQUE    ET   A (T)    POUR 'RIGI_MECA_TANG'
C     ----------------------------------------------------------------
C
      CODRET=0

C     MATRICE TANGENTE DE PREDICTION
      
      IF ( OPT(1:9) .EQ. 'RIGI_MECA' ) THEN
               
         IF ((LOI.EQ.'LAIGLE').OR.(LOI.EQ.'BETON_BURGER_FP')) THEN
            
            CALL LCJELA ( LOI  , MOD , NMAT, MATERD, VIND, DSDE)
            
         ELSEIF ((ETATD.EQ.'PLASTIC').AND.(LOI.EQ.'MONOCRISTAL')) THEN
         
                  CALL LCJPLC(LOI,MOD,ANGMAS,IMAT,NMAT,MATERF,
     &                        TIMED,TIMEF,COMP,NBCOMM,CPMONO,
     &                       PGL,NFS,NSG,TOUTMS,HSR,NR,NVI,EPSD,DEPS,
     &                        ITMAX,TOLER,SIGD,VIND,SIGD,VIND,
     &                        DSDE,DRDY,OPT,CODRET)
                  IF (CODRET.NE.0) GOTO 9999
               
         ELSEIF ((ETATD.EQ.'PLASTIC').AND.(TYPMA.EQ.'VITESSE ')) THEN
               IF ((LOI(1:10).EQ.'HOEK_BROWN').OR.
     &             (LOI(1:14).EQ.'HOEK_BROWN_EFF')) THEN
C ---              HOEK-BROWN : CALCUL DES VALEURS ET VECTEURS PROPRES
C ---                           DU DEVIATEUR ELASTIQUE
                  CALL LCHBVP(SIGD,VP,VECP)
               ENDIF
               CALL LCJPLA(FAMI,KPG,KSP,LOI,MOD,NR,IMAT,NMAT,MATERD,
     &                     NVI,DEPS,SIGD,VIND,DSDE,SIGD,VIND,
     &                     VP,VECP,THETA,DT,DEVG,DEVGII,CODRET)
               IF (CODRET.NE.0) GOTO 9999
            
         ELSE
         
C           CAS GENERAL : ELASTICITE LINEAIRE ISOTROPE OU ANISOTROPE
            CALL LCJELA ( LOI  , MOD , NMAT, MATERD, VIND, DSDE)
            
         ENDIF
C

      ELSEIF ( OPT(1:9) .EQ . 'FULL_MECA' ) THEN
      
         IF  ( ETATF .EQ. 'ELASTIC' ) THEN
           IF(LOI(1:15).EQ.'BETON_BURGER_FP')THEN
             CALL BURJPL(NMAT,MATERF,NR,DRDY,DSDE)
           ELSE
             CALL LCJELA ( LOI  , MOD , NMAT, MATERF, VINF, DSDE)
           ENDIF
           
         ELSEIF ( ETATF .EQ. 'PLASTIC' ) THEN
C   --->    ELASTOPLASTICITE ==>  TYPMA = 'VITESSE '
C   --->    VISCOPLASTICITE  ==>  TYPMA = 'COHERENT '
            IF     ( TYPMA .EQ. 'COHERENT' ) THEN
               CALL LCJPLC(LOI,MOD,ANGMAS,IMAT,NMAT,MATERF,
     &                     TIMED,TIMEF,COMP,NBCOMM,CPMONO,
     &                     PGL,NFS,NSG,TOUTMS,HSR,NR,NVI,EPSD,DEPS,
     &                     ITMAX,TOLER,SIGF,VINF,SIGD,VIND,
     &                     DSDE,DRDY,OPT,CODRET)
               IF (CODRET.NE.0) GOTO 9999
            ELSEIF ( TYPMA .EQ. 'VITESSE ' ) THEN
               CALL LCJPLA(FAMI,KPG,KSP,LOI,MOD,NR,IMAT,NMAT,MATERD,
     &                     NVI,DEPS,SIGF,VINF,DSDE,SIGD,VIND,
     &                     VP,VECP,THETA,DT,DEVG, DEVGII,CODRET)
               IF (CODRET.NE.0) GOTO 9999
            ENDIF
         ENDIF
C
      ENDIF
C
C -   MODIFICATION EN CONTRAINTE PLANES POUR TENIR COMPTE DE
C     SIG3=0 ET DE LA CONSERVATION DE L'ENERGIE
      IF ( MOD(1:6).EQ.'C_PLAN' )THEN
         DO 136 K=1,NDT
            IF (K.EQ.3) GO TO 136
            DO 137 L=1,NDT
               IF (L.EQ.3) GO TO 137
               DSDE(K,L) = DSDE(K,L)
     &                   - 1.D0/DSDE(3,3)*DSDE(K,3)*DSDE(3,L)
 137        CONTINUE
 136     CONTINUE
      ENDIF

 9999 CONTINUE
      END
