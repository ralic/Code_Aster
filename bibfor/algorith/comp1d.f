      SUBROUTINE COMP1D(OPTION,SIGM,EPS,DEPS,TREF,TEMPM,
     +                  TEMPP,VIM,VIP,SIGP,DSIDEP)
      IMPLICIT NONE
      CHARACTER*16   OPTION
      REAL*8         TEMPM,TEMPP,TREF
      REAL*8         VIM(*),VIP(*),SIGM(6),SIGP(*),EPS(6),DEPS(6)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/04/2002   AUTEUR JMBHH01 J.M.PROIX 
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
C
C     INTEGRATION DE LOIS DE COMPORTEMENT NON LINEAIRES 
C     POUR DES ELEMENTS DE BARRE 
C
C ----------------------------------------------------------------------
C IN  : OPTION    : NOM DE L'OPTION A CALCULER 
C IN  : DLONG     : INCREMENT D'ALLONGEMENT
C IN  : UML       : DEPLACEMENT
C IN  : XLONG0    : LONGUEUR DE REFERENCE DE LA BARRRE
C IN  : A         : SECTION DE LA BARRE
C IN  : TREF      : TEMPERATURE DE REFERENCE
C IN  : EFFNOM    : EFFORT NORMAL A L'INSTANT MOINS
C IN  : TEMPM     : TEMPERATURE A L'INSTANT MOINS
C IN  : TEMPP     : TEMPERATURE A L'INSTANT PLUS
C IN  : VIM       : VARIABLES INTERNES A L'INSTANT MOINS
C OUT : VIP       : VARIABLES INTERNES A L'INSTANT PLUS
C OUT : SIGP      : CONTRAINTES A L'INSTANT PLUS
C OUT : KLV       : VECTEUR DES FORCES NODALES
C OUT : FONO      : MATRICE TANGENTE
C ----------------------------------------------------------------------
C
C **************** DEBUT COMMUNS NORMALISES JEVEUX *********************
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                             ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C
C ***************** FIN COMMUNS NORMALISES JEVEUX **********************
C
C
C *************** DECLARATION DES VARIABLES LOCALES ********************
C
      INTEGER        NEQ,NVAMAX,IMATE,IINSTM
      INTEGER        IINSTP,ICOMPO,ICARCR,NZ
      INTEGER        I,J,CODRET,NZMAX,ICOMPT,ITER      
C
      REAL*8         DSIDEP(6,6)
      REAL*8         ZERO,UN,DEUX,XM,XI,XS,ALPH,SIGYI,SIGYS
      REAL*8         HYDRGM,HYDRGP,SECHGM,SECHGP
      REAL*8         EPSANM(6),EPSANP(6),PHASM(7),PHASP(7)
      REAL*8         LC(10,27),SIGMAY
      REAL*8         PREC,R8PREM,R8MIEM,NU,ALPHA,SIGXI,SIGXS
C
      LOGICAL        VECTEU
C      
      CHARACTER*8    TYPMOD(2)
C
C *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
C
C ********************* DEBUT DE LA SUBROUTINE *************************
C
C ---    INITIALISATIONS :
         NEQ  = 6
         PREC = R8PREM()
         ZERO = 0.0D0
         UN = 1.0D0
         DEUX = 2.0D0
         VECTEU = ((OPTION(1:9).EQ.'FULL_MECA') .OR. 
     +          (OPTION(1:9).EQ.'RAPH_MECA'))

         TYPMOD(1) = 'C_PLAN  '
         TYPMOD(2) = '        '
C
C ---    PARAMETRES EN ENTREE
C
         CALL JEVECH ('PMATERC','L',IMATE)
         CALL JEVECH ('PINSTMR','L',IINSTM)
         CALL JEVECH ('PINSTPR','L',IINSTP)
         CALL JEVECH ('PCOMPOR','L',ICOMPO)
         CALL JEVECH ('PCARCRI','L',ICARCR)
C
C ---    INITIALISATION DES TABLEAUX
C
         HYDRGM = ZERO
         HYDRGP = ZERO
         SECHGM = ZERO
         SECHGP = ZERO
C
         CALL R8INIR (NEQ,ZERO,EPSANM,1)
C
         CALL R8INIR (NEQ,ZERO,EPSANP,1)
C
         NZMAX = 7
         CALL R8INIR (NZMAX,ZERO,PHASM,1)
         CALL R8INIR (NZMAX,ZERO,PHASP,1) 
         NZ = NZMAX        
C
         DO 40 I = 1,10
            DO 50 J = 1,27
              LC(I,J) = ZERO
  50        CONTINUE          
  40     CONTINUE
C  
         DO 60 I = 1,NEQ
           DO 70 J = 1,NEQ
             DSIDEP(I,J) = ZERO
  70        CONTINUE          
  60      CONTINUE    
C         
C
         ITER = 0
         ICOMPT = 0
         ALPH = UN
C
C ---    INITIALISATION DE LA DICHOTOMIE
C
  90     CONTINUE       
C
         IF (VECTEU) THEN
            IF (ABS(DEPS(2)) .LE. R8MIEM()) THEN
             CALL UTMESS('F','TE0248 COMP1D','INITIALISATION DE '//
     &                 ' LA DICHOTOMIE IMPOSSIBLE - DEPLACEMENT '//
     &                 ' EN Y NUL') 
            ELSEIF (DEPS(2).GT.ZERO) THEN
               XI = -0.99D0*DEPS(2)
               XM = XI
               XS = DEPS(2)
            ELSE
               XI = DEPS(2)
               XM = XI
               XS = -0.99D0*DEPS(2)
            ENDIF 
         ELSEIF (OPTION.EQ.'RIGI_MECA_TANG') THEN
            XI=0.D0
         ENDIF 
C
C ---    TEST DU CHANGEMENT DE SIGNE DE SIGY SUR L'INTERVALLE
C ---    (XI,XS) :
C ---    ON ELARGIT LE DOMAINE EXPLORE POUR LA DICHOTOMIE DE ALPH 
C ---    SI PAS DE CHANGEMENT DE SIGNE
C
         DEPS(2) = XI  
         CALL NMCOMP(2,TYPMOD,ZI(IMATE),ZK16(ICOMPO),ZR(ICARCR),
     +               ZR(IINSTM),ZR(IINSTP),TEMPM,TEMPP,TREF,HYDRGM,
     +               HYDRGP,SECHGM,SECHGP,EPS,DEPS,SIGM,
     +               VIM,OPTION,EPSANM,EPSANP,NZ,PHASM,
     +               PHASP,LC,SIGP,VIP,DSIDEP,
     +               CODRET)
         IF (OPTION.EQ.'RIGI_MECA_TANG') GOTO 9999
         SIGYI = SIGP(2) 
C
         DEPS(2) = XS         
         CALL NMCOMP(2,TYPMOD,ZI(IMATE),ZK16(ICOMPO),ZR(ICARCR),
     +               ZR(IINSTM),ZR(IINSTP),TEMPM,TEMPP,TREF,HYDRGM,
     +               HYDRGP,SECHGM,SECHGP,EPS,DEPS,SIGM,
     +               VIM,OPTION,EPSANM,EPSANP,NZ,PHASM,
     +               PHASP,LC,SIGP,VIP,DSIDEP,
     +               CODRET)
         SIGYS = SIGP(2) 
C
         IF ((SIGYI*SIGYS) .GT. ZERO) THEN
            ALPH = ALPH*100.0D0
            DEPS(2) = -ALPH*(EPS(2)+DEPS(2))            
            ICOMPT = ICOMPT + 1
            IF (ICOMPT .GT. 100) THEN 
               CALL UTMESS('F','TE0248 - COMP1D','INTERVALLE DE '//
     &                     ' DICHOTOMIE NON TROUVE')
            ENDIF   
C            WRITE(6,*) 'ON ELARGIT LE DOMAINE EXPLORE DE: ',ALPH
            GO TO 90
         ENDIF
C               
 100     CONTINUE 
C
C ---    DEBUT DES ITERATIONS DE DICHOTOMIE
C
         ITER  = ITER+1
         DEPS(2) = XM
C       
         CALL NMCOMP(2,TYPMOD,ZI(IMATE),ZK16(ICOMPO),ZR(ICARCR),
     +               ZR(IINSTM),ZR(IINSTP),TEMPM,TEMPP,TREF,HYDRGM,
     +               HYDRGP,SECHGM,SECHGP,EPS,DEPS,SIGM,
     +               VIM,OPTION,EPSANM,EPSANP,NZ,PHASM,
     +               PHASP,LC,SIGP,VIP,DSIDEP,
     +               CODRET)
C
         IF(VECTEU) THEN
C           
C
           IF(ITER.EQ.1) THEN
             SIGMAY =  SIGP(2)             
             IF(ABS(SIGP(2)) .LT. (PREC*ABS(SIGP(1)))) GOTO 110
             XM = (XS + XI)/DEUX
             GOTO 100
           ELSE             
             IF(ABS(SIGP(2)) .LT. (PREC*ABS(SIGP(1)))) GOTO 110
             IF(ABS(XS-XI).LT.1.D-30) GOTO 110
             IF ((XS*XI) .GT. ZERO) THEN
               IF(ABS((XS-XI)/(XS+XI)).LT.R8PREM()) GOTO 110
             ENDIF  
           ENDIF
           IF(ITER.GT.1000) THEN
             WRITE(6,*)'PB CONVERGENCE CONTRAINTE PLANE'
             WRITE(6,*) ITER
             WRITE(6,*)'SIGX',SIGP(1)
             WRITE(6,*)'SIGY',SIGP(2)
             WRITE(6,*)'SIGZ',SIGP(3)
             CALL UTMESS('A','TE0248 - COMP1D','NBRE MAXI '//
     &                   'D''ITERATION ATTEINT')
             GOTO 110
           ENDIF
C           
           IF(SIGMAY*SIGP(2).GT.ZERO) THEN
             XI = XM
             XM = (XS+XI)/DEUX
           ELSE
             XS = XM
             XM = (XS+XI)/DEUX
           ENDIF
           GOTO 100
C
         ENDIF
C         
 110     CONTINUE
C
C ---    FIN DE LA DICHOTOMIE
C         
C
9999  CONTINUE
      END
