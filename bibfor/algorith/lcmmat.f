        SUBROUTINE LCMMAT (COMP, MOD,  IMAT,   NMAT,   TEMPD,   TEMPF,
     &    PGL,MATERD,MATERF, MATCST,NBCOMM,CPMONO,NDT, NDI,NR , NVI)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR JMBHH01 J.M.PROIX 
C RESPONSABLE JMBHH01 J.M.PROIX
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                    MATER(*,1) = E , NU , ALPHA
C                    MATER(*,2) = COEFFICIENT DE CHAQUE COMPORTEMENT
C                    VARIABLES INTERNES : 
C                     EPSVP(6)+ALPHA,GAMMA,P PAR SYSTEME DE GLISSEMENT
C       ----------------------------------------------------------------
C       IN  COMP   :  GRANDEUR COMPOR
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  MAXIMUM DE MATER
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           PGL    : MATRICE DE PASSAGE GLOBAL LOCAL
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON
C           NBCOMM : POSITION DES COEF POUR CHAQUE LOI DE CHAQUE SYSTEME
C           CPMONO : NOMS DES LOIS POUR CHAQUE FAMILLE DE SYSTEME
C
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C     ----------------------------------------------------------------
      INTEGER         NMAT, NDT , NDI  , NR , NVI,NBCOMM(NMAT,3)
      REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2) , TEMPD , TEMPF
      REAL*8          HYDRD , HYDRF , SECHD , SECHF,R8DGRD
      REAL*8          VALPAD(3), VALPAF(3)
      REAL*8          EPSI,R8PREM,REPERE(3),PGL(3,3)
      CHARACTER*8     MOD, NOM , NOMC(14) , NOMPAR(3)
      CHARACTER*2     BL2, CERR(14)
      CHARACTER*3     MATCST
      CHARACTER*16    COMP(*),NMATER,NECOUL,NECRIS,NECRCI
      CHARACTER*16    CPMONO(5*NMAT+1),PHENOM
      INTEGER I, ICOMPO, IMAT, NBFSYS, IFA,J,ICAMAS,IRET,ITAB(8)
C     ----------------------------------------------------------------
C
C -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
C
      CALL JEMARQ()
      
      BL2 = '  '
      IF      (MOD(1:2).EQ.'3D')THEN
         NDT = 6
         NDI = 3
      ELSE IF (MOD(1:6).EQ.'D_PLAN'.OR.MOD(1:4).EQ.'AXIS')THEN
         NDT = 4
         NDI = 3
      ELSE IF (MOD(1:6).EQ.'C_PLAN')THEN
         NDT = 4
         NDI = 3
      ENDIF
            
      NOMPAR(1) = 'TEMP'
      VALPAD(1) = TEMPD
      VALPAF(1) = TEMPF
C
      READ (COMP(2),'(I16)') NVI
      READ (COMP(7),'(I16)') NBFSYS
      CALL JEVEUO(COMP(6),'L',ICOMPO)
C     LA DERNIERE VARIABLE INTERNE EST L'INDICATEUR PLASTIQUE
C           
      NR=NVI+NDT-1
      
C     CET APPEL SERA A DEPLACER DANS NMPL3D, NMPL2D (UNE FOIS PAR PG)
      CALL TECACH('NNN','PCAMASS',1,ITAB,IRET)
      
      DO 10 I = 1, 3
         REPERE(I) = 0.D0
 10   CONTINUE
      IF (IRET.EQ.0) THEN
         ICAMAS=ITAB(1)
         IF (ZR(ICAMAS).GT.0.D0) THEN
C
C ----      ANGLES NAUTIQUES
C           ----------------
            REPERE(1) = ZR(ICAMAS+1)*R8DGRD()
            REPERE(2) = ZR(ICAMAS+2)*R8DGRD()
            REPERE(3) = ZR(ICAMAS+3)*R8DGRD()
         ELSE
            CALL UTMESS('F','LCMMAT','DEFINIR 3 ANGLES')
         ENDIF
      ENDIF
      CALL MATROT(REPERE,PGL)
      
      DO 111 I=1,NMAT
      DO 111 J=1,3
         NBCOMM(I,J)=0
 111  CONTINUE
      NBCOMM(1,1)=1
      
      DO 112 I=1,5*NBFSYS
         CPMONO(I)=ZK16(ICOMPO-1+I)
 112  CONTINUE
      CPMONO(5*NBFSYS+1)=ZK16(ICOMPO-1+5*NBFSYS+1)
 
      NBCOMM(1,1)=1
      
      DO 6 IFA=1,NBFSYS
      
         NMATER=CPMONO(5*(IFA-1)+2)
         NECOUL=CPMONO(5*(IFA-1)+3) 
         NECRIS=CPMONO(5*(IFA-1)+4) 
         NECRCI=CPMONO(5*(IFA-1)+5) 

C     COEFFICIENTS MATERIAUX LIES A L'ECOULEMENT         
      CALL LCMAFL(NMATER,IMAT,NECOUL,IFA,NBCOMM,1,NOMPAR,VALPAD,MATERD,
     &            NMAT)
C     COEFFICIENTS MATERIAUX LIES A L'ECROUISSAGE CINEMATIQUE        
      CALL LCMAEC(NMATER,IMAT,NECRCI,IFA,NBCOMM,1,NOMPAR,VALPAD,MATERD,
     &            NMAT)
C     COEFFICIENTS MATERIAUX LIES A L'ECROUISSAGE ISOTROPE
      CALL LCMAEI(NMATER,IMAT,NECRIS,IFA,NBCOMM,1,NOMPAR,VALPAD,MATERD,
     &            NMAT)
 6    CONTINUE     
C     ON STOCKE A LA FIN LE NOMBRE TOTAL DE COEF MATERIAU    
      NBCOMM(NMAT,2)=NBFSYS
      NBCOMM(NMAT,3)=NBCOMM(NBFSYS+1,1)+1
      NBCOMM(1,1)=1
      
      DO 61 IFA=1,NBFSYS
      
         NMATER=ZK16(ICOMPO-1+5*(IFA-1)+2)
         NECOUL=ZK16(ICOMPO-1+5*(IFA-1)+3) 
         NECRIS=ZK16(ICOMPO-1+5*(IFA-1)+4) 
         NECRCI=ZK16(ICOMPO-1+5*(IFA-1)+5) 
         
      
      CALL LCMAFL(NMATER,IMAT,NECOUL,IFA,NBCOMM,1,NOMPAR,VALPAF,MATERF,
     &            NMAT)
      CALL LCMAEC(NMATER,IMAT,NECRCI,IFA,NBCOMM,1,NOMPAR,VALPAF,MATERF,
     &            NMAT)
      CALL LCMAEI(NMATER,IMAT,NECRIS,IFA,NBCOMM,1,NOMPAR,VALPAF,MATERF,
     &            NMAT)
         
 61    CONTINUE         
      
      CALL RCCOMA(IMAT,'ELAS',PHENOM,CERR)
      IF (PHENOM.NE.'ELAS') THEN
         CALL UTMESS('F','LCMATE','SEUL ELAS EST POSSIBLE ACTUELLEMENT')
      ENDIF
      
C
C
C -    ELASTICITE -----------------------------------------
C
          NOMC(1) = 'E       '
          NOMC(2) = 'NU      '
          NOMC(3) = 'ALPHA   '
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C        LA IL FAUDRAIT RECUPER CHAQUE MATERIAU
C
          CALL RCVALA (  IMAT,  ' ',  'ELAS', 1,  NOMPAR,VALPAD, 2,
     1                   NOMC(1),  MATERD(1,1),  CERR(1), 'FM' )
          CALL RCVALA (  IMAT,  ' ',  'ELAS', 1,  NOMPAR,VALPAD, 1,
     1                   NOMC(3),  MATERD(3,1),  CERR(3), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
          CALL RCVALA (  IMAT, ' ',   'ELAS',  1, NOMPAR,VALPAF, 2,
     1                   NOMC(1),  MATERF(1,1),  CERR(1), 'FM' )
          CALL RCVALA (  IMAT, ' ',   'ELAS',  1, NOMPAR,VALPAF, 1,
     1                   NOMC(3),  MATERF(3,1),  CERR(3), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
C
C -     MATERIAU CONSTANT ?
C
        MATCST = 'OUI'
        EPSI=R8PREM()
        DO 30 I = 1,3
          IF (ABS(MATERD(I,1)-MATERF(I,1) ).GT.EPSI*MATERD(I,1)) THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 30     CONTINUE
        DO 40 I = 1,NMAT
          IF (ABS(MATERD(I,2)-MATERF(I,2) ).GT.EPSI*MATERD(I,2)) THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
C
 9999   CONTINUE
 
        CALL JEDEMA()
        END
