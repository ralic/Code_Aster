        SUBROUTINE VECMAT ( MOD,    JMAT,   NMAT,   TEMPD,    TEMPF,
     1                      MATERD, MATERF, MATCST, TYPMA,    NDT,
     2                      NDI,    NR,     NVI )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/05/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C    ----------------------------------------------------------------
C
C    ---------------------------ATTENTION----------------------------
C    ROUTINE ASSOCIEE A LA LOI VENDOCHAB : VISCOPLASTICITE COUPLE
C    A DE L ENDOMMAGEMENT ISOTROPE
C    ---------------------------ATTENTION----------------------------
C
C    VENDOCHAB : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                    MATER(*,1) = E , NU , ALPHA
C                    MATER(*,2) = S_VP , SEDVP1, SEDVP2
C                                 N_VP , M_VP  , K_VP
C                                 R_D  , A_D   , K_D
C
C                    VARIABLES INTERNES : EVI,  P , R , DMG, ETAT
C    ----------------------------------------------------------------
C       IN  JMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  DE MATER
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           MATCST :  'NAP' SI  MATERIAU EST UNE S_D NAPPE
C                     'OUI' SI  MATERIAU EST CONSTANT
C                     'NON' SINON
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C    ----------------------------------------------------------------
        INTEGER         NMAT, NDT , NDI  , NR , NVI
        INTEGER         IOPTIO, IDNR
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2) , TEMPD , TEMPF
        REAL*8          EPSI
        CHARACTER*8     MOD, NOMC(12), TYPMA
        CHARACTER*2     BL2, FB2, CERR(12)
        CHARACTER*3     MATCST
        CHARACTER*11    METING
C       ----------------------------------------------------------------
        COMMON /OPTI/   IOPTIO , IDNR
        COMMON /METI/   METING
C       ----------------------------------------------------------------
        INTEGER LMAT,LFCT,IPI,IPIF,IK,IMAT,IVALK,I,J,JPRO,IL,JMAT,NBMAT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C       ----------------------------------------------------------------
        DATA EPSI       /1.D-15/
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
C - POUR INTEGRATION PAR METHODE EXPLICITE ON RESTE DIMENSIONNE EN 3D
      IF      (METING(1:11).EQ.'RUNGE_KUTTA') THEN
           NDT = 6
           NDI = 3
           NR  = NDT+3
           NVI = NDT+4
C - POUR INTEGRATION PAR METHODE IMPLICITE ON RESTE DIMENSIONNE EN 3D
      ELSEIF  (METING(1:9).EQ.'IMPLICITE') THEN
           NDT = 6
           NDI = 3
           NR  = NDT+2
           NVI = NDT+3
C - 3D
      ELSE  IF      (MOD(1:2).EQ.'3D')THEN
          NDT = 6
          NDI = 3
          NR  = NDT+3
          NVI = NDT+4
C - D_PLAN AXIS
      ELSE IF (MOD(1:6).EQ.'D_PLAN'.OR.MOD(1:4).EQ.'AXIS')THEN
          NDT = 4
          NDI = 3
          NR  = NDT+3
          NVI = NDT+4
C - C_PLAN
      ELSE IF (MOD(1:6).EQ.'C_PLAN')THEN
          NDT = 4
          NDI = 3
          NR  = NDT+4
          NVI = NDT+4
      ENDIF
C
        BL2 = '  '
        FB2 = 'F '
C
C -     VISCO-PLASTICITE-AVEC-ENDOMMAGEMENT
C
          TYPMA = 'COHERENT'
C
C --- VERIFICATION: LE MATERIAU CODE EST IL UNE NAPPE OU PAS?
C     ON REPREND LA PROGRAMMATION DE FOINT1 UTILISE PAR FOINTA
C     UTILISE PAR RCVALA
C --- CALCUL DE JPRO: DANS FOINT1 JPRO=ZI(IPIF+1) OU IPIF EST LE
C     POINTEUR DANS LE MATERIAU CODE UTILISE PAR FOINTA(IPIF,ETC.)
C
C     IPIF EST PASSE DIRECTEMENT EN ARGUMENT DE FOINTA (APPELEE
C     PAR RCVALA) A FOINT1
C
C --- CALCUL DE IPIF: DANS RCVALA ON A
C            IFON = IPI+LMAT-1+LFCT*(IK-1)
C            CALL FOINTA (IFON,NBPAR,NOMPAR,VALPAR,VALRES(IRES))
C
C            ET IPI=ZI(IMAT+2+ZI(IMAT+1)-1) LMAT=7 LFCT=9
C            IK=(1..NBF) OU NBF=ZI(IPI+2)
C            (NBF EST LE NOMBRE DE PARAMETRES MAIS ON PREND IK=NBF CAR
C             C'EST LE COEFF NBMATK_D (LE DERNIER) QUI EST SUCEPTIBLE
C            D'ETRE UNE NAPPE
C            ON BOUCLE SUR LES 9 (ZI(IPI+2)) FONCTIONS DU MATERIAU
C            VENDOCHAB, QUAND ON A K_D, ON TESTE SI C EST UNE NAPPE
C
C --- DONC:
C
      LMAT=7
      LFCT=9
C
      MATCST = 'OUI'
      
      NBMAT=ZI(JMAT)
C     UTILISABLE SEULEMENT AVEC UN MATERIAU PAR MAILLE
      CALL ASSERT(NBMAT.EQ.1)
      IMAT = JMAT+ZI(JMAT+NBMAT+1)
      
      DO 10 IK = 1, ZI(IMAT+1)
        IF(ZK16(ZI(IMAT)+IK-1)(1:9).EQ.'VENDOCHAB') THEN
          IPI=ZI(IMAT+IK+ZI(IMAT+1)-1)
          DO 15 IL = 1, ZI(IPI+2)
            IVALK = ZI(IPI+3)
            IF (ZK8(IVALK+IL-1)(1:3).EQ.'K_D') THEN
              IPIF=IPI+LMAT-1+LFCT*(IL-1)
              JPRO=ZI(IPIF+1)
              IF (ZK16(JPRO).EQ.'NAPPE') THEN
                MATCST = 'NAP'
              ENDIF
            ENDIF
 15       CONTINUE
        ENDIF
 10   CONTINUE
C
C -     RECUPERATION MATERIAU -----------------------------------------
C
C --- ATTENTION :
C SI MATCST='NAP' LES VALEURS INITIALES ET FINALES DE K_D (MATERD(9,2)
C ET MATERF(9,2)) SONT PRISES NULLES (0.0D0) PAR DEFFAUT. LA LECTURE
C DES VALEURS DE K_D QUAND CE COEFFICIENT EST RENTRE SOUS FORME DE
C NAPPE EST FAITE  DIRECTEMENT PAR UN APPEL A LA SOUS ROUTINE
C RCVALA DANS LA SOUS ROUTINE DONNANT LES VALEURS DES DERIVEES DES
C VARIABLES INTERNES DANS LA SOUS ROUTINE D'INTEGRATION LOCALE PAR
C UNE METHODE DE RUNGE-KUTTA NMVPRK.F
C
C
          NOMC(1)   = 'E       '
          NOMC(2)   = 'NU      '
          NOMC(3)   = 'ALPHA   '
          NOMC(4)   = 'S_VP    '
          NOMC(5)   = 'SEDVP1  '
          NOMC(6)   = 'SEDVP2  '
          NOMC(7)   = 'N_VP    '
          NOMC(8)   = 'M_VP    '
          NOMC(9)   = 'K_VP    '
          NOMC(10)  = 'R_D     '
          NOMC(11)  = 'A_D     '
          NOMC(12)  = 'K_D     '

C
          DO  9 J = 1 , 2
          DO  9 I = 1 , NMAT
          MATERD(I,J) = 0.D0
          MATERF(I,J) = 0.D0
 9       CONTINUE
C
C -     RECUPERATION MATERIAU A TEMPD (T)
C
          CALL RCVALA(JMAT,' ',   'ELAS',       1,  'TEMP', TEMPD, 3,
     1                   NOMC(1),  MATERD(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERD(3,1) = 0.D0
          CALL RCVALA(JMAT,' ','VENDOCHAB',   1,  'TEMP', TEMPD, 8,
     1                   NOMC(4),  MATERD(1,2),  CERR(4), FB2 )
        IF (MATCST.EQ.'NAP') THEN
          MATERD(9,2)=0.0D0
        ELSE
          CALL RCVALA(JMAT,' ','VENDOCHAB',   1,  'TEMP', TEMPD, 1,
     1                   NOMC(12),  MATERD(9,2),  CERR(12), FB2 )
        ENDIF
C
C -     RECUPERATION MATERIAU A TEMPF (T+DT)
C
          CALL RCVALA(JMAT,' ',   'ELAS',       1,  'TEMP', TEMPF, 3,
     1                   NOMC(1),  MATERF(1,1),  CERR(1), BL2 )
          IF ( CERR(3) .NE. 'OK' ) MATERF(3,1) = 0.D0
          CALL RCVALA(JMAT,' ','VENDOCHAB',   1,  'TEMP', TEMPF, 8,
     1                   NOMC(4),  MATERF(1,2),  CERR(4), FB2 )
        IF (MATCST.EQ.'NAP') THEN
          MATERF(9,2)=0.0D0
        ELSE
          CALL RCVALA(JMAT,' ','VENDOCHAB',   1,  'TEMP', TEMPF, 1,
     1                   NOMC(12),  MATERF(9,2),  CERR(12), FB2 )
        ENDIF
C
C
C - MATERIAU CONSTANT ? REMARQUE : NON UTILISE POUR L'INSTANT
C
      IF (MATCST.EQ.'OUI') THEN
        DO 30 I = 1,2
          IF ( ABS ( MATERD(I,1) - MATERF(I,1) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 30     CONTINUE
        DO 40 I = 1,9
          IF ( ABS ( MATERD(I,2) - MATERF(I,2) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
      ENDIF
C
 9999   CONTINUE
        END
