      SUBROUTINE TE0032 ( OPTION , NOMTE )
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 31/10/2006   AUTEUR PABHHHH N.TARDIEU 
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
C     -----------------------------------------------------------------
C     IN  OPTION : NOM DE L'OPTION A CALCULER
C     IN  NOMTE  : NOM DU TYPE_ELEMENT
C     -----------------------------------------------------------------
C     CALCUL DE PRESSION SUR LES ELEMENTS DKT, DST, DKQ, DSQ ET Q4G
C         OPTIONS TRAITEES   ==>   CHAR_MECA_FRCO3D
C                                  CHAR_MECA_FFCO3D
C                                  CHAR_MECA_PRES_R
C                                  CHAR_MECA_PRES_F
C                                  CHAR_MECA_PESA_R
C     -----------------------------------------------------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER      NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO
      INTEGER      I, J, IER, IPLAN, JGEOM, JCOQU, JVECG, JPRES, ITEMPS
      INTEGER      IADZI, IAZK24, LPESA
      REAL*8       PGL(3,3) , XYZL(3,4) , PGLO(3)  , PLOC(3)
      REAL*8       VECL(24) , FOR(6,4)  , RHO      , EPAIS
      REAL*8       UNDEMI
      REAL*8       VALPAR(4), DIST , EXCENT , PR
      LOGICAL      GLOBAL, LOCAPR
      CHARACTER*8  NOMPAR(4), MOPLAN, NOMAIL
C DEB ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)
C
      UNDEMI = 0.5 D0
      IPLAN  = 0
C
      CALL JEVECH ('PGEOMER', 'L', JGEOM)
      CALL JEVECH ('PCACOQU', 'L', JCOQU)
      CALL JEVECH ('PVECTUR', 'E', JVECG)
C
      IF (     NNO .EQ. 3) THEN
         CALL DXTPGL ( ZR(JGEOM) , PGL )
      ELSE IF( NNO .EQ. 4) THEN
         CALL DXQPGL ( ZR(JGEOM) , PGL )
      ENDIF
      CALL UTPVGL ( NNO , 3 , PGL , ZR(JGEOM) , XYZL )
C
C --- CAS DES CHARGEMENTS DE FORME REEL
      IF (OPTION .EQ. 'CHAR_MECA_PRES_R') THEN
C         ------------------------------
         GLOBAL = .FALSE.
         CALL JEVECH ('PPRESSR', 'L', JPRES)
         DO 110 J = 1, NNO
            DO 100 I = 1, 6
               FOR(I,J) = 0.D0
  100       CONTINUE
C----------------------------------------------------------------------
C           LE SIGNE MOINS CORRESPOND A LA CONVENTION :
C              UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
C----------------------------------------------------------------------
            FOR(3,J) = - ZR(JPRES+J-1)
  110    CONTINUE
C
      ELSE IF (OPTION .EQ. 'CHAR_MECA_FRCO3D') THEN
C              ------------------------------
         CALL JEVECH ('PFRCO3D', 'E', JPRES)
         GLOBAL = ABS(ZR(JPRES+6)) .LT. 1.D-3
         IF ( GLOBAL ) THEN
            CALL UTPVGL ( 1 , 6 , PGL , ZR(JPRES   ) , FOR(1,1) )
            CALL UTPVGL ( 1 , 6 , PGL , ZR(JPRES+ 8) , FOR(1,2) )
            CALL UTPVGL ( 1 , 6 , PGL , ZR(JPRES+16) , FOR(1,3) )
            IF ( NNO .EQ. 4 ) THEN
               CALL UTPVGL ( 1 , 6 , PGL , ZR(JPRES+24) , FOR(1,4) )
            ENDIF
         ELSE
C----------------------------------------------------------------------
C          LE SIGNE AFFECTE A FOR(3,J) A ETE CHANGE PAR AFFE_CHAR_MECA
C          SI PRES POUR RESPECTER LA CONVENTION :
C              UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
C              ET IL N'Y A PAS LIEU DE LE CHANGER ICI
C----------------------------------------------------------------------
            DO 210 J = 1, NNO
               DO 200 I = 1, 5
                  FOR(I,J) = ZR(JPRES-1+8*(J-1)+I)
  200          CONTINUE
               FOR(6,J) = 0.D0
  210       CONTINUE
         ENDIF
         IPLAN = NINT(ZR(JPRES+7))
C
C --- CAS DES CHARGEMENTS DE FORME FONCTION
C
      ELSE IF (OPTION .EQ. 'CHAR_MECA_PRES_F') THEN
C              ------------------------------
         CALL JEVECH ('PPRESSF', 'E', JPRES)
         CALL JEVECH ('PTEMPSR', 'E', ITEMPS)
         VALPAR(4) = ZR(ITEMPS)
         NOMPAR(4) = 'INST'
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'Z'
         DO 222 J = 0, NNO-1
            VALPAR(1) = ZR(JGEOM+3*J  )
            VALPAR(2) = ZR(JGEOM+3*J+1)
            VALPAR(3) = ZR(JGEOM+3*J+2)
            CALL FOINTE('FM',ZK8(JPRES),4,NOMPAR,VALPAR,PR,IER)
            IF ( PR .NE. 0.D0 ) THEN
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               CALL UTDEBM ('F','TE0032','LA PRESSION DOIT ETRE NULLE')
               CALL UTIMPK ('S',' POUR LA MAILLE ', 1, NOMAIL )
               CALL UTFINM
            ENDIF
  222    CONTINUE
         GOTO 9999
C
      ELSE IF (OPTION .EQ. 'CHAR_MECA_FFCO3D') THEN
C              ------------------------------
         CALL JEVECH ('PFFCO3D', 'E', JPRES)
         CALL JEVECH ('PTEMPSR', 'E', ITEMPS)
         VALPAR(4) = ZR(ITEMPS)
         NOMPAR(4) = 'INST'
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'Z'
         GLOBAL = ZK8(JPRES+6) .EQ. 'GLOBAL'
         LOCAPR = ZK8(JPRES+6) .EQ. 'LOCAL_PR'
         MOPLAN = ZK8(JPRES+7)
         IF (MOPLAN.EQ.'SUP') THEN
           IPLAN = 1
         ELSEIF (MOPLAN.EQ.'INF') THEN
           IPLAN = -1
         ELSEIF (MOPLAN.EQ.'MOY') THEN
           IPLAN = 2
         ENDIF
C
         IF ( GLOBAL ) THEN
C          REPERE GLOBAL
C --       LECTURE DES INTERPOLATIONS DE FX, FY, FZ, MX, MY, MZ
C
            DO 220 J = 0, NNO-1
               VALPAR(1) = ZR(JGEOM+3*J  )
               VALPAR(2) = ZR(JGEOM+3*J+1)
               VALPAR(3) = ZR(JGEOM+3*J+2)
C------------------------------------------------------
C  PAS DE CHANGEMENT DE SIGNE POUR LES FORCES REPARTIES
C------------------------------------------------------
           CALL FOINTE('FM',ZK8(JPRES  ),4,NOMPAR,VALPAR,FOR(1,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+1),4,NOMPAR,VALPAR,FOR(2,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+2),4,NOMPAR,VALPAR,FOR(3,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+3),4,NOMPAR,VALPAR,FOR(4,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+4),4,NOMPAR,VALPAR,FOR(5,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+5),4,NOMPAR,VALPAR,FOR(6,J+1),IER)
  220       CONTINUE
C
         ELSE IF ( LOCAPR ) THEN
C --        REPERE LOCAL - CAS D UNE PRESSION
C --        LECTURE DES INTERPOLATIONS DE LA PRESSION PRES
C
            DO 230 J = 0, NNO-1
               VALPAR(1) = ZR(JGEOM+3*J  )
               VALPAR(2) = ZR(JGEOM+3*J+1)
               VALPAR(3) = ZR(JGEOM+3*J+2)
               CALL FOINTE('FM',ZK8(JPRES+2),4,NOMPAR,VALPAR,PR,IER)
C-----------------------------------------------------
C       LE SIGNE MOINS DE FOR(3,J+1) CORRESPOND A LA CONVENTION :
C          UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
C-----------------------------------------------------
               FOR(3,J+1) = -1 * PR
               FOR(1,J+1) = 0.D0
               FOR(2,J+1) = 0.D0
               FOR(4,J+1) = 0.D0
               FOR(5,J+1) = 0.D0
               FOR(6,J+1) = 0.D0
  230       CONTINUE
C
         ELSE
C --        REPERE LOCAL - CAS DE F1, F2, F3, MF1, MF2
C --        LECTURE DES INTERPOLATIONS DE F1, F2, F3, MF1, MF2
C
            DO 235 J = 0, NNO-1
               VALPAR(1) = ZR(JGEOM+3*J  )
               VALPAR(2) = ZR(JGEOM+3*J+1)
               VALPAR(3) = ZR(JGEOM+3*J+2)
C------------------------------------------------------
C  PAS DE CHANGEMENT DE SIGNE POUR LES FORCES REPARTIES
C------------------------------------------------------
           CALL FOINTE('FM',ZK8(JPRES  ),4,NOMPAR,VALPAR,FOR(1,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+1),4,NOMPAR,VALPAR,FOR(2,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+2),4,NOMPAR,VALPAR,FOR(3,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+3),4,NOMPAR,VALPAR,FOR(4,J+1),IER)
           CALL FOINTE('FM',ZK8(JPRES+4),4,NOMPAR,VALPAR,FOR(5,J+1),IER)
               FOR(6,J+1) = 0.D0
  235       CONTINUE
         ENDIF
C
      ELSE IF ( OPTION.EQ.'CHAR_MECA_PESA_R') THEN
C              ------------------------------
         GLOBAL = .TRUE.

         CALL DXROEP ( RHO , EPAIS )
         CALL JEVECH ('PPESANR', 'L', LPESA)
         DO 240 I = 1, 3
            PGLO(I) = ZR(LPESA) * ZR(LPESA+I) * RHO * EPAIS
  240    CONTINUE
         CALL UTPVGL ( 1 , 3 , PGL , PGLO , PLOC )
         DO 260 I = 1, NNO
            DO 250 J = 1, 3
               FOR(J  ,I) = PLOC(J)
               FOR(J+3,I) = 0.D0
  250       CONTINUE
  260    CONTINUE
      ENDIF
C
      IF ( IPLAN .NE. 0 ) THEN
         EPAIS  = ZR(JCOQU)
         EXCENT = ZR(JCOQU+4)
         IF ( IPLAN .EQ. 1 ) THEN
            DIST = EXCENT + UNDEMI*EPAIS
         ELSEIF ( IPLAN .EQ. -1 ) THEN
            DIST = EXCENT - UNDEMI*EPAIS
         ELSEIF ( IPLAN .EQ.  2 ) THEN
            DIST = EXCENT
         ENDIF
C
         DO 270 I = 1, NNO
            FOR(4,I) = FOR(4,I) - DIST*FOR(2,I)
            FOR(5,I) = FOR(5,I) + DIST*FOR(1,I)
  270    CONTINUE
      ENDIF
C
      IF (     NNO .EQ. 3 ) THEN
         CALL DXTFOR ( NOMTE(1:8) , GLOBAL , XYZL , PGL , FOR , VECL )
      ELSE IF( NNO .EQ. 4 ) THEN
         CALL DXQFOR ( NOMTE(1:8) , GLOBAL , XYZL , PGL , FOR , VECL )
      ENDIF

      CALL UTPVLG ( NNO , 6 , PGL , VECL , ZR(JVECG) )
C
 9999 CONTINUE
      END
