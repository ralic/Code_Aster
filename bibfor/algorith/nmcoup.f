        SUBROUTINE NMCOUP(FAMI,KPG,KSP,NDIM,TYPMOD,IMAT,COMP,LCPDB,CRIT,
     &                      TIMED,TIMEF, EPSDT, DEPST,
     &                   SIGD,VIND,OPT,ELGEOM,NUMLC,SIGF,VINF,DSDE,IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/06/2010   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE JMBHH01 J.M.PROIX
C TOLE CRP_21
C       ----------------------------------------------------------------
        INTEGER         IMAT , NDIM,KPG,KSP,IRET,NUMLC
C
        REAL*8          CRIT(*)
        REAL*8          TIMED,     TIMEF
        REAL*8          ELGEOM(*)
        REAL*8          EPSDT(*),  DEPST(*)
        REAL*8          SIGD(6),   SIGF(6)
        REAL*8          VIND(*),   VINF(*)
C
        REAL*8          DSDE(*)
        LOGICAL         LCPDB
C
        CHARACTER*16    COMP(*),     OPT
        CHARACTER*(*)   FAMI
        CHARACTER*8     TYPMOD(*)
        
C       ----------------------------------------------------------------
C
C       AIGUILLAGE DES LOIS DE COMPORTEMENT COUPLES
C
C       ================================================================
C       ARGUMENTS
C
C       IN      KPG,KSP  NUMERO DU (SOUS)POINT DE GAUSS
C               NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
C               IMAT    ADRESSE DU MATERIAU CODE
C               COMP    COMPORTEMENT DE L ELEMENT
C                       COMP(1) = RELATION DE COMPORTEMENT
C                       COMP(2) = NB DE VARIABLES INTERNES
C                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C               OPT     OPTION DE CALCUL A FAIRE
C                               'RIGI_MECA_TANG'> DSDE(T)
C                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
C                               'RAPH_MECA'     > SIG(T+DT)
C               CRIT    CRITERES  LOCAUX
C                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                 (ITER_INTE_MAXI == ITECREL)
C                       CRIT(2) = TYPE DE JACOBIEN A T+DT
C                                 (TYPE_MATR_COMP == MACOMP)
C                                 0 = EN VITESSE     > SYMETRIQUE
C                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
C                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                                 (RESI_INTE_RELA == RESCREL)
C                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
C                                 REDECOUPAGE LOCAL DU PAS DES
C                                 (ITER_INTE_PAS == ITEDEC)
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C               ELGEOM  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
C                       FIXEE EN DUR)
C               TIMED   INSTANT T
C               TIMEF   INSTANT T+DT
C               EPSDT   DEFORMATION TOTALE A T
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               IRET    CODE RETOUR DE L'INTEGRATION INTEGRATION DU
C                       COUPLAGE FLUAGE/FISSURATION
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ABSENCE DE CONVERGENCE
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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

      CHARACTER*16 OPTION(2),CMP1,CMP2,CMP3,CMP4
      CHARACTER*16   TEXTE(2)

      CMP1(1:16) = COMP(8)
      CMP2(1:16) = COMP(9)
      CMP3(1:16) = COMP(10)
      OPTION(1)(1:16) = OPT
      IF (CMP3(1:8).NE.'        ') THEN
          CALL U2MESS('F','ALGORITH7_1')
      ENDIF


      IF (CMP1(1:10).EQ.'GRANGER_FP') THEN

         IF (CMP2(1:5) .EQ. 'ELAS '             .OR.
     &       CMP2(1:9) .EQ. 'VMIS_ISOT'        .OR.
     &       CMP2(1:14).EQ. 'VMIS_ISOT_LINE'   .OR.
     &       CMP2(1:8) .EQ. 'ROUSS_PR'         .OR.
     &       CMP2(1:15).EQ. 'BETON_DOUBLE_DP') THEN

           CALL NMCPLA (FAMI,KPG,KSP,NDIM,TYPMOD,IMAT,COMP,CRIT,
     &                      TIMED,TIMEF,
     &                      EPSDT,DEPST,SIGD,VIND,OPT,ELGEOM,NUMLC,
     &                      SIGF,VINF,DSDE,IRET)
           IF(IRET.EQ.1) GOTO 9999
         ELSE IF (CMP2(1:10) .EQ. 'ENDO_ISOT_BETON' .OR.
     &            CMP2(1:6)  .EQ. 'MAZARS') THEN
           OPTION(2)(1:16) = CMP2(1:16)
C          CALL NMGRAN ( NDIM, TYPMOD, IMAT, COMP, CRIT,
C     1                  TIMED,TIMEF, TEMPD,TEMPF,TREF,HYDRD,
C     &                  HYDRF,SECHD,SECHF,SREF,TEMPD,TEMPF,EPSDT,
C     2                  DEPST,SIGD, VIND, OPTION,ELGEOM,SIGF,VINF,DSDE)
           CALL U2MESS('F','ALGORITH7_2')

         ELSE
           CALL U2MESS('F','ALGORITH7_3')
         ENDIF

      ELSE IF (CMP1(1:13).EQ.'BETON_UMLV_FP') THEN

        IF (CMP2(1:15) .EQ. 'ENDO_ISOT_BETON' .OR.
     &      CMP2(1:6)  .EQ. 'MAZARS') THEN
         
          CMP4(1:16) = TYPMOD(2)
          OPTION(2)(1:16) = CMP2(1:16)

          IF  (CMP2(1:15) .EQ. 'ENDO_ISOT_BETON')  THEN
            IF ((TYPMOD(1).EQ.'C_PLAN').AND.(.NOT.LCPDB)) THEN
              CALL U2MESS('F','ALGORITH7_5')
            ENDIF  
          ELSE
             IF ((TYPMOD(1).EQ.'C_PLAN') .AND. LCPDB) THEN
              CALL U2MESS('F','ALGORITH7_4')
            ENDIF 
          ENDIF
                     
          IF (TYPMOD(2) .EQ. 'GRADEPSI') THEN         
            CALL LCUMFE ( FAMI,KPG,KSP,NDIM, TYPMOD, IMAT,
     &                    TIMED,TIMEF,
     &                    EPSDT,DEPST,SIGD, VIND, OPTION,SIGF,
     &                    VINF,DSDE,ELGEOM)             
          ELSEIF (TYPMOD(2) .EQ. 'GRADVARI') THEN
            TEXTE(1)=CMP4
            TEXTE(2)=CMP2
            CALL U2MESK('F','COMPOR1_49',2,TEXTE)
          ELSE
            CALL LCUMFP ( FAMI,KPG,KSP,NDIM, TYPMOD, IMAT, COMP,
     &                    TIMED,TIMEF,
     &                    EPSDT,DEPST,SIGD, VIND, OPTION, SIGF,
     &                    VINF,DSDE,CRIT) 
          ENDIF
        ELSE
          CALL U2MESS('F','ALGORITH7_3')
        ENDIF

      ELSE IF (CMP1(1:4).EQ.'GLRC') THEN


        IF (CMP2 .EQ. 'VMIS_ISOT_TRAC' .OR.
     &      CMP2 .EQ. 'VMIS_ISOT_LINE'. OR.
     &      CMP2 .EQ. 'VMIS_CINE_LINE') THEN

            OPTION(2)(1:16) = CMP2(1:16)

            CALL LGDMVM(IMAT,CMP2,EPSDT,DEPST,VIND,OPT,SIGD,
     &                  SIGF,VINF,DSDE,CRIT,IRET)

        ELSE
          CALL U2MESS('F','ALGORITH7_3')
        ENDIF

      ELSE
        CALL U2MESS('F','ALGORITH7_6')
      ENDIF

9999  CONTINUE
      END
