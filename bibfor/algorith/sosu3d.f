        SUBROUTINE  SOSU3D(NNO, NPG, POIDSG, VFF, DFF,
     &             GEOM, OPTION, IMATE, CRIT, TREF, DEPL,
     &             MATRI,MASS,RIGI, SIG, MATUU, VECTU , 
     &             DEPLM, PESA, RINSTM, RINSTP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C TOLE CRP_21
C       IMPLICIT REAL*8 (A-H,O-Z)
       IMPLICIT NONE
C
       INTEGER       NNO, NPG, IMATE
       CHARACTER*16  OPTION
C
       REAL*8         VFF(NNO,NPG), DFF(2,NNO,NPG)
       REAL*8         GEOM(3,NNO), CRIT(4), TREF
       REAL*8        DEPL(1:2,1:NNO),DEPLM(1:2,1:NNO),SIG(7,NPG)
       REAL*8        MATRI(2*NNO,2*NNO),POIDSG(NPG)
       REAL*8        MATUU(*), VECTU(2,NNO)
       REAL*8        PESA(4),RINSTP,RINSTM,DT
       REAL*8        MASS(NNO,NNO),RIGI(NNO,NNO)
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM_CT
C           POUR LES JOINTS SURFACIQUES
C          AINSI QUE LES FORCES SUIVEUSES ET LEUR MATRICE DE
C          RIGIDITE CHAR_MECA_THMG ET RIGI_MECA_THMG
C
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  DFF    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : MATERIAU CODE
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  TA      : PARAMETRE TETA (TETA METHODE)
C IN  DEPL    : DEPLACEMENT A PARTIR DE LA CONF DE REF
C VAR SIG     : CONTRAINTES LAGRANGIENNES
C                  MODIFIEES  SOUS OPTIONS RAPH_MECA ET FULL_MECA
C                  INCHANGEES SOUS OPTION  RIGI_MECA_TANG
C OUT MATUU   : MATRICE DE RIGIDITE  (RIGI_MECA_TANG ET FULL_MECA)
C               MATRICE DE RIGIDITE DES CHARRGES (RIGI_MECA_THMG)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
C                FORCES SUIVEUSES (CHAR_MECA_THMG)
C......................................................................
C
      INTEGER KPG,N,I4,J4,II,JJ,I7,I8,II2,JJ2,KJI,KK
      REAL*8  C0,AF,MM,LT0,LH,POIDS,RHOF,SM0,Q0,W0,TR
      REAL*8  FM(3),RDEP(2),RDEPM(2),FGREC(2),RDEPD(2,2)
      REAL*8  TZER,PZER,TA,P,T,PM,TM,RTEMP,DPRES,DTEMP,EQ0TP,EQ0TM
      REAL*8  COVA(3,3),METR(2,2),JAC,A(2,2),CNVA(3,2)
C ....................................................................
C
C - INITIALISATION
      DT = RINSTP-RINSTM
      TZER = TREF
      PZER = 0.D0
C FORCE VOLUMIQUE  RECUPEREE DANS AFFE_CHAR_MECA
      IF ((OPTION(11:14) .EQ. 'THMG')) THEN
         FM(1)= PESA(1)*PESA(2)
         FM(2)= PESA(1)*PESA(3)
         FM(3)= PESA(1)*PESA(4)
      ELSE
         FM(1) = 0.D0
         FM(2) = 0.D0
         FM(3) = 0.D0
      ENDIF
      DO 703 J4 = 1,NNO
         DO 803 I4= 1,NNO
            MATRI((I4-1)*2+1,(J4-1)*2+1)=0.D0
            MATRI((I4-1)*2+2,(J4-1)*2+2)=0.D0
            MATRI((I4-1)*2+1,(J4-1)*2+2)=0.D0
            MATRI((I4-1)*2+2,(J4-1)*2+1)=0.D0
            MASS(J4,I4)=0.D0
            RIGI(J4,I4)=0.D0
 803     CONTINUE
 703  CONTINUE
C
C PARAMETRE THETA RECUPERE DANS STAT_NON_LINE
C
      TA= CRIT(4)
C
C - CALCUL POUR CHAQUE POINT DE GAUSS
C
      DO 10 KPG=1,NPG
C        CALCUL DES CHAMPS AU PT DE GAUSS  ET LEUR GRADIENT
C
         RDEP(1)=0.D0
         RDEPM(1)=0.D0
         RDEP(2)=0.D0
         RDEPM(2)=0.D0
         DO 702 J4 = 1,2
            DO 802 I4= 1,NNO
               RDEP(J4)=RDEP(J4)+VFF(I4,KPG)*DEPL(J4,I4)
               RDEPM(J4)=RDEPM(J4)+VFF(I4,KPG)*DEPLM(J4,I4)
 802        CONTINUE
 702     CONTINUE
         P = RDEP(1)
         T = RDEP(2)
C        CALCUL DU CORRECTIF DS D INTEGRATION  JAC SUR LA SURFACE
         CALL SUBACO(NNO,DFF(1,1,KPG),GEOM,COVA)
         CALL SUMETR(COVA,METR,JAC)
         CALL SUBACV(COVA,METR,JAC,CNVA,A)
         POIDS=POIDSG(KPG)*JAC
C
C        IF ((OPTION(1:14) .EQ. 'CHAR_MECA_THMG')) THEN
            FGREC(1)=0.D0
            FGREC(2)=0.D0
            DO 738 II=1,3
C              FORCE EXT DANS LES COORDONNEES DE L ELEMENT
               FGREC(1)=CNVA(II,1)*FM(II)+FGREC(1)
               FGREC(2)=CNVA(II,2)*FM(II)+FGREC(2)
 738        CONTINUE
C        ENDIF
C
C       PRODUITS DES FF ET DES DERIVEES
C       MATRICES DE MASSE ET DE RIGIDITE
        DO 81 II=1,NNO
           DO 82 JJ=1,NNO
              MASS(II,JJ)=POIDS*VFF(II,KPG)*VFF(JJ,KPG)
              RTEMP=0.D0
              DO 83 I7=1,2
                 DO 84 I8=1,2
                    RTEMP=RTEMP+DFF(I7,II,KPG)*DFF(I8,JJ,KPG)*A(I7,I8)
 84              CONTINUE
 83           CONTINUE
              RIGI(II,JJ)=POIDS*RTEMP
 82        CONTINUE
 81     CONTINUE
C
C       LOI DE COMPORTEMENT
C       RECUPERATION DES COEFFS DU MATERIAU
C
        CALL SOSULC(C0,MM,AF,LH,LT0,Q0,W0,RHOF,TR,SM0,IMATE)
C
        IF(OPTION(1:16).EQ.'CHAR_MECA_THMG') THEN
           DO 840 N=1,NNO
              RTEMP=FGREC(1)*DFF(1,N,KPG)+FGREC(2)*DFF(2,N,KPG)
              VECTU(1,N)=VECTU(1,N)-POIDS*RTEMP*RHOF*LH*TA*DT
              VECTU(2,N)=0.D0
 840       CONTINUE
        ENDIF
C
        IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN
           DO 740 N=1,NNO
              DO 741 II=1,NNO
                 DPRES = DEPL(1,II)-DEPLM(1,II)
                 DTEMP = DEPL(2,II)-DEPLM(2,II)
                 VECTU(1,N)=
     &           VECTU(1,N) +3*AF*DTEMP*MASS(II,N) - DPRES*MASS(II,N)/MM
     &           -LH*(TA*DEPL(1,II)+(1.D0-TA)*DEPLM(1,II))*RIGI(II,N)*DT
C
                 VECTU(2,N)=
     &           VECTU(2,N) +3*AF*DPRES*MASS(II,N) - C0*DTEMP*MASS(II,N)
     &          -LT0*(TA*DEPL(2,II)+(1.D0-TA)*DEPLM(2,II))*RIGI(II,N)*DT
 741          CONTINUE
              EQ0TP = EXP(-W0*RINSTP)
              EQ0TM = EXP(-W0*RINSTM)
              VECTU(2,N) = VECTU(2,N)+
     &                Q0*DT*(TA*EQ0TP+(1.D0-TA)*EQ0TM)*POIDS*VFF(N,KPG)
 740       CONTINUE
        ENDIF
C
        IF ( OPTION(1:16) .EQ. 'RIGI_MECA_TANG'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN
            DO 130 II=1,NNO
               DO 131 JJ=1,NNO
                  II2 = (II-1)*2
                  JJ2 = (JJ-1)*2
                  MATRI(II2+1,JJ2+1)=
     &            MATRI(II2+1,JJ2+1)-MASS(II,JJ)/MM-TA*LH*RIGI(II,JJ)*DT
                  MATRI(II2+2,JJ2+2)=
     &            MATRI(II2+2,JJ2+2)
     &                       -MASS(II,JJ)*C0/TR-TA*LT0*RIGI(II,JJ)*DT/TR
                  MATRI(II2+1,JJ2+2)=
     &            MATRI(II2+1,JJ2+2)-MASS(II,JJ)*(-3.D0)*AF
                  MATRI(II2+2,JJ2+1)=
     &            MATRI(II2+2,JJ2+1)-MASS(II,JJ)*(-3.D0)*AF
 131           CONTINUE
 130        CONTINUE
         ENDIF
C
         IF ( OPTION(1:16) .EQ. 'RIGI_MECA_THMG') THEN
         DO 230 II=1,NNO
            DO 231 JJ=1,NNO
               MATRI(II2+1,JJ2+1)=0.D0
               MATRI(II2+2,JJ2+2)=0.D0
               MATRI(II2+1,JJ2+2)=0.D0
               MATRI(II2+2,JJ2+1)=0.D0
 231        CONTINUE
 230     CONTINUE
         ENDIF
C
         IF ( OPTION(1:16) .EQ. 'RIGI_MECA_TANG'
     &   .OR. OPTION(1:16) .EQ. 'RIGI_MECA_THMG'
     &   .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN
            IF (KPG.EQ.NPG) THEN
               KJI=1
               DO 132 II=1,2*NNO
                  DO 133 JJ=1,II
                     MATUU(KJI) = MATRI(II,JJ)
                     KJI= KJI + 1
 133              CONTINUE
 132           CONTINUE
            ENDIF
         ENDIF
C
C -  CALCUL DES CONTRAINTES
C
         IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &      OPTION(1:9).EQ.'RAPH_MECA') THEN
C
C           APPORT DE MASSE FLUIDE
C
            SIG(1,KPG) = -3.D0*AF*(T-TZER) +(P-PZER)/MM
C
C           ENTROPIE
C
            SIG(2,KPG) = C0*(T-TZER)/TR -3.D0*AF*(P-PZER)
            SIG(3,KPG)=0.D0
C
C           CALCUL DE GRAD P ET GRAD T
C
            DO 99 JJ = 1,2
               DO 98 KK = 1,2
                  RDEPD(JJ,KK) = 0.D0
 98            CONTINUE
 99         CONTINUE
            DO 101 II=1,NNO
               DO 102 I7=1,2
                  DO 103 I8=1,2
                     RDEPD(1,I7)=RDEPD(1,I7)+
     &                           A(I7,I8)*DFF(I8,II,KPG)*DEPL(1,II)
                     RDEPD(2,I7)=RDEPD(2,I7)+
     &                           A(I7,I8)*DFF(I8,II,KPG)*DEPL(2,II)
 103              CONTINUE
 102           CONTINUE
 101        CONTINUE
C
C           FLUX HYDRAULIQUE SIGMA4
C
            SIG(4,KPG) = LH*(RHOF*FGREC(1)-RDEPD(1,1))
            SIG(5,KPG) = LH*(RHOF*FGREC(2)-RDEPD(1,2))
C
C           FLUX THERMIQUE SIGMA5
C
            SIG(6,KPG) =(-1)*LT0*RDEPD(2,1)/TR
            SIG(7,KPG) =(-1)*LT0*RDEPD(2,2)/TR
         ENDIF
 10   CONTINUE
      END
