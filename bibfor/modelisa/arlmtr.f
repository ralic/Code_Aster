      SUBROUTINE ARLMTR(MAIL,NOMZ,NORMZ,NTM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C  CALCUL METRIQUE D'ADIMENSIONNEMENT DU PRODUIT SCALAIRE H1 / ARLEQUIN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C CHARACTER*8     MAIL    : SD MAILLAGE
C CHARACTER*(10)  NOMZ    : SD DOMAINE DE COLLAGE
C CHARACTER*(10)  NORMZ   : NORMALES LISSEES COQUE (CF LISNOR)
C CHARACTER*8     NTM(*)  : VECTEUR NOMS TYPES DE MAILLE
C
C SD D'ENTREE
C NOM.GROUPEMA : LISTE DE MAILLES DOMAINE DE COLLAGE
C NOM.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C
C SD DE SORTIE
C NOM.METRQ    : MATRICE SYMETRIQUE DIM*DIM (DIM=DIMENSION DE L'ESPACE)
C                DEFINIE POSITIVE. ELLE A LA DIMENSION D'UNE LONGUEUR.
C               ( MXX, MXY, [MXZ], MXY, MYY, [MYZ, MXZ, MYZ, MZZ] )
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C --- PARAMETRES
      INTEGER       NN0,NG0,NPERM
      PARAMETER     (NN0 = 27)
      PARAMETER     (NG0 = 64)
      PARAMETER     (NPERM = 12)

      REAL*8        TOL,TOLDYN
      PARAMETER     (TOL = 1.D-10)
      PARAMETER     (TOLDYN = 1.D-2)
 
C --- VARIABLES
      CHARACTER*(*) NOMZ,NORMZ
      CHARACTER*10  NOM,NORM
      CHARACTER*8   MAIL,NTM(*),TYPEMA
      INTEGER   DIM,NMA,NNO,NG,DG(4,2),NN,TYPE,MA1,MA2,IMA
      INTEGER   P0,P1,P2,P3,P4,P5,P6,Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7,I,J,K,L
      REAL*8    M0,M1(3),M2(9),F(NN0),DF(NN0*3),NO(NN0*3),JAC(9),DET,R
      LOGICAL   FAUX

      NORM = NORMZ
      NOM = NOMZ
      FAUX = .FALSE.

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.COORDO    .VALE','L', P0)
      CALL JEVEUO(MAIL//'.CONNEX','L',P1)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',P2)
      CALL JEVEUO(MAIL//'.TYPMAIL','L',P3)
      CALL JEEXIN(NORM,I)
      IF (I.NE.0) CALL JEVEUO(NORM,'L',P4)

      CALL JEVEUO(NOM//'.GROUPEMA','L',P5)
      CALL JEVEUO(NOM//'.BOITE.MMGLOB','L',P6)
      CALL JEVEUO(NOM//'.BOITE.DIME','L',Q0)

      DIM = ZI(Q0)
      NMA = ZI(Q0+1)

      Q0 = 0
      M0 = 0.D0
      DO 10 I = 1, DIM
        M1(I) = 0.D0
        DO 10 J = I, DIM
          Q0 = Q0 + 1
          M2(Q0) = 0.D0
 10   CONTINUE

C --- ALLOCATION OBJETS TEMPORAIRES

      CALL WKVECT('&&ARLMTR.FILTRE','V V L',NMA,Q0)
      CALL WKVECT('&&ARLMTR.PG','V V R',NG0,Q1)
      CALL WKVECT('&&ARLMTR.G','V V R',NG0*3,Q2)
      CALL WKVECT('&&ARLMTR.FG','V V R',NN0*NG0,Q3)
      CALL WKVECT('&&ARLMTR.DFG','V V R',NN0*3*NG0,Q4)
       
      DO 20 I = 1, NMA
        ZL(Q0-1+I) = .FALSE.
 20   CONTINUE

C --- MATRICE D'INERTIE DE LA ZONE DE COLLAGE

      DO 30 MA1 = 1, NMA
 
        IF (ZL(Q0-1+MA1)) GOTO 30
        
        TYPE = ZI(P3-1+ZI(P5-1+MA1))
        TYPEMA = NTM(TYPE)

        NG = 0
        CALL TMACOQ(TYPEMA,DIM,L)
        CALL FORMEN(TYPEMA,NG,DG)
        CALL NPGAUS(TYPEMA,DG(3,1)+2*DG(1,1),NG)
        CALL PGAUSS(TYPEMA,NG,ZR(Q1),ZR(Q2),NG,DIM)
            
        Q5 = Q2
        Q6 = Q3
        Q7 = Q4

        DO 40 I = 1, NG
          CALL FORME0(ZR(Q5),TYPEMA,ZR(Q6),NNO)
          CALL FORME1(ZR(Q5),TYPEMA,ZR(Q7),NNO,DIM)
          Q5 = Q5 + DIM
          Q6 = Q6 + NNO
          Q7 = Q7 + NNO*DIM
 40     CONTINUE
        
        DO 50 MA2 = MA1, NMA

          IF (ZL(Q0-1+MA2)) GOTO 50

          IMA = ZI(P5-1+MA2)
          IF (ZI(P3-1+IMA).NE.TYPE) GOTO 50

          ZL(Q0-1+MA2) = .TRUE.
          CALL CONOEU(IMA,ZI(P1),ZI(P2),ZR(P0),ZR(P4),DIM,L,NO,NNO)

C ------- INTEGRATION
          
          Q6 = Q3
          Q7 = Q4
          DET = 1.D0
          NN = NNO*DIM

          DO 60 I = 1, NG

            CALL DCOPY(NN,ZR(Q7),1,DF,1)
            CALL MTPROD(NO,DIM,0,DIM,0,NNO,ZR(Q6),1,0,1,0,F)
            CALL MTPROD(NO,DIM,0,DIM,0,NNO,DF,DIM,0,DIM,0,JAC)
            CALL MGAUST(JAC,DF,DIM,DIM,NNO,DET,FAUX)
            DET = ABS(DET) * ZR(Q1-1+I)

            Q6 = Q6 + NNO
            Q7 = Q7 + NN

C --------- CALCUL DES MOMENTS

            Q5 = 0
            M0 = M0 + DET    
            DO 60 J = 1, DIM
              R = DET*F(J)
              M1(J) = M1(J) + R
              DO 60 K = J, DIM
                Q5 = Q5 + 1
                M2(Q5) = M2(Q5) + R*F(K)            
 60       CONTINUE

 50     CONTINUE
 
 30   CONTINUE

C --- MESURE DE LA BOITE ENGLOBANTE

      DET = 1.D0
      DO 70 I = 1, DIM
        DET = DET*(ZR(P6+1)-ZR(P6))
        P6 = P6 + 2
 70   CONTINUE

C --- METRIQUE DE LA ZONE DE COLLAGE
      
      Q0 = 0
      DO 80 I = 1, DIM
        R = M1(I) / M0
        DO 80 J = I, DIM
          Q0 = Q0 + 1
          M2(Q0) = (M2(Q0) - R*M1(J)) / DET
 80   CONTINUE

C --- MATRICE UNITE

      Q0 = 0
      DO 90 I = 1, DIM
        Q0 = Q0 + 1
        F(Q0) = 1.D0
        DO 90 J = I+1, DIM
          Q0 = Q0 + 1
          F(Q0) = 0.D0
 90   CONTINUE

C --- RACINE CARREE DE LA METRIQUE

      CALL JACOBI(DIM,NPERM,TOL,TOLDYN,M2,F,JAC,M1,DF,I,2,2)
      
      Q0 = 0
      DO 100 I = 1, DIM
        R = SQRT(M1(I))
        DO 100 J = 1, DIM
          Q0 = Q0 + 1
          M2(Q0) = JAC(Q0)*R
 100  CONTINUE

C --- ECRITURE .METRQ

      CALL WKVECT(NOM//'.METRQ','V V R',DIM*DIM,Q0)
      CALL MTPROD(JAC,DIM,0,DIM,0,DIM,M2,DIM,0,DIM,0,ZR(Q0))

C --- DESALLOCATION

      CALL JEDETR('&&ARLMTR.DFG')
      CALL JEDETR('&&ARLMTR.FG')
      CALL JEDETR('&&ARLMTR.G')
      CALL JEDETR('&&ARLMTR.PG')
      CALL JEDETR('&&ARLMTR.FILTRE')

      CALL JEDEMA()

      END
