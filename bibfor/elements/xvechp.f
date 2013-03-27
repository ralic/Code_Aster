      SUBROUTINE XVECHP(NDIM,ELREFP,NNOP,IGEOM,ITEMP,ITPS,IHECHP,JPTINT,
     &                  JAINT,JCFACE,JLONCH,JLST,JBASEC,NFH,NFE,FONREE,
     &                  IVECTT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/03/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
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
C TOLE CRS_1404
C RESPONSABLE CUVILLIEZ
C.......................................................................
      IMPLICIT NONE
C
C     BUT: THERMIQUE LINEAIRE / ELEMENTS PRINCIPAUX X-FEM LINEAIRES
C          ECHANGE_PAROI POUR FISSURES X-FEM 
C
C          CALCUL DE 'CHAR_THER_PARO_F' ET 'CHAR_THER_PARO_R' (SOUS-TE)
C
C IN :
C ---
C NDIM   --> DIMENSION DE L'ESPACE (2 OU 3)
C ELREFP --> NOM DE L'ELT PARENT DE REFERENCE
C NNOP   --> NBRE DE NOEUDS DE L'ELT PARENT DE REFERENCE
C IGEOM  --> ADRESSE DES COORDONEES DES NOEUDS DE L'ELT PARENT
C ITEMP  --> ADRESSE DU CHAMP DE TEMPERATURE A L'INSTANT -
C ITEMPS --> ADRESSE DES PARAMETRES DE LA DICRETISATION EN TEMPS
C IHECHP --> ADRESSE DU COEFFICIENT ENCHANGE PAROI
C JPTINT --> ADRESSE DU VECTEUR DES POINTS D'INTERSECTION (*)
C JAINT  --> ADRESSE DU VECTEUR DES ARRETES INTERSECTEES
C JCFACE --> ADRESSE DU VECTEUR DE CONNECTIVITE DES FACETTES
C JLONCH --> ADRESSE DU VECTEUR DE LONGUEUR DES CHAMPS
C JLST   --> ADRESSE DE LA LEVEL SET TANGENTIELLE
C JBASEC --> ADRESSE DU VECTEUR DE LA BASE COVARIANTE DES FACETTES
C NFH    --> NBRE DE FONCTION D'ENRICHISSEMENT HEAVISIDE (0 OU 1)
C NFE    --> NBRE DE FONCTION D'ENRICHISSEMENT CRACKTIP  (0 OU 1)
C FONREE --> 'FONC' OU 'REEL'
C
C OUT :
C ----
C IVECTT --> ADRESSE DU VECTEUR ELEMENTAIRE
C.......................................................................
      INCLUDE 'jeveux.h'
C-----------------------------------------------------------------------
C
      CHARACTER*4 FONREE
      CHARACTER*8 ELREFP
      INTEGER     NDIM,NNOP,IGEOM,ITEMP,ITPS,IHECHP,JPTINT,JAINT,JCFACE
      INTEGER     JLONCH,JLST,JBASEC,NFH,NFE,IVECTT
C
C-----------------------------------------------------------------------
C
      CHARACTER*8 TYPMA,FPG,ELC,ELREFC,NOMPAR(4)
      LOGICAL     AXI,LTEATT
      INTEGER     NBDDL,ZXAIN,XXMMVD,IADZI,IAZK24,IBID,IBID2(12,3),NBF
      INTEGER     FAC(6,4),NBAR,AR(12,3),CFACE(5,3),NINTER,NFACE,NPTF
      INTEGER     I,J,IFA,NLI,IN(3),CPT,INO,NNOF,NPGF,IPOIDF,IVFF,IDFDEF
      INTEGER     IPGF,ILEV,INP,JNP,KDDL,LDDL,IER
      INTEGER     MXSTAC,IPOS
C
      PARAMETER (MXSTAC=1000)
C
      REAL*8      THETA,HE(2),MULT,XG(4),JAC,FF(27),R27BID(27),ND(3)
      REAL*8      DFBID(27,3),R3BID(3),LST,RR(2),FFENR(NNOP,1+NFH+NFE)
      REAL*8      HECHP,R8TMP,R
C
C     PAR CONVENTION :
C     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
C     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
      DATA    HE / -1.D0 , 1.D0/
C
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C     INITIALISATIONS
C-----------------------------------------------------------------------
C
C     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
C     (VOIR CRS 1404)
      CALL ASSERT(NNOP.LE.MXSTAC .AND. 1+NFH+NFE.LE.MXSTAC)
C
C     S'AGIT-IL D'UNE MODELISATION AXIS
      AXI = .FALSE.
      IF ( LTEATT(' ','AXIS','OUI') ) AXI = .TRUE.
C
C     RECUP DU PARAMETRE THETA (POUR LE THETA SCHEMA)
      THETA = ZR(ITPS-1+3)
C
C     NBRE DE DDLS PAR NOEUD PARENT
      NBDDL = 1+NFH+NFE
C
C     LONGUEUR FIXE DU VECTEUR D'ADRESSE JAINT
      ZXAIN = XXMMVD('ZXAIN')
C
C     ELREFE ET FPG POUR LES FACETTES
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)(1:8)
      IF (NDIM.EQ.3) THEN
        CALL CONFAC(TYPMA,IBID2,IBID,FAC,NBF)
        ELC='TR3'
        FPG='XCON'
      ELSEIF (NDIM.EQ.2) THEN
        CALL CONARE(TYPMA,AR,NBAR)
        ELC='SE2'
        FPG='MASS'
      ENDIF
C
C     RECUPERATIONS DES DONNEES SUR LA TOPOLOGIE DES FACETTES
      NINTER=ZI(JLONCH-1+1)
      NFACE =ZI(JLONCH-1+2)
      NPTF  =ZI(JLONCH-1+3)
      IF (NINTER.LT.NDIM) GOTO 9999
      DO 11 I=1,NFACE
        DO 12 J=1,NPTF
          CFACE(I,J)=ZI(JCFACE-1+NDIM*(I-1)+J)
 12     CONTINUE
 11   CONTINUE
C
C-----------------------------------------------------------------------
C     BOUCLE SUR LES FACETTES
C-----------------------------------------------------------------------
C
      DO 100 IFA=1,NFACE
C
C       PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
        MULT=1.D0
        DO 101 I=1,NDIM
          NLI=CFACE(IFA,I)
          IN(I)=NINT(ZR(JAINT-1+ZXAIN*(NLI-1)+2))
101     CONTINUE
C       SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
        IF (NDIM .EQ. 3) THEN
          IF (IN(1).NE.0.AND.IN(2).NE.0.AND.IN(3).NE.0) THEN
            DO 102 I=1,NBF
              CPT=0
              DO 103 INO=1,4
                IF (IN(1).EQ.FAC(I,INO).OR.IN(2).EQ.FAC(I,INO).OR.
     &            IN(3).EQ.FAC(I,INO))    CPT=CPT+1
 103          CONTINUE
              IF (CPT.EQ.3) THEN
                 MULT=0.5D0
                 GOTO 104
              ENDIF
 102        CONTINUE
          ENDIF
        ELSEIF (NDIM .EQ. 2) THEN
          IF (IN(1).NE.0.AND.IN(2).NE.0) THEN
            DO 1021 I=1,NBAR
              CPT=0
              DO 1031 INO=1,2
                IF (IN(1).EQ.AR(I,INO).OR.IN(2).EQ.AR(I,INO))
     &          CPT=CPT+1
 1031         CONTINUE
              IF (CPT.EQ.2) THEN
                MULT=0.5D0
                GOTO 104
              ENDIF
 1021       CONTINUE
          ENDIF
        ENDIF
 104    CONTINUE
C
        CALL ELREF4(ELC,FPG,IBID,NNOF,IBID,NPGF,IPOIDF,IVFF,IDFDEF,IBID)
C
C-----------------------------------------------------------------------
C       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
C-----------------------------------------------------------------------
C
        DO 200 IPGF=1,NPGF
C
C         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
C         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
C         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
C         ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
          ELREFC='NON'
          IF (NDIM.EQ.3) THEN
            CALL XJACFF(ELREFP,ELREFC,ELC,NDIM,FPG,JPTINT,IFA,CFACE,
     &                  IPGF,NNOP,IGEOM,JBASEC,XG,JAC,FF,R27BID,DFBID,
     &                  ND,R3BID,R3BID)
          ELSEIF (NDIM.EQ.2) THEN
            CALL XJACF2(ELREFP,ELREFC,ELC,NDIM,FPG,JPTINT,IFA,CFACE,
     &                  NPTF,IPGF,NNOP,IGEOM,JBASEC,XG,JAC,FF,R27BID,
     &                  DFBID,ND,R3BID)
          ENDIF
C
C         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
          IF (NFE.EQ.1) THEN
            LST=0.D0
            DO 210 I=1,NNOP
              LST=LST+ZR(JLST-1+I)*FF(I)
 210        CONTINUE
            CALL ASSERT(LST.LT.0.D0)
            RR(1)=-SQRT(-LST)
            RR(2)= SQRT(-LST)
          ENDIF
C
C         VALEUR DU COEFFICIENT ECHANGE PAROI
          IF (FONREE.EQ.'REEL') THEN
            HECHP = ZR(IHECHP)
          ELSE IF (FONREE.EQ.'FONC') THEN
            NOMPAR(1)='X'
            NOMPAR(2)='Y'
            IF (NDIM.EQ.3) NOMPAR(3)='Z'
            IF (NDIM.EQ.3) NOMPAR(4)='INST'
            IF (NDIM.EQ.2) NOMPAR(3)='INST'
            XG(NDIM+1) = ZR(ITPS)
            CALL FOINTE('FM',ZK8(IHECHP),NDIM+1,NOMPAR,XG,HECHP,IER)
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
C
C         MODIFICATION DU JACOBIEN SI AXI
          IF (AXI) THEN
            R  = 0.D0
            DO 220 INP=1,NNOP
              R = R + FF(INP)*ZR(IGEOM-1+2*(INP-1)+1)
 220        CONTINUE
            CALL ASSERT(R.GT.0D0)
            JAC = JAC * R
          END IF
C
C-----------------------------------------------------------------------
C         BOUCLE SUR LES (DEUX) LEVRES DE LA FISSURES
C-----------------------------------------------------------------------
C
          DO 300 ILEV = 1,2
C
C           FFENR : TABLEAU DES FF ENRICHIES
            DO 310 I=1,NNOP
C             DDL CLASSIQUE (TEMP)
              FFENR(I,1) = FF(I)
C             DDL HEAVISIDE (H1)
              IF (NFH.EQ.1) THEN
                FFENR(I,1+NFH) = HE(ILEV)*FF(I)
              ENDIF
C             DDL CRACK-TIP (E1)
              IF (NFE.EQ.1) THEN
                FFENR(I,1+NFH+NFE) = RR(ILEV)*FF(I)
              ENDIF
 310        CONTINUE
C
C           REMPLISSAGE DU VECTEUR
            IPOS = 0
            DO 400 INP=1,NNOP
              DO 410 KDDL=1,NBDDL
C
                IPOS = IPOS + 1
C               QUANTITE LIEE AU SAUT DE TEMPERATURE
C               A TRAVERS LES LEVRES DE LA FISSURE
                R8TMP = 0.D0
                DO 420 JNP=1,NNOP
                  DO 421 LDDL=1,NBDDL
                    IF (LDDL.GT.1)
     &                R8TMP = R8TMP + ZR(ITEMP-1+NBDDL*(JNP-1)+LDDL)
     &                               *FFENR(JNP,LDDL)
 421              CONTINUE
 420            CONTINUE
                R8TMP = -2.D0*R8TMP
C
                ZR(IVECTT-1+IPOS) = ZR(IVECTT-1+IPOS) + (1.0D0-THETA)*
     &                              HECHP*JAC*MULT*FFENR(INP,KDDL)*R8TMP
C
 410          CONTINUE
 400        CONTINUE
C
 300      CONTINUE
C
C-----------------------------------------------------------------------
C         FIN BOUCLE SUR LES (DEUX) LEVRES DE LA FISSURES
C-----------------------------------------------------------------------
C
 200    CONTINUE
C-----------------------------------------------------------------------
C       FIN BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
C-----------------------------------------------------------------------
C
 100  CONTINUE
C-----------------------------------------------------------------------
C     FIN BOUCLE SUR LES FACETTES
C-----------------------------------------------------------------------
C
 9999 CONTINUE
C
      END
