      SUBROUTINE ALGOCU(DEFICU,RESOCU,LMAT  ,LDSCON,CNCINE,
     &                  RESU  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT     NONE
      CHARACTER*24 DEFICU
      CHARACTER*24 RESOCU
      CHARACTER*19 CNCINE,RESU
      INTEGER      LMAT
      INTEGER      LDSCON
C      
C ----------------------------------------------------------------------
C
C ROUTINE LIAISON_UNILATER (RESOLUTION)
C
C ALGO. DES CONTRAINTES ACTIVES POUR LES LIAISONS UNILATERALES
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICU  : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCU  : SD DE TRAITEMENT NUMERIQUE 
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON  : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C
      CHARACTER*32 JEXNUM
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      COMPLEX*16   CBID
      LOGICAL      TROUAC,DELPOS,LELPIV
      INTEGER      IER,IFM,NIV,NDECI,ISINGU,NPVNEG
      INTEGER      II,KK,ITER,ILIAC,NEQMAX
      INTEGER      JRESU
      INTEGER      INDIC,KKMIN,LLMIN
      INTEGER      LLIAC,JDECAL,POSNBL
      INTEGER      INDFAC,AJLIAI,SPLIAI,POSIT,SPAVAN
      INTEGER      NEQ,NBLIAC,NBLIAI,NBDDL
      REAL*8       R8MAEM,AJEU,RHO,RHORHO,AADELT,RMINMU,VAL,R8PREM
      REAL*8       XJVMAX,X1
      CHARACTER*1  TYPEAJ,TYPESP
      CHARACTER*24 APCOEF,APJEU,APDDL,COCO
      INTEGER      JAPCOE,JAPJEU,JAPDDL,JCOCO
      CHARACTER*24 POINOE
      INTEGER      JPOI
      INTEGER      CUDISI,NNOCU
      CHARACTER*19 LIAC,MU,DELT0,DELTA,CM1A,ATMU
      INTEGER      JLIAC,JMU,JDELT0,JDELTA,JCM1A,JATMU
      INTEGER      ITEMAX,COMPTS
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C ----------------------------------------------------------------------

C DELTA  : INCREMENT DONNE PAR CHAQUE ITERATION DE CONTRAINTES ACTIVES.
C          C'EST D/K+1.

C
      CALL INFNIV(IFM,NIV)
      
      IF(NIV.GE.2) THEN
        WRITE (IFM,*) '<LIA_UNIL> <> ALGORITHME   : CONT. ACTIVES'
      ENDIF
C 
C --- LECTURE DES STRUCTURES DE DONNEES 
C 
      POINOE = DEFICU(1:16)//'.POINOE'
      APCOEF = RESOCU(1:14)//'.APCOEF'
      APJEU  = RESOCU(1:14)//'.APJEU'
      APDDL  = RESOCU(1:14)//'.APDDL'
      LIAC   = RESOCU(1:14)//'.LIAC'
      MU     = RESOCU(1:14)//'.MU'
      DELT0  = RESOCU(1:14)//'.DEL0'
      DELTA  = RESOCU(1:14)//'.DELT'
      CM1A   = RESOCU(1:14)//'.CM1A'
      ATMU   = RESOCU(1:14)//'.ATMU' 
      COCO   = RESOCU(1:14)//'.COCO'         
C ======================================================================
      CALL JEVEUO(POINOE,'L',JPOI)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APJEU,'E',JAPJEU)      
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(ATMU,'E',JATMU)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(COCO ,'E',JCOCO)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NBLIAI : NOMBRE DE LIAISONS 
C --- NBLIAC : NOMBRE DE LIAISONS ACTIVES
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
C --- ITEMAX : NOMBRE D'ITERATIONS DANS L'ALGO
C --- INDFAC : INDICE DE DEBUT DE LA FACTORISATION
C --- INDIC  : 0  INITIALISATION,
C             +1 ON A RAJOUTE UNE LIAISON
C             -1 ON A ENLEVE UNE LIAISON
C --- SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C --- AJLIAI : INDICE DANS LuA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL
C              DE LA MATRICE  ACM1AT
C ======================================================================
      NNOCU  = CUDISI(DEFICU,'NNOCU')  
      NBLIAI = NNOCU 
      NEQ    = ZI(LMAT+2)
      ITEMAX = 2*NBLIAI
      TYPEAJ = 'A'
      TYPESP = 'S'
      INDIC  = 0
      INDFAC = 1
      AJLIAI = 0
      SPLIAI = 0
      NBLIAC = 0
      XJVMAX = 0.0D0
      ITER   = 0
C ======================================================================
C                             INITIALISATIONS

C
C --- RECOPIE DANS DELT0 DU CHAMP DE DEPLACEMENTS OBTENU SANS
C --- TRAITER LES CONDITIONS UNILATERALES
C --- CREATION DE DELTA0 = C-1B
C 
      DO 10 KK = 1,NEQ
        ZR(JDELT0-1+KK) = ZR(JRESU-1+KK)
        ZR(JRESU-1+KK)  = 0.0D0
   10 CONTINUE
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS ACTIVES
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
C --- (IL EST NEGATIF LORSQU'IL Y A ACTIVATION -> LIAISON ACTIVE)
C ======================================================================
      IF ( NIV .EQ. 2 ) THEN
        WRITE(IFM,*)'<LIA_UNIL> <> LIAISONS INITIALES '
      ENDIF
      IF (NBLIAC.EQ.0) THEN
        DO 30 II = 1,NBLIAI
          JDECAL = ZI(JPOI+II-1)
          NBDDL  = ZI(JPOI+II) - ZI(JPOI+II-1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDELT0),VAL)
          AJEU = ZR(JAPJEU+II-1) - VAL

          IF ((AJEU.LT.0.0D0).OR.(ZR(JAPJEU+II-1).LT. (0.D0))) THEN
            INDIC = 0
            POSIT = NBLIAC + 1
            CALL CUTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,
     &                  RESOCU,TYPEAJ,POSIT,II)
            IF (NIV.GE.2) THEN
              CALL CUIMP2(IFM,II,TYPEAJ,'ALG',RESOCU)
            ENDIF
          ENDIF

   30   CONTINUE
      END IF
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1000) NBLIAI
        WRITE(IFM,1005) NBLIAC
        WRITE(IFM,1001) ITEMAX
      ENDIF
C
C ======================================================================
C                    REPRISE DE LA BOUCLE PRINCIPALE
C ======================================================================
C
   40 CONTINUE
C
C ======================================================================
C ---
C --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
C ---
C ======================================================================
C

C ======================================================================
C --- SI PAS DE LIAISON ACTIVE, ON REMPLIT DELTA ET ON VA
C --- DIRECTEMENT AU CALCUL DE RHO
C ======================================================================

      IF (NBLIAC.EQ.0) THEN
        DO 50 KK = 1,NEQ
          ZR(JDELTA+KK-1) = ZR(JDELT0+KK-1) - ZR(JRESU-1+KK)
   50   CONTINUE
      END IF

C ======================================================================
C --- S'IL Y A DES LIAISONS ACTIVES, ON CALCULE MU ET DELTA
C ======================================================================

C
C --- DETERMINATION DE LA 1ERE LIAISON AYANT CHANGE D'ETAT (IN/ACTIF)
C --- (ON NE RECONSTRUIRA -A.C-1.AT QU'A PARTIR DE CETTE LIAISON)
C
      IF (NBLIAC.NE.0) THEN

C
C --- PAR LDLT OU MULT_FRONT
C
         SPAVAN = SPLIAI
C
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE INDFAC)
C
         CALL CUACAT(INDIC ,NBLIAC,AJLIAI,SPLIAI,LMAT  ,
     &               INDFAC,DEFICU,RESOCU,CNCINE,XJVMAX)
C 
C --- ELIMINATION DES PIVOTS NULS
C 
         CALL CUPIVO(XJVMAX,INDIC ,NBLIAC,AJLIAI,SPLIAI,
     &               SPAVAN,DEFICU,RESOCU)
C
C --- ON A SUPPRIME UNE LIAISON
C
         IF (INDIC.EQ.-1) THEN
           GOTO 150
         ENDIF    

C 
C --- FACTORISATION LDLT DE -A.C-1.AT
C
         IF (INDFAC.LE.NBLIAC) THEN
           IF(NIV.GE.2) THEN
             WRITE(IFM,*)'<LIA_UNIL> <> FACTORISATION MATRICE'
           ENDIF

           CALL TLDLGG(2     ,LDSCON,INDFAC,NBLIAC,0     ,
     &                 NDECI ,ISINGU,NPVNEG,IER   )

          INDFAC = NBLIAC + 1
C
C --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
C
          IF (IER.GT.0) THEN
            GO TO 999
          END IF
        END IF
C 
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C 
        CALL CUADU(DEFICU,RESOCU,NEQ,NBLIAC)
C 
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C --- DE 1 A NBLIAC
C 
        NEQMAX = ZI(LDSCON+2)
        ZI(LDSCON+2) = NBLIAC
        CALL RLDLGG(LDSCON,ZR(JMU),CBID,1)
        ZI(LDSCON+2) = NEQMAX
C 
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C 
        DO 70 KK = 1,NEQ
          ZR(JDELTA-1+KK) = ZR(JDELT0-1+KK) - ZR(JRESU-1+KK)
   70   CONTINUE
C 
C --- MISE A JOUR DU VECTEUR DEPLACEMENT <DU> CORRIGE
C 
        POSNBL = 0
        DO 71 ILIAC = 1, NBLIAC 
          LLIAC  = ZI(JLIAC-1+ILIAC) 
          POSNBL = POSNBL + 1
          CALL JEVEUO(JEXNUM(CM1A,LLIAC),'L',JCM1A)
          CALL DAXPY(NEQ,-ZR(JMU-1+POSNBL),ZR(JCM1A),1, ZR(JDELTA),1) 
          CALL JELIBE(JEXNUM(CM1A,LLIAC)) 
 71     CONTINUE 

      END IF

C
C
C --- CALCUL DE RHO = MIN ( (E(DEPTOT) - A.RESU)II / (A.DELTA)II) )
C --- SUR LES LIAISONS NON ACTIVES DE NUMERO II
C
      RHO    = R8MAEM()
      DELPOS = .FALSE.
C
      IF (NBLIAC.EQ.NBLIAI) THEN
C ======================================================================
C -- SI TOUTES LES LIAISONS SONT ACTIVES : RHO = 1
C ======================================================================
        RHO = 1.D0
      ELSE IF (NBLIAC.LT.NBLIAI) THEN
C ======================================================================
C -- S'IL Y A DES LIAISONS NON ACTIVES : CALCUL DE RHO
C ======================================================================
        DO 112 II = 1,NBLIAI
          TROUAC = .FALSE.
C ======================================================================
C -- LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
          DO 90 ILIAC = 1,NBLIAC
            IF (ZI(JLIAC-1+ILIAC).EQ.II) TROUAC = .TRUE.
   90     CONTINUE
C ======================================================================
C -- CALCUL DE A.DELTA SI LA LIAISON II N'EST PAS ACTIVE
C ======================================================================
          IF (.NOT.TROUAC) THEN
            JDECAL = ZI(JPOI+II-1)
            NBDDL  = ZI(JPOI+II) - ZI(JPOI+II-1)             
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JDELTA),AADELT)

C ======================================================================
C -- SI A.DELTA EST POSITIF POUR II : CALCUL DE E(DEPTOT) - A.RESU
C -- RHO = MIN ( ( E(DEPTOT) - A.RESU )II / (A.DELTA)II )
C -- ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
C -- MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
              CALL CUELPV(II,RESOCU,NBLIAI,LELPIV)
              IF (LELPIV) THEN
                GOTO 112
              ENDIF            
              DELPOS = .TRUE.
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                    ZR(JRESU),VAL)

              AJEU = ZR(JAPJEU+II-1) - VAL
              AJEU = AJEU/AADELT
              IF (AJEU.LT.RHO) THEN
                RHO = AJEU
                LLMIN = II
              ENDIF
            ENDIF
          ENDIF
  112   CONTINUE
C ======================================================================
C -- SI TOUS LES (A.DELTA)II SONT NEGATIFS : RHO = 1
C ======================================================================
        IF (.NOT.DELPOS) THEN
          RHO = 1.0D0
        END IF
      END IF
C
C --- TESTS SUR RHO ET ACTUALISATION DE RESU
C
      X1 = 1.D0
      RHORHO = MIN(RHO,X1)

      DO 120 KK = 1,NEQ
        ZR(JRESU-1+KK) = ZR(JRESU-1+KK) + RHORHO*ZR(JDELTA-1+KK)
  120 CONTINUE
C 
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (LLMIN)
C 
      IF (RHO.LT.1.0D0) THEN
        POSIT = NBLIAC + 1
        CALL CUTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,
     &              RESOCU,TYPEAJ,POSIT,LLMIN)
         IF (NIV.GE.2) THEN
            CALL CUIMP2(IFM,LLMIN,TYPEAJ,'ALG',RESOCU)
         END IF
      ELSE
C 
C -- SI RHO > 1 OU RHO = 1
C 
C - SI PAS DE LIAISONS ACTIVES -> ON A CONVERGE
C 
        IF (NBLIAC.EQ.0) THEN
          GO TO 160
        ENDIF

        RMINMU = R8MAEM()
        DO 130 ILIAC = 1,NBLIAC
          IF (RMINMU.GT.ZR(JMU-1+ILIAC)) THEN
            RMINMU = ZR(JMU-1+ILIAC)
            KKMIN  = ILIAC
          END IF
  130   CONTINUE

C 
C - SI TOUS LES MU SONT > 0 -> ON A CONVERGE 
C 
        IF (RMINMU.GE.0.0D0) THEN
          GO TO 160
        ENDIF
C ======================================================================
C - SINON ON ENLEVE LA LIAISON KKMIN AYANT LE MU LE PLUS NEGATIF
C - ET ON DECALE LA LISTE DES LIAISONS ACTIVES
C - ATTENTION KKMIN EST UN INDICE DANS LA LISTE DES LIAISONS <ACTIVES>
C - ET NON DANS LA LISTE DE TOUTES LES LIAISONS POSSIBLES
C ======================================================================
        LLIAC = ZI(JLIAC-1+KKMIN)   
        CALL CUTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,
     &              RESOCU,TYPESP,KKMIN,LLIAC)
          
        IF (NIV.GE.2) THEN
          CALL CUIMP2(IFM,LLIAC,TYPESP,'ALG',RESOCU)
        END IF

      END IF
C ======================================================================
C - ON PASSE A L'ITERATION DE CONTRAINTES ACTIVES SUIVANTES
C ======================================================================
  150 CONTINUE
      ITER = ITER + 1

C 
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS 
C 
      IF (ITER.GT.ITEMAX+1) THEN
        GO TO 999
      END IF

      GO TO 40

C ======================================================================
C                            ON A CONVERGE
C ======================================================================

  160 CONTINUE

C 
C --- CALCUL DES FORCES (AT.MU) 
C 

      CALL R8INIR(NEQ,0.D0,ZR(JATMU),1)

      COMPTS = 0
      DO 161 ILIAC = 1, NBLIAC
         LLIAC  = ZI(JLIAC+ILIAC-1)
         JDECAL = ZI(JPOI+LLIAC-1)
         NBDDL  = ZI(JPOI+LLIAC) - ZI(JPOI+LLIAC-1)         
         COMPTS = COMPTS + 1         
         CALL CALATM(NEQ,NBDDL,ZR(JMU-1+COMPTS),
     +               ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
 161  CONTINUE

C
C --- MAJ DU JEU (IL N'EST RECALCULE QU'EN DEBUT DE PAS DE TPS)
C

      DO 162 ILIAC = 1, NBLIAC
        JDECAL = ZI(JPOI+ILIAC-1)
        NBDDL  = ZI(JPOI+ILIAC) - ZI(JPOI+ILIAC-1)
        CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &             ZR(JRESU),VAL)      
        ZR(JAPJEU+ILIAC-1) = ZR(JAPJEU+ILIAC-1) - VAL
 162  CONTINUE
C
      ZI(JCOCO+2) = NBLIAC
C 
C --- AFFICHAGE FINAL
C 
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1002) ITER
        WRITE(IFM,1003) NBLIAC
      END IF

  999 CONTINUE
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES
C ======================================================================
C
      CALL JEDEMA()
C
 1000 FORMAT (' <LIA_UNIL> <> NOMBRE DE LIAISONS POSSIBLES: ',I6)
 1001 FORMAT (' <LIA_UNIL> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <LIA_UNIL> <> FIN DES ITERATIONS (NBR: ',I6,')')
 1003 FORMAT (' <LIA_UNIL> <> NOMBRE DE LIAISONS FINALES:',
     &       I6,')')
 1005 FORMAT (' <LIA_UNIL> <> NOMBRE DE LIAISONS INITIALES:',
     &       I6,')')
C ======================================================================

      END
