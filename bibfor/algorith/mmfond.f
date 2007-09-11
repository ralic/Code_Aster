      SUBROUTINE MMFOND(NOMA,DEFICO,IZONE,NBN,POSMA,IMA,BARSOU,IMABAR,
     &                  TYPBAR,INIBA1,INIBA2,INIBA3,NOQBA1,NOQBA2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/09/2007   AUTEUR KHAM M.KHAM 
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
C
      IMPLICIT NONE
      CHARACTER*24 DEFICO
      CHARACTER*8  NOMA
      LOGICAL      BARSOU
      INTEGER      NBN,POSMA,IMA,IZONE,IMABAR,TYPBAR
      INTEGER      INIBA1,INIBA2,INIBA3,NOQBA1,NOQBA2 
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR MAPPAR
C ----------------------------------------------------------------------
C
C TRAITEMENT DES MAILLES EN FOND DE FISSURE ET DU CAS DES ELEMENTS
C BARSOUM
C
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
C IN  POSMA  : NUMERO DE LA MAILLE ESCLAVE
C IN  NBN    : NOMBRE DE POINTS DE CONTACT DE LA MAILLE ESCLAVE
C               /!\  DEPEND DE LA MAILLE ET DU SCHEMA INTEGRATION
C OUT BARSOU : VAUT .TRUE. SI NOEUDS APPARTENANT A DES ELEMENTS DE 
C              BARSOUM SONT EXCLUS
C OUT IMABAR : NUMERO DE LA MAILLE ESCLAVE DE CET ELEMENT DE BARSOUM
C OUT TYPBAR : NOEUDS EXCLUS PAR CET ELEMENT DE BARSOUM
C              CAS TYPBAR = 1:
C                 NOEUDS EXCLUS: 1 ET 2 (SOMME=3)
C                 ON EXCLUE LE NOEUD MILIEU 5
C                 ON EXCLUE LES NOEUDS EN FACE: 3 ET 4
C              CAS TYPBAR = 2: 
C                 NOEUDS EXCLUS: 3 ET 4 (SOMME=7)
C                 ON EXCLUE LE NOEUD MILIEU 7
C                 ON EXCLUE LES NOEUDS EN FACE: 1 ET 2                
C              CAS TYPBAR = 3: 
C                 NOEUDS EXCLUS: 2 ET 3 (SOMME=5)
C                 ON EXCLUE LE NOEUD MILIEU 6
C                 ON EXCLUE LES NOEUDS EN FACE: 1 ET 4                
C              CAS TYPBAR = 4: 
C                 NOEUDS EXCLUS: 1 ET 4 (SOMME=5)
C                 ON EXCLUE LE NOEUD MILIEU 8
C                 ON EXCLUE LES NOEUDS EN FACE: 2 ET 3 
C OUT INIBA1 : PREMIER NUMERO DU NOEUD A EXCLURE DANS LA MAILLE 
C                 (DE 1 A 4. SEG3 OU QUAD4)
C OUT INIBA2 : DEUXIEME NUMERO DU NOEUD A EXCLURE DANS LA MAILLE 
C                 (DE 1 A 4. SEG3 OU QUAD4)
C OUT INIBA3 : TROISIEME NUMERO DU NOEUD A EXCLURE DANS LA MAILLE 
C                 (DE 1 A 4. SEG3 OU QUAD4)
C                  C'EST UN NOEUD MILIEU (LE NOEUD BARSOUM)
C OUT NOQBA1 : PREMIER NUMERO DU NOEUD EN FACE DU NOEUD A EXCLURE
C OUT NOQBA2 : DEUXIEME NUMERO DU NOEUD EN FACE DU NOEUD A EXCLURE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
      CHARACTER*32 JEXATR      
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*24 NOMACO,PNOMA,CONTNO
      INTEGER      JNOMA,JPONO,JNOCO,POSNOE,NUMNOE
      INTEGER      IBID,II,KK,N1,N2,JJ
      REAL*8       R8BID
      CHARACTER*24 K24BLA,KINFO,BARSNO,PBARS,BARSMA,PBARM
      CHARACTER*8  ALIAS
      LOGICAL      BARSO1,BARSO2
      INTEGER      MBARS,NBARS,ILONG,NUMBAR,NUBAR
      INTEGER      JBARS,JPBARS,JBARM,JPBARM,ICONEX
      DATA K24BLA /' '/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C      
      NOMACO = DEFICO(1:16) // '.NOMACO'
      PNOMA  = DEFICO(1:16) // '.PNOMACO'
      CONTNO = DEFICO(1:16) // '.NOEUCO' 
      BARSNO = DEFICO(1:16) // '.BANOCO'
      PBARS  = DEFICO(1:16) // '.PBANOCO'
      BARSMA = DEFICO(1:16) // '.BAMACO'
      PBARM  = DEFICO(1:16) // '.PBAMACO'                       
      CALL JEVEUO(NOMACO,'L',JNOMA)
      CALL JEVEUO(PNOMA ,'L',JPONO)
      CALL JEVEUO(CONTNO,'L',JNOCO)           
      CALL JEVEUO(BARSNO,'L',JBARS)
      CALL JEVEUO(PBARS ,'L',JPBARS)
      CALL JEVEUO(BARSMA,'L',JBARM)
      CALL JEVEUO(PBARM ,'L',JPBARM)
      
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILONG)
      CALL JEVEUO(NOMA(1:8)       //'.CONNEX','L',ICONEX)
C
C --- ON TESTE SI LA MAILLE EST UNE MAILLE DE FISSURE
C --- GROUP_MA_FOND OU MAILLE_FOND
C
      BARSO1 = .FALSE.
      BARSO2 = .FALSE.
      MBARS  = ZI(JPBARM+IZONE) - ZI(JPBARM+IZONE-1)
      IF (MBARS .GE. 1) THEN
        DO 33 KK  = 1,MBARS
        
          NUBAR  = ZI(JBARM+ZI(JPBARM+IZONE-1)+KK-1)
          CALL MMELTY(NOMA,NUBAR,ALIAS,IBID,IBID)

          IF (ALIAS.EQ.'PO1') THEN
            N1 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+1)
            DO 53 JJ = 1,NBN
              POSNOE = ZI(JNOMA+ZI(JPONO+POSMA-1)+JJ-1)
              NUMNOE = ZI(JNOCO+POSNOE-1)
              IF (NUMNOE .EQ. N1) THEN
                BARSOU = .TRUE.
                INIBA1 = JJ
                IMABAR = IMA
                GOTO 33
              END IF
 53         CONTINUE   
          ELSEIF (ALIAS.EQ.'SG3') THEN
            N1 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+1)
            N2 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+2)
            BARSO1 = .FALSE.
            BARSO2 = .FALSE.
            DO 55 JJ = 1,NBN
              POSNOE = ZI(JNOMA+ZI(JPONO+POSMA-1)+JJ-1)
              NUMNOE = ZI(JNOCO+POSNOE-1)
              IF (NUMNOE .EQ. N1) THEN
                BARSO1 = .TRUE.
                INIBA1 = JJ
                IMABAR = IMA
              ELSEIF (BARSO1 .AND. NUMNOE .EQ. N2) THEN
                BARSO2 = .TRUE.
                INIBA2 = JJ
                IMABAR = IMA
              END IF
 55         CONTINUE
C 
C --- TRAITEMENT DES "SEG3" EN FOND DE FISSURE
C ---   ON TRAITE LES QUAD 4
C ---   (VOIR ENTETE DE ROUTINE)
C
            IF (BARSO1 .AND. BARSO2) THEN
            
              BARSOU = .TRUE.
              IF (INIBA1+INIBA2 .EQ. 3) THEN
                TYPBAR = 1
                IF (INIBA1 .EQ. 1) THEN
                  NOQBA1 = 4
                  NOQBA2 = 3
                  INIBA3 = 5
                ELSE
                  NOQBA1 = 3
                  NOQBA2 = 4
                  INIBA3 = 5
                END IF  
              ELSEIF (INIBA1+INIBA2 .EQ. 7) THEN 
                TYPBAR = 2
                IF (INIBA1 .EQ. 3) THEN
                  NOQBA1 = 2
                  NOQBA2 = 1
                  INIBA3 = 7
                ELSE
                  NOQBA1 = 1
                  NOQBA2 = 2
                  INIBA3 = 7
                END IF 
              ELSEIF (INIBA1+INIBA2 .EQ. 5 .AND. (INIBA1.EQ.2
     &               .OR. INIBA2.EQ.2)) THEN
                TYPBAR = 3
                IF (INIBA1 .EQ. 2) THEN
                  NOQBA1 = 1
                  NOQBA2 = 4
                  INIBA3 = 6
                ELSE
                  NOQBA1 = 4
                  NOQBA2 = 1
                  INIBA3 = 6
                END IF 
              ELSEIF (INIBA1+INIBA2 .EQ. 5 .AND. (INIBA1.EQ.4
     &               .OR. INIBA2.EQ.4)) THEN
                TYPBAR = 3
                IF (INIBA1 .EQ. 1) THEN
                  NOQBA1 = 2
                  NOQBA2 = 3
                  INIBA3 = 8
                ELSE
                  NOQBA1 = 3
                  NOQBA2 = 2
                  INIBA3 = 8
                END IF 
              END IF            
              GOTO 54           
            END IF
          ELSE
            KINFO(1:8) = ALIAS
            CALL MMERRO(DEFICO,K24BLA,NOMA,'MMFOND','F','BARSOUM_NEX',
     &                  NUBAR,IBID,IBID,IBID,R8BID,KINFO)            
          END IF
 33     CONTINUE
 54     CONTINUE

      ELSE
C
C ---- ON TESTE SI LA MAILLE CONTIENT UN NOEUD DE FOND DE
C ---- FISSURE GROUP_NO_FOND OU NOEUD_FOND
C
        NBARS = ZI(JPBARS+IZONE) - ZI(JPBARS+IZONE-1)
        IF (NBARS .EQ. 0) GOTO 56
        DO 52 JJ = 1,NBN
          POSNOE = ZI(JNOMA+ZI(JPONO+POSMA-1)+JJ-1)
          NUMNOE = ZI(JNOCO+POSNOE-1)
          DO 32 II = 1,NBARS
            NUMBAR = ZI(JBARS+ZI(JPBARS+IZONE-1)+II-1)
            IF (NUMNOE .EQ. NUMBAR) THEN
              BARSOU = .TRUE.
              TYPBAR = 0
              INIBA1 = JJ
              INIBA2 = 0
              NOQBA1 = 1
              NOQBA2 = 0
              IMABAR = IMA
              GOTO 52
            END IF
 32       CONTINUE 
 52     CONTINUE
 
      END IF
 56   CONTINUE
C
      CALL JEDEMA()      
      END
