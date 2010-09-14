      SUBROUTINE MMFOND(NOMA  ,DEFICO,IZONE ,POSMAE,TYPBAR)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      CHARACTER*24 DEFICO
      CHARACTER*8  NOMA      
      INTEGER      POSMAE
      INTEGER      IZONE
      INTEGER      TYPBAR
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - LECTURE DONNEES)
C
C INDICATEUR QU'UNE MAILLE ESCLAVE EST DE TYPE BARSOUM
C      
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
C IN  POSMAE : NUMERO DE LA MAILLE ESCLAVE
C OUT TYPBAR : NOEUDS EXCLUS PAR CET ELEMENT DE BARSOUM
C               TYPBAR = 0
C                PAS DE FOND DE FISSURE
C               TYPBAR = 1
C                QUAD 8 - FOND FISSURE: 1-2
C               TYPBAR = 2
C                QUAD 8 - FOND FISSURE: 2-3
C               TYPBAR = 3
C                QUAD 8 - FOND FISSURE: 3-4
C               TYPBAR = 4
C                QUAD 8 - FOND FISSURE: 4-1
C               TYPBAR = 5
C                SEG 3  - FOND FISSURE: 1
C               TYPBAR = 6
C                SEG 3  - FOND FISSURE: 2
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32  JEXNUM
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
      INTEGER      IBID,IMABAR,N1,N2,INOE
      CHARACTER*24 PBARS,BARSMA,PBARM
      CHARACTER*8  ALIAS,ELTCTC,NOMMAI,NOMNOE
      INTEGER      NNOMAE,NUMNOE
      INTEGER      POSNNO(9),NUMNNO(9)
      INTEGER      NUMMAE
      LOGICAL      BARSO1,BARSO2
      INTEGER      NUNOBA(2)
      INTEGER      MBARS,NBARS,ILONG,NUBAR,ARTFIS,SUPPOK
      INTEGER      JPBARS,JBARM,JPBARM,ICONEX
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C 
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C      
      PBARS  = DEFICO(1:16)//'.PBANOCO'
      BARSMA = DEFICO(1:16)//'.BAMACO'
      PBARM  = DEFICO(1:16)//'.PBAMACO'         
      CALL JEVEUO(PBARS ,'L',JPBARS)
      CALL JEVEUO(BARSMA,'L',JBARM)
      CALL JEVEUO(PBARM ,'L',JPBARM) 
C         
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILONG)
      CALL JEVEUO(NOMA(1:8)       //'.CONNEX','L',ICONEX)
C
C --- INITIALISATIONS
C
      BARSO1 = .FALSE.
      BARSO2 = .FALSE.
      TYPBAR = 0
C
C --- INFOS SUR LA MAILLE DE CONTACT   
C  
      CALL CFNUMM(DEFICO,1     ,POSMAE,NUMMAE)
      CALL MMELTY(NOMA  ,NUMMAE,ELTCTC,IBID  ,IBID  )
      CALL CFPOSN(DEFICO,POSMAE,POSNNO,NNOMAE)
      CALL ASSERT(NNOMAE.LE.9)
      CALL CFNUMN(DEFICO,NNOMAE,POSNNO,NUMNNO)
      
      CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMAE),NOMMAI)
C          
      MBARS  = ZI(JPBARM+IZONE) - ZI(JPBARM+IZONE-1) 
C
C --- ON TESTE SI LA MAILLE EST UNE MAILLE DE FISSURE
C --- GROUP_MA_FOND OU MAILLE_FOND
C
      DO 33 IMABAR  = 1,MBARS 
C
C --- MAILLE DE FOND DONNEE
C      
        NUBAR  = ZI(JBARM+ZI(JPBARM+IZONE-1)+IMABAR-1)
        CALL MMELTY(NOMA  ,NUBAR ,ALIAS ,IBID  ,IBID  )
        CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUBAR),NOMMAI)
C 
C --- TRAITEMENT DES "POI1" EN FOND DE FISSURE (2D)
C         
        IF (ALIAS.EQ.'PO1') THEN
          N1     = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+1)
          NUMNOE = NUMNNO(1)
          IF (NUMNOE .EQ. N1) THEN
            TYPBAR    = 5
            GOTO 33
          END IF   
          NUMNOE = NUMNNO(2)                 
          IF (NUMNOE .EQ. N1) THEN
            TYPBAR    = 6
            GOTO 33
          END IF
          CALL U2MESS('F','CONTACT3_2')      
C 
C --- TRAITEMENT DES "SEG3" EN FOND DE FISSURE (3D)
C 
        ELSEIF (ALIAS.EQ.'SE3') THEN
          N1 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+1)
          N2 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+2)
          BARSO1 = .FALSE.
          BARSO2 = .FALSE.
          DO 55 INOE = 1,NNOMAE
            NUMNOE = NUMNNO(INOE)          
            IF (NUMNOE .EQ. N1) THEN
              BARSO1 = .TRUE.
              NUNOBA(1) = INOE 
            ELSEIF (BARSO1 .AND. NUMNOE .EQ. N2) THEN
              BARSO2 = .TRUE.
              NUNOBA(2) = INOE 
            END IF
 55       CONTINUE
C
C --- ON TRAITE LES QUAD 8 QUI DONNENT CES SEG3 EN FOND DE FISSURE
C
          IF (BARSO1 .AND. BARSO2) THEN
C
C --- REPERAGE ARETE FOND FISSURE
C
            IF (((NUNOBA(1).EQ.1).AND.(NUNOBA(2).EQ.2)).OR.
     &          ((NUNOBA(1).EQ.2).AND.(NUNOBA(2).EQ.1))) THEN
              ARTFIS = 1
            ELSEIF (((NUNOBA(1).EQ.2).AND.(NUNOBA(2).EQ.3)).OR.
     &              ((NUNOBA(1).EQ.3).AND.(NUNOBA(2).EQ.2))) THEN
              ARTFIS = 2
            ELSEIF (((NUNOBA(1).EQ.3).AND.(NUNOBA(2).EQ.4)).OR.
     &              ((NUNOBA(1).EQ.4).AND.(NUNOBA(2).EQ.3))) THEN
              ARTFIS = 3
            ELSEIF (((NUNOBA(1).EQ.4).AND.(NUNOBA(2).EQ.1)).OR.
     &              ((NUNOBA(1).EQ.1).AND.(NUNOBA(2).EQ.4))) THEN
              ARTFIS = 4
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
C
C --- NOEUD MILIEU DU FOND FISSURE            
C                 
            IF (ARTFIS.EQ.1) THEN 
              TYPBAR = 1
            ELSEIF (ARTFIS.EQ.2) THEN   
              TYPBAR = 2                   
            ELSEIF (ARTFIS.EQ.3) THEN  
              TYPBAR = 3                      
            ELSEIF (ARTFIS.EQ.4) THEN
              TYPBAR = 4            
            ELSE
              CALL ASSERT(.FALSE.)             
            ENDIF   
          ENDIF         
        END IF
 33   CONTINUE
C
C ---- ON TESTE SI LA MAILLE CONTIENT UN NOEUD DE FOND DE
C ---- FISSURE GROUP_NO_FOND OU NOEUD_FOND
C
       NBARS = ZI(JPBARS+IZONE) - ZI(JPBARS+IZONE-1) 
       IF (NBARS .NE. 0) THEN  
        IF (ELTCTC(1:2).EQ.'SE') THEN
          NUMNOE = NUMNNO(1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUMNOE),NOMNOE)        
          CALL CFMMEX(DEFICO,'FOND',IZONE ,NUMNOE,SUPPOK)
          IF (SUPPOK .EQ. 1) THEN
            TYPBAR    = 5         
            GOTO 56
          END IF
          NUMNOE = NUMNNO(2)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUMNOE),NOMNOE)       
          CALL CFMMEX(DEFICO,'FOND',IZONE ,NUMNOE,SUPPOK)
          IF (SUPPOK .EQ. 1) THEN
            TYPBAR    = 6
            GOTO 56
          END IF 
        ENDIF
      ENDIF
 56   CONTINUE  
C
      CALL JEDEMA()      
      END
