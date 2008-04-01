      SUBROUTINE MMFOND(NOMA  ,DEFICO,IZONE ,NBNOE ,POSMAE,
     &                  TYPBAR,NUNOBA,NUNFBA,EXNOEB)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      NBNOE       
      INTEGER      POSMAE
      INTEGER      IZONE
      INTEGER      TYPBAR
      INTEGER      NUNOBA(3),NUNFBA(2) 
      LOGICAL      EXNOEB
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
C
C TRAITEMENT DES MAILLES EN FOND DE FISSURE ET DU CAS DES ELEMENTS
C BARSOUM
C      
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
C IN  POSMAE : NUMERO DE LA MAILLE ESCLAVE
C IN  NBNOE  : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C               /!\  CAR SCHEMA INTEGRATION AUX NOEUDS
C OUT EXNOEB : .TRUE. SI NOEUDS DE BARSOUM A TRAITER
C OUT TYPBAR : NOEUDS EXCLUS PAR CET ELEMENT DE BARSOUM
C              CAS TYPBAR = 0:
C                 EXCLUSION DIRECTE D'UN NOEUD PAR GROUP_NO_FOND
C              CAS TYPBAR = 1:
C                 NOEUDS EXCLUS: 1 ET 2 
C                 ON EXCLUE LE NOEUD MILIEU 5
C                 ON EXCLUE LES NOEUDS EN FACE: 3 ET 4
C              CAS TYPBAR = 2: 
C                 NOEUDS EXCLUS: 3 ET 4 
C                 ON EXCLUE LE NOEUD MILIEU 7
C                 ON EXCLUE LES NOEUDS EN FACE: 1 ET 2                
C              CAS TYPBAR = 3: 
C                 NOEUDS EXCLUS: 2 ET 3 
C                 ON EXCLUE LE NOEUD MILIEU 6
C                 ON EXCLUE LES NOEUDS EN FACE: 1 ET 4                
C              CAS TYPBAR = 4: 
C                 NOEUDS EXCLUS: 1 ET 4
C                 ON EXCLUE LE NOEUD MILIEU 8
C                 ON EXCLUE LES NOEUDS EN FACE: 2 ET 3 
C OUT NUNOBA : NUMERO DES NOEUDS (1 A 8) A EXCLURE DU CONTACT 
C                SI MAILLE_FOND: POI1
C                  (1) NUMERO DU NOEUD A EXCLURE
C                SI MAILLE_FOND: SEG3
C                  (1) PREMIER NOEUD A EXCLURE
C                  (2) SECOND NOEUD A EXCLURE
C                  (3) NOEUD MILIEU A EXCLURE
C OUT NONFBA : NUMERO DES NOEUDS (1 A 8) EN FACE
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
      CHARACTER*24 NOMACO,PNOMA,CONTNO
      INTEGER      JNOMA,JPONO,JNOCO,POSNOE,NUMNOE
      INTEGER      IBID,IMABAR,N1,N2,INOE
      CHARACTER*24 PBARS,BARSMA,PBARM,VALK(2)
      CHARACTER*8  ALIAS,NOMMAI
      LOGICAL      BARSO1,BARSO2
      INTEGER      MBARS,NBARS,ILONG,NUBAR,ARTFIS,SUPPOK,NBNOBA
      INTEGER      JPBARS,JBARM,JPBARM,ICONEX
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C    
C --- ACCES OBJETS JEVEUX
C  
      NOMACO = DEFICO(1:16)//'.NOMACO'
      PNOMA  = DEFICO(1:16)//'.PNOMACO'
      CONTNO = DEFICO(1:16)//'.NOEUCO' 
      PBARS  = DEFICO(1:16)//'.PBANOCO'
      BARSMA = DEFICO(1:16)//'.BAMACO'
      PBARM  = DEFICO(1:16)//'.PBAMACO'                       
      CALL JEVEUO(NOMACO,'L',JNOMA)
      CALL JEVEUO(PNOMA ,'L',JPONO)
      CALL JEVEUO(CONTNO,'L',JNOCO)           
      CALL JEVEUO(PBARS ,'L',JPBARS)
      CALL JEVEUO(BARSMA,'L',JBARM)
      CALL JEVEUO(PBARM ,'L',JPBARM)    
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILONG)
      CALL JEVEUO(NOMA(1:8)       //'.CONNEX','L',ICONEX)
C
C --- INITIALISATIONS
C
      BARSO1 = .FALSE.
      BARSO2 = .FALSE.
      EXNOEB = .FALSE.
      TYPBAR = 0
      NBNOBA = 0
      NUNOBA(1) = 0
      NUNOBA(2) = 0
      NUNOBA(3) = 0
      NUNFBA(1) = 0
      NUNFBA(2) = 0
C          
      MBARS = ZI(JPBARM+IZONE) - ZI(JPBARM+IZONE-1)
      NBARS = ZI(JPBARS+IZONE) - ZI(JPBARS+IZONE-1) 
C
C --- ON TESTE SI LA MAILLE EST UNE MAILLE DE FISSURE
C --- GROUP_MA_FOND OU MAILLE_FOND
C
      DO 33 IMABAR  = 1,MBARS 
        NUBAR  = ZI(JBARM+ZI(JPBARM+IZONE-1)+IMABAR-1)
        CALL MMELTY(NOMA,NUBAR,ALIAS,IBID,IBID)
C 
C --- TRAITEMENT DES "POI1" EN FOND DE FISSURE (2D)
C         
        IF (ALIAS.EQ.'PO1') THEN
          N1 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+1)
          DO 53 INOE = 1,NBNOE
            POSNOE = ZI(JNOMA+ZI(JPONO+POSMAE-1)+INOE-1)
            NUMNOE = ZI(JNOCO+POSNOE-1)
            IF (NUMNOE .EQ. N1) THEN
              NBNOBA    = 1
              NUNOBA(1) = INOE
              GOTO 33
            END IF
 53       CONTINUE   
C 
C --- TRAITEMENT DES "SEG3" EN FOND DE FISSURE (3D)
C 
        ELSEIF (ALIAS.EQ.'SG3') THEN
          N1 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+1)
          N2 = ZI(ICONEX+ZI(ILONG-1+NUBAR)-2+2)
          BARSO1 = .FALSE.
          BARSO2 = .FALSE.
          DO 55 INOE = 1,NBNOE
            POSNOE = ZI(JNOMA+ZI(JPONO+POSMAE-1)+INOE-1)
            NUMNOE = ZI(JNOCO+POSNOE-1)
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
            NBNOBA = 1
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
              NUNOBA(3) = 5 
            ELSEIF (ARTFIS.EQ.2) THEN 
              NUNOBA(3) = 6                         
            ELSEIF (ARTFIS.EQ.3) THEN
              NUNOBA(3) = 7                           
            ELSEIF (ARTFIS.EQ.4) THEN
              NUNOBA(3) = 8              
            ELSE
              CALL ASSERT(.FALSE.)             
            ENDIF             
C
C --- NOEUDS EN FACE DU FOND DE FISSURE 
C             
            IF (ARTFIS.EQ.1) THEN 
              IF (NUNOBA(1).EQ.1) THEN 
                NUNFBA(1) = 4
                NUNFBA(2) = 3                
              ELSEIF (NUNOBA(1).EQ.2) THEN
                NUNFBA(1) = 3
                NUNFBA(2) = 4                
              ELSE
                CALL ASSERT(.FALSE.) 
              ENDIF 
            ELSEIF (ARTFIS.EQ.2) THEN 
              IF (NUNOBA(1).EQ.2) THEN 
                NUNFBA(1) = 1
                NUNFBA(2) = 4                
              ELSEIF (NUNOBA(1).EQ.3) THEN
                NUNFBA(1) = 4
                NUNFBA(2) = 1                
              ELSE
                CALL ASSERT(.FALSE.) 
              ENDIF                        
            ELSEIF (ARTFIS.EQ.3) THEN
              IF (NUNOBA(1).EQ.3) THEN 
                NUNFBA(1) = 2
                NUNFBA(2) = 1                
              ELSEIF (NUNOBA(1).EQ.4) THEN
                NUNFBA(1) = 1
                NUNFBA(2) = 2                
              ELSE
                CALL ASSERT(.FALSE.) 
              ENDIF                          
            ELSEIF (ARTFIS.EQ.4) THEN
              IF (NUNOBA(1).EQ.4) THEN 
                NUNFBA(1) = 3
                NUNFBA(2) = 2                
              ELSEIF (NUNOBA(1).EQ.1) THEN
                NUNFBA(1) = 2
                NUNFBA(2) = 3                
              ELSE
                CALL ASSERT(.FALSE.) 
              ENDIF             
            ELSE
              CALL ASSERT(.FALSE.)             
            ENDIF                        
            GOTO 54 
          ENDIF              
        ELSE
          CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUBAR),NOMMAI)
          VALK(1) = NOMMAI
          VALK(2) = ALIAS
          CALL U2MESK('F','CONTACT3_25', 2 ,VALK)          
        END IF
 33   CONTINUE
 54   CONTINUE
C
C ---- ON TESTE SI LA MAILLE CONTIENT UN NOEUD DE FOND DE
C ---- FISSURE GROUP_NO_FOND OU NOEUD_FOND
C
      IF (NBARS .NE. 0) THEN
        DO 52 INOE = 1,NBNOE
          POSNOE = ZI(JNOMA+ZI(JPONO+POSMAE-1)+INOE-1)
          NUMNOE = ZI(JNOCO+POSNOE-1)
          CALL CFMMEX(DEFICO,'FOND',IZONE ,NUMNOE,SUPPOK)
          IF (SUPPOK .EQ. 1) THEN
            NBNOBA    = 1
            TYPBAR    = 0
            NUNOBA(1) = INOE
            NUNOBA(2) = 0
            NUNFBA(1) = 1
            NUNFBA(2) = 0
            GOTO 56
          END IF
 52     CONTINUE
      ENDIF
 56   CONTINUE  
C
      EXNOEB = (NBNOBA.GT.0)
C
      CALL JEDEMA()      
      END
