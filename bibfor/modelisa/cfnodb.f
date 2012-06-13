      SUBROUTINE CFNODB(CHAR  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8  CHAR
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - ELIMINATION)
C
C DETECTION DE NOEUDS APPARTENANT A DEUX SURFACES DE CONTACT
C
C DEUX CAS SONT DETECTES : AU SEIN D'UNE MEME ZONE
C                          ENTRE 2 SURFACES ESCLAVES
C
C ----------------------------------------------------------------------
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C
C
C
C
      CHARACTER*24 DEFICO
      LOGICAL      MMINFL,LCALC
      INTEGER      CFDISI,NZOCO ,NNOCO,IFORM,MMINFI
      CHARACTER*24 NODBL ,NODBL2
      INTEGER      JNODBL,JNODB2
      CHARACTER*24 CONTNO,SANSNO, PSANS
      INTEGER      JNOCO ,JSANS ,JPSANS
      INTEGER      IZONE ,IBID  ,NDOUBL,NVDBL
      INTEGER      IZONEA,IZONEB,NVDBA ,NVDBB
      INTEGER      NBNOE ,JDECNE,NBNOEA,NBNOEB
      INTEGER      NBNOM ,JDECNM,JDECEA,JDECEB
      INTEGER      NSANS ,JDECS ,NSANSA,JDECSA,NSANSB,JDECSB
      INTEGER      VALI(3)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      DEFICO = CHAR(1:8)//'.CONTACT'
      NZOCO  = CFDISI(DEFICO,'NZOCO' )
      NNOCO  = CFDISI(DEFICO,'NNOCO' )
      IFORM  = CFDISI(DEFICO,'FORMULATION')
C
C --- OBJETS TEMPORAIRES
C
      NODBL = '&&CFNODB.NODBL'
      CALL WKVECT(NODBL ,'V V I',NNOCO,JNODBL)
      NODBL2 = '&&CFNODB.NODBL2'
      CALL WKVECT(NODBL2,'V V I',NNOCO,JNODB2)
C
C --- ACCES AU TABLEAU DES NOEUDS DE CONTACT
C
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CALL JEVEUO(CONTNO,'L',JNOCO)
      SANSNO = DEFICO(1:16)//'.SSNOCO'
      CALL JEVEUO(SANSNO,'L',JSANS)
      PSANS  = DEFICO(1:16)//'.PSSNOCO'
      CALL JEVEUO(PSANS ,'L',JPSANS)
C
C ----------------------------------------------------------------------
C
C --- PREMIER CAS : NOEUDS COMMUNS DANS UNE MEME ZONE DE CONTACT
C
      DO 100 IZONE = 1,NZOCO
        NBNOE  = MMINFI(DEFICO,'NBNOE' ,IZONE )
        NBNOM  = MMINFI(DEFICO,'NBNOM' ,IZONE )
        JDECNE = MMINFI(DEFICO,'JDECNE',IZONE )
        JDECNM = MMINFI(DEFICO,'JDECNM',IZONE )
        LCALC  = MMINFL(DEFICO,'CALCUL',IZONE )
        IF (.NOT.LCALC) THEN
          GOTO 100
        ENDIF
        CALL UTLISI('INTER',ZI(JNOCO+JDECNE),NBNOE ,
     &                      ZI(JNOCO+JDECNM),NBNOM ,
     &                      ZI(JNODBL)      ,NNOCO ,
     &                      NDOUBL)
        IF (NDOUBL.NE.0) THEN
          IF (NDOUBL.GT.0) THEN
C --------- LES NOEUDS COMMUNS SONT-ILS EXCLUS PAR SANS_NOEUD ?
            NSANS = ZI(JPSANS+IZONE) - ZI(JPSANS+IZONE-1)
            JDECS = ZI(JPSANS+IZONE-1)
            CALL UTLISI('DIFFE',ZI(JNODBL)     ,NDOUBL,
     &                          ZI(JSANS+JDECS),NSANS ,
     &                          IBID           ,1     ,
     &                          NVDBL)
C --------- NON !
            IF (NVDBL.NE.0) THEN
              VALI(1) = IZONE
              VALI(2) = ABS(NVDBL)
              CALL U2MESI('F','CONTACT2_13',2,VALI)
            ENDIF
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
 100  CONTINUE
C
C ----------------------------------------------------------------------
C
C --- SECOND CAS : NOEUDS COMMUNS A DEUX SURFACES ESCLAVES
C
      DO 200 IZONEA = 1,NZOCO
        LCALC = MMINFL(DEFICO,'CALCUL',IZONEA)
        IF (.NOT.LCALC) THEN
          GOTO 200
        ENDIF
        DO 201 IZONEB = IZONEA+1,NZOCO
          LCALC = MMINFL(DEFICO,'CALCUL',IZONEB)
          IF (.NOT.LCALC) THEN
            GOTO 201
          ENDIF
          NBNOEA = MMINFI(DEFICO,'NBNOE' ,IZONEA)
          NBNOEB = MMINFI(DEFICO,'NBNOE' ,IZONEB)
          JDECEA = MMINFI(DEFICO,'JDECNE',IZONEA)
          JDECEB = MMINFI(DEFICO,'JDECNE',IZONEB)
          CALL UTLISI('INTER',ZI(JNOCO+JDECEA),NBNOEA,
     &                        ZI(JNOCO+JDECEB),NBNOEB,
     &                        ZI(JNODBL)      ,NNOCO ,
     &                        NDOUBL)
          IF (NDOUBL.NE.0) THEN
            IF (NDOUBL.GT.0) THEN
              IF (IFORM.EQ.1) THEN
C ------------- LES NOEUDS COMMUNS SONT-ILS EXCLUS PAR LA ZONE A ?
                NSANSA = ZI(JPSANS+IZONEA) - ZI(JPSANS+IZONEA-1)
                JDECSA = ZI(JPSANS+IZONEA-1)
                CALL UTLISI('DIFFE',ZI(JNODBL)      ,NDOUBL,
     &                              ZI(JSANS+JDECSA),NSANSA,
     &                              ZI(JNODB2)      ,NNOCO ,
     &                              NVDBA)
                IF (NVDBA.NE.0) THEN
                  IF (NVDBA.GT.0) THEN
C ----------------- LES NOEUDS RESTANTS SONT-ILS EXCLUS PAR LA ZONE B ?
                    NSANSB = ZI(JPSANS+IZONEB) - ZI(JPSANS+IZONEB-1)
                    JDECSB = ZI(JPSANS+IZONEB-1)
                    CALL UTLISI('DIFFE',ZI(JNODB2)      ,NVDBA ,
     &                                  ZI(JSANS+JDECSB),NSANSB,
     &                                  IBID            ,1     ,
     &                                  NVDBB)
                    IF (NVDBB.NE.0) THEN
                      VALI(1) = IZONEA
                      VALI(2) = IZONEB
                      VALI(3) = ABS(NVDBB)
                      CALL U2MESI('A','CONTACT2_15',3,VALI)
                    ENDIF
                  ELSE
                    CALL ASSERT(.FALSE.)
                  ENDIF
                ENDIF
              ELSEIF (IFORM.EQ.2) THEN
                VALI(1) = IZONEA
                VALI(2) = IZONEB
                VALI(3) = ABS(NDOUBL)
                CALL U2MESI('F','CONTACT2_16',3,VALI)
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF
 201    CONTINUE
 200  CONTINUE
C
C --- MENAGE
C
      CALL JEDETR(NODBL )
      CALL JEDETR(NODBL2)
C
      CALL JEDEMA()
      END
