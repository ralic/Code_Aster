      SUBROUTINE PJEFTC(MA1,MA2,RESUOU,BASE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE GREFFET N.GREFFET
C ======================================================================
C     COMMANDE:  PROJ_CHAMP  METHODE:'COUPLAGE' (COUPLAGE IFS VIA YACS)
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
      INCLUDE 'jeveux.h'
      CHARACTER*8  MA1, MA2
      CHARACTER*16 RESUOU
      CHARACTER*1 BASE
C
C 0.2. ==> COMMUNS


C
C 0.3. ==> VARIABLES LOCALES
      INTEGER      NBVAL, NBOCC, IOCC, NBGNO2, VALI(2)
      CHARACTER*1  KBID
      CHARACTER*16 NOMGMA, NOMGNO
      CHARACTER*16 CORRE1,CORRE2,CORRE3
      INTEGER      IARG

C DEB ------------------------------------------------------------------
      CALL JEMARQ()
      CALL ASSERT(BASE.EQ.'V')
      CORRE1 = '&&PJEFTC.CORRES1'
      CORRE2 = '&&PJEFTC.CORRES2'
      CORRE3 = '&&PJEFTC.CORRES3'


C     NOMBRE D'OCCURENCE (ou NOMBRE DE GROUP_MAILLE DEFINIS)
C     ------------------------------------------------------
      CALL GETFAC('VIS_A_VIS',NBOCC)

C     COHERENCE ENTRE GROUP_MAILLE ET GOURP_NOEUDS
C     --------------------------------------------
      CALL JELIRA(MA2//'.GROUPENO','NMAXOC',NBGNO2,KBID)
      IF (NBGNO2.NE.NBOCC) THEN
        VALI(1) = NBGNO2
        VALI(2) = NBOCC
        CALL U2MESI('F','COUPLAGEIFS_8',2,VALI)
      ENDIF

C     PROJECTION ENTRE GROUP_MAILLE ET GROUP_NOEUDS
C     ---------------------------------------------
      IF (NBOCC.GT.0) THEN

        DO 10 IOCC = 1,NBOCC

C         -- NOMS DES GROUPES DE MAILLES ET DE NOEUDS COUPLES :
C         -----------------------------------------------------------
          CALL GETVTX('VIS_A_VIS','GROUP_MA_1',IOCC,IARG,1,NOMGMA,NBVAL)
          CALL GETVTX('VIS_A_VIS','GROUP_NO_2',IOCC,IARG,1,NOMGNO,NBVAL)

C         -- CALCUL DU CORRESP_2_MAILLA POUR IOCC :
C         ----------------------------------------------
          CALL PJECOU(MA1, MA2, NOMGMA, NOMGNO, CORRE1)

C        -- SURCHARGE DU CORRESP_2_MAILLA :
C        ----------------------------------------------
          IF (IOCC.EQ.1) THEN
            CALL COPISD('CORRESP_2_MAILLA','V',CORRE1,CORRE2)
          ELSE
            CALL PJFUC2(CORRE2,CORRE1,'V',CORRE3)
            CALL DETRSD('CORRESP_2_MAILLA',CORRE2)
            CALL COPISD('CORRESP_2_MAILLA','V',CORRE3,CORRE2)
            CALL DETRSD('CORRESP_2_MAILLA',CORRE3)
          END IF
          CALL DETRSD('CORRESP_2_MAILLA',CORRE1)
 10     CONTINUE
        CALL COPISD('CORRESP_2_MAILLA','G',CORRE2,RESUOU)
        CALL DETRSD('CORRESP_2_MAILLA',CORRE2)
      ELSE
         CALL U2MESS('F','COUPLAGEIFS_2')
      END IF

      CALL JEDEMA()
      END
