      SUBROUTINE ERGLHM (PERMAN,JCELD,IAVALE,IORD,LIGREL,
     &                   LONGT ,NBGR ,RESUC1)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      JCELD,IAVALE,IORD,LONGT,NBGR
      CHARACTER*19 LIGREL,RESUC1
      LOGICAL      PERMAN
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/10/2012   AUTEUR DELMAS J.DELMAS 
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
C =====================================================================
C  ERREUR GLOBALE AU MAILLAGE - HYDRO-MECANIQUE
C  **     **                    *     *
C =====================================================================
C     BUT:
C         CALCUL DES ESTIMATEURS GLOBAUX POUR L'HYDRO-MECANIQUE
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   PERMAN : MODELISATION THM PERMANENTE ?
C IN   JCELD  : ADRESSE DU DESCRIPTEUR DU CHAMP LOCAL
C IN   IAVALE : ADRESSE DES CHAMPS LOCAUX
C IN   IORD   : NUMERO D'ORDRE
C IN   LIGREL : NOM DU LIGREL
C IN   LONGT  : NOMBRE DE CHAMPS LOCAUX
C IN   NBGR   : NOMBRE DE GRELS
C IN   RESUC1 : NOM DU CONCEPT RESULTAT DE LA COMMANDE CALC_ERREUR
C
C      SORTIE :
C-------------
C
C ......................................................................

C
C
      INTEGER NBELEM,NEL,IAUX
      INTEGER MODE,II,K,J,IAD,IDECGR
      INTEGER LJEVEU(6)
C
      CHARACTER*1  KBID
      CHARACTER*16 LPARTR(6)
      CHARACTER*16 LPARST(3)
C
      REAL*8       TABERR(6),TABERM(6),TABER2(7)
C
C     ------------------------------------------------------------------
      DATA LPARTR / 'ERRE_MEC_LOC' ,'ERRE_MEC_LOC_D' ,'ERRE_HYD_LOC',
     &              'ERRE_MEC_GLOB','ERRE_MEC_GLOB_D','ERRE_HYD_GLOB' /
      DATA LPARST / 'ERRE_MEC'     ,'ERRE_HYD_S'     ,'ERRE_HYD_D' /
C     ------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IF (.NOT.PERMAN) THEN
C
        CALL RSADPA(RESUC1,'L',6,LPARTR,IORD-1,0,LJEVEU,KBID)
C
        DO 40 , IAUX = 1 , 6
          IF (IORD.EQ.1) THEN
            TABERM(IAUX) = 0.D0
          ELSE
            TABERM(IAUX) = ZR(LJEVEU(IAUX))
          ENDIF
 40     CONTINUE
      ENDIF
C
C =======================================================
C 1. CALCUL DES TERMES GLOBAUX POUR CHACUN DES PARAMETRES
C    DE L'OPTION
C =======================================================
C
      DO 10 , II = 1,LONGT
C
        TABER2(II) = 0.D0
C
        DO 20 , J = 1,NBGR
C
          MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
          IF (MODE.EQ.0) GOTO 20
          NEL = NBELEM(LIGREL,J)
          IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
C
          DO 30 , K = 1,NEL
C
            IAD = IAVALE-1+IDECGR+(K-1)*LONGT
            TABER2(II) = TABER2(II) + ZR(IAD+II-1)
C
 30       CONTINUE
C
 20     CONTINUE
C
        TABER2(II) = SQRT(TABER2(II))
C
 10   CONTINUE
C
C ==================================================
C 2. CALCUL DES TERMES GLOBAUX
C ==================================================
C
      IF (PERMAN) THEN
C
        TABERR(1) = TABER2(4)
        TABERR(2) = TABER2(3)
        TABERR(3) = TABER2(5)
C
      ELSE
C
        TABERR(1) = TABER2(2)
        TABERR(2) = TABER2(3)
        TABERR(3) = TABER2(4)
        TABERR(4) = MAX(TABERM(4),TABER2(2))
        TABERR(5) = TABERM(5) + TABER2(3)
        TABERR(6) = SQRT( TABERM(6)**2 + TABER2(4)**2 )
C
      ENDIF
C
C ===================================================
C 3. ARCHIVAGE DES RESULTATS DANS LA SD RESULTAT
C ===================================================
C
      IF (PERMAN) THEN
C
        CALL RSADPA(RESUC1,'E',3,LPARST,IORD,0,LJEVEU,KBID)
C
        DO 50 , IAUX = 1 , 3
          ZR(LJEVEU(IAUX)) = TABERR(IAUX)
 50     CONTINUE
C
      ELSE
C
        CALL RSADPA(RESUC1,'E',6,LPARTR,IORD,0,LJEVEU,KBID)
C
        DO 60 , IAUX = 1 , 6
          ZR(LJEVEU(IAUX)) = TABERR(IAUX)
 60     CONTINUE
C
      ENDIF
C
      CALL JEDEMA()
C
      END
