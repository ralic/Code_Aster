      SUBROUTINE NDLOAM()
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8  RESULT
      REAL*8       INSTIN
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (DYNAMIQUE)
C
C LECTURE DES DEPL/VITE/ACCEL GENRALISES DANS SD_RESULT
C      
C ----------------------------------------------------------------------
C
C  
C IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT  
C IN  INSTIN : PREMIER INSTANT DE CALCUL
C IN  NUMEDD : NUME_DDL
C IN  NUMINS : NUMERO INSTANT COURANT
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  SDDYNA : SD DYNAMIQUE 
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C     
      REAL*8       PREC,EPS0
      CHARACTER*8  K8BID,CTYPE
      COMPLEX*16   C16BID
      INTEGER      IBID,NUME,NBR,IRET
      CHARACTER*8  TRGENE
      INTEGER      JTRGEN
      LOGICAL      LCREA
      INTEGER      IFM,NIV         
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... LECTURE PROJ. MODALE' 
      ENDIF
C
C --- INITIALISATIONS
C
      LCREA = .FALSE.  
      CTYPE = 'K24'   
C
C --- RECUPERATION NUMERO D'ORDRE DANS SD RESULT
C
      PREC = 1.D-6
      EPS0 = 1.D-12
      IF (ABS(INSTIN).GT.EPS0) THEN
        CALL RSORAC(RESULT,'INST',IBID  ,INSTIN,K8BID ,
     &              C16BID,PREC  ,'RELATIF',NUME,1,NBR)
      ELSE
        CALL RSORAC(RESULT,'INST',IBID  ,INSTIN,K8BID ,
     &              C16BID,EPS0  ,'ABSOLU' ,NUME,1,NBR)
      ENDIF
C
C --- EXISTENCE DU PARAMETRE DANS SD_RESULTAT
C 
      CALL RSADPA(RESULT,'L',1,'TRAN_GENE',NUME,1,JTRGEN,CTYPE)
      TRGENE = ZK24(JTRGEN)
      CALL JEEXIN(TRGENE,IRET)
      IF (IRET.EQ.0) THEN
        LCREA = .TRUE.
      ENDIF
C
C --- CREATION DU TRAN_GENE
C
      IF (LCREA) THEN
C
      ENDIF
 
C    
      CALL JEDEMA()

      END
