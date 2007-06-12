      SUBROUTINE REDCEX(DEFICO,NOMA,INI,POSMA,IZONE,NORMA,VECNTD,
     &                  NEQ,VECNTX,VECNTY,VECNTZ,EXNOE,IFM,
     &                  PREMIE)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR TORKHANI M.TORKHANI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
      IMPLICIT NONE
      INTEGER INI,POSMA,NEQ,IZONE,IFM
      REAL*8 NORMA(3)
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO
      LOGICAL EXNOE,PREMIE
C
C ---------------------------------------------------------------------
C ROUTINE APPELEE PAR : MMMBCA
C ---------------------------------------------------------------------
C DETECTION DE RELATIONS DE CONTACT REDONDANTES AVEC
C LES CONDITIONS AUX LIMITES DDL_IMPO ET LIAISON_DDL
C
C VAR DEFICO      : SD POUR LA DEFINITION DE CONTACT
C IN  INI                : NUMERO D'ORDRE DU NOEUD
C IN  POSMA        : POSITION DE LA MAILLE DANS TABLEAU
C IN  IZONE          : ZONE DE CONTACT  
C IN  NORMA        : VECTEUR NORMAL AU NOEUD MAITRE APPARIE
C IN  NEQ             :
C IN  VECNOD      : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                            DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C IN VECNOX     : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                            DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C                            PORTANT SUR LE DDL 'DX'
C IN VECNOY     : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                            DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C                            PORTANT SUR LE DDL 'DY'
C IN VECNOZ     : VECTEUR CONTENANT LES NOEUDS IMPLIQUES
C                            DANS DES RELATIONS DDL_IMPO OU LIAISON_DDL
C                            PORTANT SUR LE DDL 'DZ'
C OUT EXNOE       : VAUT .TRUE. SI LE NOEUD DOIT ETRE EXCLU DE 
C                            LA SURFACE DE CONTACT (PIVOT NUL)
C
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8,ALIAS
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER JNOCO,JDIM,JNOMA,JPONO,NDIM,JMETH
      INTEGER POSNOE,NUMNOE
      INTEGER VECNTD(1,*),VECNTX(1,*),VECNTY(1,*),VECNTZ(1,*)
      INTEGER I,J,K,NUMAES,JMACO,JDEC
      REAL*8 VECT(3),PROV(3),NPROV
      CHARACTER*8  NOMAES,NONOES
      CHARACTER*24 CONTNO,PNOMA,NOMACO,NDIMCO,METHCO,COTAMA
      LOGICAL EXNOX,EXNOY,EXNOZ
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      
      CONTNO = DEFICO(1:16) // '.NOEUCO'
      NDIMCO = DEFICO(1:16) // '.NDIMCO'
      NOMACO = DEFICO(1:16) // '.NOMACO'
      PNOMA  = DEFICO(1:16) // '.PNOMACO'
      METHCO = DEFICO(1:16) // '.METHCO'
      COTAMA = DEFICO(1:16) //'.MAILCO'
      
      CALL JEVEUO(CONTNO, 'L', JNOCO)
      CALL JEVEUO(NDIMCO,  'E', JDIM)
      CALL JEVEUO(NOMACO,'L', JNOMA)
      CALL JEVEUO(PNOMA,   'L', JPONO)
      CALL JEVEUO(METHCO, 'L', JMETH)
      CALL JEVEUO(COTAMA, 'L', JMACO)
      
      NDIM   = ZI(JDIM)
      NUMAES = ZI(JMACO+POSMA-1)
      JDEC   = ZI(JPONO+POSMA-1)
      POSNOE = ZI(JNOMA+JDEC+INI-1)
      NUMNOE = ZI(JNOCO+POSNOE-1)
      EXNOX= .FALSE. 
      EXNOY= .FALSE.
      EXNOZ= .FALSE.
      
      DO 11, I=(1+NEQ),(2*NEQ)
        IF(NUMNOE .EQ. VECNTD(1,I)) THEN
          EXNOE =.TRUE.
        ENDIF
11    CONTINUE      
      
      DO 12, K=1,NEQ
        IF (NUMNOE .EQ. VECNTD(1,K)) THEN
          IF(VECNTX(1,K) .EQ. 1) THEN
            EXNOX =.TRUE.
          ENDIF
          IF(VECNTY(1,K) .EQ. 1) THEN
            EXNOY =.TRUE.
          ENDIF           
          IF(VECNTZ(1,K) .EQ. 1) THEN
            EXNOZ =.TRUE.
          ENDIF
        ENDIF  
12    CONTINUE
      
      IF (NDIM.EQ.2) THEN
        
        IF (EXNOX .AND. EXNOY) EXNOE=.TRUE.
             IF (EXNOX .AND.(.NOT. EXNOY)) THEN 
           VECT(1)= 1.D0
           VECT(2)= 0.D0
           VECT(3)= 0.D0                 
           CALL PROVEC(NORMA,VECT,PROV)
           NPROV=0.D0
           DO 13, J=1,3
             NPROV=PROV(J)*PROV(J)+NPROV 
13         CONTINUE                  
           IF(NPROV .EQ. 0.D0) EXNOE = .TRUE.              
        ENDIF
             IF ((.NOT. EXNOX) .AND. EXNOY) THEN 
           VECT(1)= 0.D0
           VECT(2)= 1.D0
           VECT(3)= 0.D0                 
           CALL PROVEC(NORMA,VECT,PROV)
           NPROV=0.D0
           DO 14, J=1,3
             NPROV=PROV(J)*PROV(J)+NPROV 
14         CONTINUE                  
           IF(NPROV .EQ. 0.D0) EXNOE = .TRUE.              
        ENDIF
      
      ELSE IF (NDIM.EQ.3) THEN
        
        IF (EXNOX .AND. EXNOY .AND.  EXNOZ) EXNOE=.TRUE.
             IF (EXNOX .AND. EXNOY .AND.(.NOT. EXNOZ)) THEN 
          IF(NORMA(3) .EQ. 0.D0) EXNOE = .TRUE.            
        ENDIF
        IF (EXNOX .AND. EXNOZ .AND.(.NOT. EXNOY)) THEN 
          IF(NORMA(2) .EQ. 0.D0) EXNOE = .TRUE.            
        ENDIF
        IF (EXNOY .AND. EXNOZ .AND.(.NOT. EXNOX)) THEN 
          IF(NORMA(1) .EQ. 0.D0) EXNOE = .TRUE.            
        ENDIF
             IF (EXNOX .AND.(.NOT. EXNOY).AND.(.NOT. EXNOZ)) THEN 
           VECT(1)= 1.D0
           VECT(2)= 0.D0
           VECT(3)= 0.D0                 
           CALL PROVEC(NORMA,VECT,PROV)
           NPROV=0.D0
           DO 15, J=1,3
             NPROV=PROV(J)*PROV(J)+NPROV 
15         CONTINUE                  
           IF(NPROV .EQ. 0.D0) EXNOE = .TRUE.              
        ENDIF
        IF ((.NOT. EXNOX) .AND. EXNOY .AND.(.NOT. EXNOZ)) THEN 
           VECT(1)= 0.D0
           VECT(2)= 1.D0
           VECT(3)= 0.D0   
           CALL PROVEC(NORMA,VECT,PROV)
           NPROV=0.D0
           DO 16, J=1,3
              NPROV=PROV(J)*PROV(J)+NPROV 
16      CONTINUE                     
           IF(NPROV .EQ. 0.D0) EXNOE = .TRUE.     
        ENDIF
        IF ((.NOT. EXNOX) .AND. (.NOT. EXNOY).AND. EXNOZ) THEN 
           VECT(1)= 0.D0
           VECT(2)= 0.D0
           VECT(3)= 1.D0
           CALL PROVEC(NORMA,VECT,PROV)
           NPROV=0.D0
           DO 17, J=1,3
              NPROV=PROV(J)*PROV(J)+NPROV 
17      CONTINUE                     
           IF(NPROV .EQ. 0.D0) EXNOE = .TRUE.     
        ENDIF       
      
      ENDIF
      
      IF (EXNOE .AND. PREMIE) THEN
        CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAES),NOMAES)
        CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUMNOE),NONOES)
        WRITE(IFM,1000) 
        WRITE(IFM,2000)  
        WRITE(IFM,3000) NONOES 
        WRITE(IFM,4000) NOMAES
                      
 1000   FORMAT (' <CONTACT>     * ATTENTION EXCLUSION AUTOMATIQUE')
 2000   FORMAT (' <CONTACT>     ** PAR LA DETECTION DES PIVOTS NULS')
 3000   FORMAT (' <CONTACT>     ** DU NOEUD ESCLAVE ',A8) 
 4000   FORMAT (' <CONTACT>     ** DE LA MAILLE ESCLAVE ',A8) 
 
      END IF
            
      CALL JEDEMA()
      END
