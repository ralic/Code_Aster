      SUBROUTINE XINILS(IFM,NOMA,METH,NFONF,NFONG,CNSLT,CNSLN)
      IMPLICIT NONE
      INTEGER       IFM
      CHARACTER*8   NOMA,METH,NFONF,NFONG
      CHARACTER*19  CNSLT,CNSLN

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/01/2006   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C                      CALCUL INITIAL DES LEVEL-SETS
C
C    ENTREE :
C              IFM    :   FICHIER D'IMPRESSION
C              NOMA   :   OBJET MAILLAGE
C              METH   :   MÉTHODE DE CALUL DES LEVEL-SETS
C
C    SORTIE : 
C              CNSLN  :   LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
C              CNSLT  :   LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     
      INTEGER       IA,IMA,ITYPMA,JCONX1,JCONX2,JDLIMA,JDLISE,JMA
      INTEGER       NA,NB,NBAR,NBMA,NBSEF,NMAABS,NUNOA,NUNOB
      REAL*8        XLN,XLT
      INTEGER       DIMENS, ADDIM, DIMNO
      INTEGER       IBID,IRET,ME1,ME2,CLSM
      INTEGER       NBNO,INO,JCOOR,NBMAF,I
      INTEGER       JLTSV,JLTSL,JLNSV,JLNSL,AR(12,2)
      REAL*8        VALPU(3)
      REAL*8        NORME,LSNA,LSNB,D,PREREA,LSTA,LSTB
      CHARACTER*8   K8BID,NOMPU(3),TYPMA 
      CHARACTER*16  K16BID
      CHARACTER*19  MAI
      CHARACTER*24  LISMA,LISNO,LISSE
      PARAMETER     (PREREA=1.D-2)
      REAL*8        R8PREM
C
      CALL JEMARQ()
C
      NOMPU(1)='X'
      NOMPU(2)='Y'
      NOMPU(3)='Z'
C
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IRET)
C
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
C
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTSV)
      CALL JEVEUO(CNSLT//'.CNSL','E',JLTSL)
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNSV)
      CALL JEVEUO(CNSLN//'.CNSL','E',JLNSL)
     
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
     
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      DIMENS=ZI(ADDIM-1+6)

      IF (METH.EQ.'FONCTION') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE FONC_LT ET FONC_LN
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 1'
        DO 1 INO=1,NBNO
           DO 12 DIMNO=1, DIMENS
             VALPU(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 12        CONTINUE
           CALL FOINTE('F ',NFONF, DIMENS, NOMPU, VALPU, XLT, IBID )
           CALL FOINTE('F ',NFONG, DIMENS, NOMPU, VALPU, XLN, IBID )
           ZR(JLTSV-1+(INO-1)+1)=XLT
           ZR(JLNSV-1+(INO-1)+1)=XLN
           ZL(JLTSL-1+(INO-1)+1)=.TRUE.
           ZL(JLNSL-1+(INO-1)+1)=.TRUE.             
 1      CONTINUE

      ELSEIF (METH.EQ.'GROUP_MA') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE GROUP_MA_FISS ET GROUP_MA_FOND
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 2'  
        LISMA = '&&XINILS.LISTE_MA_FISSUR'    
        CALL RELIEM(' ',NOMA,'NU_MAILLE','DEFI_FISS',1,1,
     &              'GROUP_MA_FISS','GROUP_MA',LISMA,NBMAF)       
        CALL JEVEUO(LISMA,'L',JDLIMA)

        LISSE = '&&XINILS.LISTE_MA_FONFIS' 
        CALL RELIEM(' ',NOMA,'NU_MAILLE','DEFI_FISS',1,1,
     &              'GROUP_MA_FOND','GROUP_MA',LISSE,NBSEF)  
        CALL JEVEUO(LISSE,'L',JDLISE)

        IF (DIMENS .EQ. 3) THEN
         CALL XLS3D(JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &   NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2)
        ELSE     
         CALL XLS2D(JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &   NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2)
        ENDIF

      ENDIF   
      
C-----------------------------------------------------------------------
C     REAJUSTEMENT DE LSN (BOOK III 06/02/04)
C-----------------------------------------------------------------------

C BUT : ON MODIFIE LES VALEURS DE LSN AUX NOEUDS SI TROP PROCHES DE 0
C ---   POUR ÉVITER LES ERREURS D'INTÉGRATION
     
C     COMPTEUR DES LSN MODIFIÉES
      CLSM=0
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)
C     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
      DO 200 IMA=1,NBMA
        NMAABS=IMA
        ITYPMA=ZI(JMA-1+IMA)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
C      SI MAILLE NON VOLUMIQUE ON CONTINUE À 200
C        IF (TYPMA(1:4).NE.'HEXA'.AND.TYPMA(1:5).NE.'PENTA'.
C     &       AND.TYPMA(1:5).NE.'TETRA') GOTO 200  
C       BOUCLE SUR LES ARETES DE LA MAILLE VOLUMIQUE
        CALL CONARE(TYPMA,AR,NBAR)
        IF (NBAR .GT. 0) THEN
         DO 210 IA=1,NBAR
          NA=AR(IA,1)
          NB=AR(IA,2)
          NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
          NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)
          LSNA=ZR(JLNSV-1+(NUNOA-1)+1)
          LSNB=ZR(JLNSV-1+(NUNOB-1)+1)   
C          IF (LSNA*LSNB.LT.0.D0) THEN
          IF (ABS(LSNA-LSNB).GT.R8PREM()) THEN
            D=LSNA/(LSNA-LSNB)
            IF (ABS(D).LE.PREREA) THEN
C              RÉAJUSTEMENT DE LSNA
               ZR(JLNSV-1+(NUNOA-1)+1)=0.D0
               ZL(JLNSL-1+(NUNOA-1)+1)=.TRUE.
               CLSM=CLSM+1
            ENDIF
            IF (ABS(D-1.D0).LE.(PREREA)) THEN
C              RÉAJUSTEMENT DE LSNB
               ZR(JLNSV-1+(NUNOB-1)+1)=0.D0
               ZL(JLNSL-1+(NUNOB-1)+1)=.TRUE.
               CLSM=CLSM+1
            ENDIF
          ENDIF 
          LSTA=ZR(JLTSV-1+(NUNOA-1)+1)
          LSTB=ZR(JLTSV-1+(NUNOB-1)+1)    
          IF (ABS(LSTA-LSTB).GT.R8PREM()) THEN          
C          IF (LSTA*LSTB.LT.0.D0) THEN
            D=LSTA/(LSTA-LSTB)
            IF (ABS(D).LE.PREREA) THEN
C              RÉAJUSTEMENT DE LSTA
               ZR(JLTSV-1+(NUNOA-1)+1)=0.D0
               ZL(JLTSL-1+(NUNOA-1)+1)=.TRUE.
               CLSM=CLSM+1
            ENDIF
            IF (ABS(D-1.D0).LE.(PREREA)) THEN
C              RÉAJUSTEMENT DE LSTB
               ZR(JLTSV-1+(NUNOB-1)+1)=0.D0
               ZL(JLTSL-1+(NUNOB-1)+1)=.TRUE.
               CLSM=CLSM+1
            ENDIF
          ENDIF
 210     CONTINUE
        ENDIF
 200  CONTINUE
C     CONTROLE ET MIZE A ZERO 
C      DO 300 INO=1,NBNO
C        IF (ZL(JLNSL-1+(INO-1)+1).AND.
C     &      ABS(ZR(JLNSV-1+(INO-1)+1)).LE.1.D-16) THEN
C          ZR(JLNSV-1+(INO-1)+1)=0.D0
C          CLSM=CLSM+1
C        ENDIF
C 300  CONTINUE
      WRITE(IFM,*)'NOMBRE DE LSN REAJUSTEES APRES CONTROLE:',CLSM
      WRITE(IFM,*)'FIN DU CALCUL DES LEVEL-SETS'

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------

      CALL JEDEMA()
      END
