      SUBROUTINE FETPRJ(NBI,VI,VO,GI,JGITGI,LRIGID,DIMGI,OPTION)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/08/2004   AUTEUR BOITEAU O.BOITEAU 
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL AU SENS FETI DE:
C          * LA PROJECTION COMPLETE P=I-GI.(GIT.GI)-1.GIT SI OPTION=1
C               P(VI) EST STOCKEE DANS LES NBI COMPOSANTES DE VO
C          * LA PROJECTION PARTIELLE P'=(GIT.GI)-1.GIT SI OPTION=2
C               P'(VI) EST STOCKEE DANS LES DIMGI PREMIERES COMPOSANTES
C                DE VO
C
C      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
C      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NBI
C      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NBI SI OPTION=1
C      IN    GI : MATR8: MATRICE GI
C      IN JGITGI: IN  : ADRESSE OBJET JEVEUX (GI)T*GI
C      IN LRIGID: LO  : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
C         SOUS-DOMAINES FLOTTANT
C      IN  DIMGI:  IN : TAILLE DE GIT*GI
C      IN OPTION:  IN  : 1 -> PROJECTION., 2-> RECONSTRUCTION ALPHA SOL.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       28/01/04 (OB): CREATION.
C       04/06/04 (OB): MODIFICATION POUR MODES DE CORPS RIGIDES.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBI,JGITGI,DIMGI,OPTION
      REAL*8       VI(NBI),VO(NBI),GI(NBI,DIMGI)
      LOGICAL      LRIGID

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER      IFM,NIV,JGITVI,JGITV1,NGITGI,NGITG2,JA,I,J,IDECAI,K,
     &             NB,IDECAO,L,M
      REAL*8       DET,RAUX     
      LOGICAL      IRET
      
C CORPS DU PROGRAMME
      CALL JEMARQ()
        
C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)

      IF ((OPTION.NE.1).AND.(OPTION.NE.2))
     &  CALL UTMESS('F','FETPRJ','OPTION DE CALCUL NON PREVUE !')

C---------------------------------------------------------------------
C --------------------------------------------------------------------
C AUCUN MODE DE CORPS RIGIDE P=ID (OPTION=1 OBLIGEATOIRE)
C --------------------------------------------------------------------
C---------------------------------------------------------------------
      IF (.NOT.LRIGID) THEN
        IF (OPTION.NE.1)
     &    CALL UTMESS('F','FETPRJ','OPTION DE CALCUL INCOHERENTE !')
   
        DO 10 I=1,NBI
          VO(I)=VI(I)
   10   CONTINUE
   
      ELSE
C---------------------------------------------------------------------
C --------------------------------------------------------------------
C PRESENCE DE MODES DE CORPS RIGIDES P (OPTION=1) OU P' (OPTION=2)
C --------------------------------------------------------------------
C---------------------------------------------------------------------

C INITS.
        IRET=.TRUE.
        DET=-999.D0
C --------------------------------------------------------------------
C CONSTITUTION DE (GI)T*VI STOCKE DANS '&&FETPRJ.GITVI.R'
C --------------------------------------------------------------------
C VECTEUR AUXILIAIRE
        CALL WKVECT('&&FETPRJ.GITVI.R','V V R',DIMGI,JGITVI)
        JGITV1=JGITVI-1
        
        DO 20 I=1,DIMGI
          ZR(JGITV1+I)=0.D0
          DO 19 J=1,NBI
            ZR(JGITV1+I)=ZR(JGITV1+I)+GI(J,I)*VI(J)
   19     CONTINUE
   20   CONTINUE
   
        
C MONITORING
        IF (NIV.GE.3) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          WRITE(IFM,*)'<FETI/FETPRJ> CONSTRUCTION DE (GI)T*LAMBDA'
        ENDIF
        
C --------------------------------------------------------------------
C CONSTITUTION DE ((GI)T*GI)-1*(GI)T*VI STOCKE DANS '&&FETPRJ.GITGI.R'
C --------------------------------------------------------------------
C VECTEUR AUXILIAIRE CONTENANT A=(GI)T*GI DESYMETRISEE
        NGITGI=(DIMGI*(DIMGI+1)/2)-1
        NGITG2=DIMGI*DIMGI      
        CALL WKVECT('&&FETPRJ.GITGI.R','V V R',NGITG2,JA)

C DESYMETRISE (GI)T*GI DANS A CAR MTGAUSS ECRASE LA MATRICE A INVERSER
        I=1
        J=1
        DO 100 K=0,NGITGI
          IDECAI=(J-1)*DIMGI+I-1
          ZR(JA+IDECAI)=ZR(JGITGI+K)
          IF (I.NE.J) THEN
            IDECAI=(I-1)*DIMGI+J-1
            ZR(JA+IDECAI)=ZR(JGITGI+K)      
          ENDIF
          I=I+1
          IF (I.GT.DIMGI) THEN
            J=J+1
            I=J
           ENDIF
  100   CONTINUE
                
        NB=1
        CALL MGAUSS(ZR(JA),ZR(JGITVI),DIMGI,DIMGI,NB,DET,IRET)
        
C MONITORING
        IF (NIV.GE.3) THEN
          WRITE(IFM,*)'<FETI/FETPRJ> INVERSION (GITGI)-1*...'   
          WRITE(IFM,*)'<FETI/FETPRJ> DET/IRET ',DET,' ',IRET      
        ENDIF
        IF (.NOT.IRET) THEN
          CALL UTDEBM('F','FETPRJ','SYSTEME (GI)T*GI PROBABLEMENT')
          CALL UTIMPI('S','  NON INVERSIBLE: ',0,I)
          CALL UTIMPR('L','DETERMINANT APPROCHE: ',1,DET)
          CALL UTFINM()
        ENDIF
        
        IF (OPTION.EQ.1) THEN
C --------------------------------------------------------------------
C CONSTITUTION DE V0=VI-GI*((GI)T*GI)-1*(GI)T*VI (OPTION=1)
C --------------------------------------------------------------------
          DO 150 I=1,NBI
            VO(I)=VI(I)
  150     CONTINUE
  
          DO 201 J=1,DIMGI
            RAUX=ZR(JGITV1+J)
            DO 200 I=1,NBI
              VO(I)=VO(I)-GI(I,J)*RAUX
  200       CONTINUE
  201     CONTINUE

C MONITORING
          IF (NIV.GE.5) THEN
            WRITE(IFM,*)'<FETI/FETPRJ> LAMBDA = P * LAMBDA'
            IF (NIV.GE.5) THEN
              DO 300 I=1,NBI
                WRITE(IFM,*)I,'  ',VI(I),' ',VO(I)
  300         CONTINUE
            ENDIF         
            WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
            WRITE(IFM,*)
          ENDIF
          
        ELSE
C --------------------------------------------------------------------
C CONSTITUTION DE V0=((GI)T*GI)-1*(GI)T*VI (OPTION=2)
C --------------------------------------------------------------------
          DO 310 I=1,DIMGI
            VO(I)=ZR(JGITV1+I)
  310     CONTINUE

C MONITORING
          IF (NIV.GE.5) THEN
            WRITE(IFM,*)'<FETI/FETPRJ> ALPHA = P'' * RESIDU'
            IF (NIV.GE.5) THEN
              DO 320 I=1,DIMGI
                WRITE(IFM,*)I,'  ',VI(I),' ',VO(I)
  320         CONTINUE
            ENDIF         
            WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
            WRITE(IFM,*)
          ENDIF
                        
        ENDIF   
C DESTRUCTION OBJET TEMPORAIRE  
        CALL JEDETR('&&FETPRJ.GITVI.R')
        CALL JEDETR('&&FETPRJ.GITGI.R')                          
      ENDIF
                
      CALL JEDEMA()
      END
