      SUBROUTINE FETINL(NBI,VLAGI,GI,JGITGI,MATAS,CHSECM,LRIGID,DIMGI,
     &                  NBSD,VSDF,VDDL)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2004   AUTEUR BOITEAU O.BOITEAU 
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
C    - FONCTION REALISEE:  CALCUL DU VECTEUR LAGRANGE INITIAL LANDA0
C
C   IN   NBI   : IN  : TAILLE DU VECTEUR
C   IN/OUT VLAGI : VR8: VECTEUR LAGRANGE INITIAL
C   IN    GI : MATR8 : MATRICE GI
C   IN   JGITGI: IN : ADRESSES OBJETS JEVEUX (GI)T*GI
C   IN   MATAS: K19  : NOM DE LA MATRICE DE RIGIDITE GLOBALE
C   IN  CHSECM: K19  : CHAM_NO SECOND MEMBRE GLOBAL
C   IN  LRIGID: LO   : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
C         SOUS-DOMAINES FLOTTANT
C   IN  DIMGI:  IN   : TAILLE DE GIT*GI
C   IN   NBSD:  IN   : NOMBRE DE SOUS-DOMAINES
C   IN   VSDF: VIN  : VECTEUR MATR_ASSE.FETF INDIQUANT SI SD FLOTTANT
C   IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       04/12/03 (OB): CREATION.
C       03/06/04 (OB): MODIFICATION POUR MODES DE CORPS RIGIDES.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBI,JGITGI,DIMGI,NBSD,VSDF(NBSD),VDDL(NBSD)
      REAL*8       VLAGI(NBI),GI(NBI,DIMGI)
      CHARACTER*19 MATAS,CHSECM
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
      INTEGER      I,IFM,NIV,JVE,IDD,NBSDF,IFETR,NBMC,IMC,NBMC1,IDECAI,
     &             IVALE,IDECAO,NBDDL,IFETC,NGITGI,JA,NGITG2,NB,J,JVE1,K
      REAL*8       R8DOT,DET,RAUX
      CHARACTER*19 CHSMDD
      CHARACTER*24 NOMSDR      
      CHARACTER*32 JEXNUM
      LOGICAL      IRET
      
C CORPS DU PROGRAMME
      CALL JEMARQ()

C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)

C INITS.
      NOMSDR=MATAS//'.FETR'
      IRET=.TRUE.
      DET=-999.D0
      DO 10 I=1,NBI
        VLAGI(I)=0.D0
   10 CONTINUE

C SI MODES DE CORPS RIGIDES CALCUL DE LANDA0=GI*(GITGI)-1*E   
      IF (LRIGID) THEN

C OBJET JEVEUX POINTANT SUR LA LISTE DES CHAM_NO SECOND MEMBRE
        CALL JEVEUO(CHSECM//'.FETC','L',IFETC)
      
C VECTEUR AUXILLIAIRE CONTENANT VECTEUR E=[F1T*B1...FQT*BQ]T
        CALL WKVECT('&&FETINL.E.R','V V R',DIMGI,JVE)

C -------------------------------------------------------------------- 
C CONSTITUTION DE E STOCKE DANS '&&FETINL.E.R'
C --------------------------------------------------------------------
C ----  BOUCLE SUR LES SOUS-DOMAINES

C DECALAGE DU VECTEUR OUTPUT DE FETREX (E)
        IDECAO=JVE
C NOMBRE DE SOUS-DOMAINES FLOTTANTS      
        NBSDF=0
C DECALAGE STOCKAGE E   
        IDECAO=JVE        
        DO 100 IDD=1,NBSD
                  
C NOMBRES DE MODES DE CORPS RIGIDES DU SOUS-DOMAINE IDD
          NBMC=VSDF(IDD)
                        
          IF (NBMC.NE.-1) THEN
C SOUS-DOMAINE FLOTTANT
            NBMC1=NBMC-1
            NBSDF=NBSDF+1

C NBRE DE DDL DU SOUS-DOMAINE IDD       
            NBDDL=VDDL(IDD)
                    
C COMPOSANTES DES MODES DE CORPS RIGIDES
            CALL JEVEUO(JEXNUM(NOMSDR,NBSDF),'L',IFETR)
C SECOND MEMBRE LOCAL AU SOUS-DOMAINE
            CHSMDD=ZK24(IFETC+IDD-1)(1:19)
            CALL JEVEUO(CHSMDD//'.VALE','L',IVALE)
                  
C ----  BOUCLE SUR LES MODES DE CORPS RIGIDES
C DECALAGE DE .FETR
            IDECAI=IFETR
            DO 90 IMC=0,NBMC1       
              ZR(IDECAO)=R8DOT(NBDDL,ZR(IVALE),1,ZR(IDECAI),1)
              IDECAI=IDECAI+NBDDL
              IDECAO=IDECAO+1         
   90       CONTINUE
          ENDIF
  100   CONTINUE

C MONITORING
        IF (NIV.GE.3) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          WRITE(IFM,*)'<FETI/FETINL> CONSTRUCTION DE E'
          IF (NIV.GE.5) THEN
            DO 105 I=1,DIMGI
              WRITE(IFM,*)'E(I)',I,ZR(JVE+I-1)
  105       CONTINUE      
          ENDIF
        ENDIF

C -------------------------------------------------------------------- 
C CONSTITUTION DE ((GI)T*GI)-1*E STOCKE DANS '&&FETINL.E.R'
C --------------------------------------------------------------------
C VECTEUR AUXILIAIRE CONTENANT A=(GI)T*GI DESYMETRISEE
        NGITGI=(DIMGI*(DIMGI+1)/2)-1
        NGITG2=DIMGI*DIMGI      
        CALL WKVECT('&&FETINL.GITGI.R','V V R',NGITG2,JA)
C DESYMETRISE (GI)T*GI DANS A CAR MTGAUSS ECRASE LA MATRICE A INVERSER
        I=1
        J=1
        DO 107 K=0,NGITGI
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
  107   CONTINUE
  
        IF (NIV.GE.5) THEN
          IDECAO=JA
          DO 114 J=1,DIMGI
            DO 113 I=1,DIMGI
              WRITE(IFM,*)'GTG(I,J)',I,J,ZR(IDECAO)
              IDECAO=IDECAO+1
  113       CONTINUE
  114     CONTINUE
        ENDIF
                
        NB=1
        CALL MGAUSS(ZR(JA),ZR(JVE),DIMGI,DIMGI,NB,DET,IRET)
C MONITORING
        IF (NIV.GE.3) THEN
          WRITE(IFM,*)'<FETI/FETINL> INVERSION (GITGI)-1*E'
          WRITE(IFM,*)'<FETI/FETINL> DET/IRET ',DET,' ',IRET
          IF (NIV.GE.5) THEN
            DO 115 I=1,DIMGI
              WRITE(IFM,*)'(GIT*GI)-1*E(I)',I,ZR(JVE+I-1)
  115       CONTINUE
          ENDIF   
        ENDIF   

        IF (.NOT.IRET) THEN
          CALL UTDEBM('F','FETINL','SYSTEME (GI)T*GI PROBABLEMENT')
          CALL UTIMPI('S','  NON INVERSIBLE: ',0,I)
          CALL UTIMPR('L','DETERMINANT APPROCHE: ',1,DET)
          CALL UTFINM()
        ENDIF
C -------------------------------------------------------------------- 
C CONSTITUTION DE LANDA0=GI*(((GI)T*GI)-1*E) STOCKE DANS VLAGI
C --------------------------------------------------------------------
        JVE1=JVE-1
        DO 201 J=1,DIMGI
          RAUX=ZR(JVE1+J)
          DO 200 I=1,NBI
            VLAGI(I)=VLAGI(I)+GI(I,J)*RAUX
  200     CONTINUE
  201   CONTINUE

C MONITORING
        IF (NIV.GE.3) THEN
          WRITE(IFM,*)'<FETI/FETINL> CONSTRUCTION DE LANDA0'
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          WRITE(IFM,*)    
          IF (NIV.GE.5) THEN
            DO 205 I=1,NBI
              WRITE(IFM,*)'LANDA0(I)',I,VLAGI(I)
  205       CONTINUE      
          ENDIF
        ENDIF
                
C DESTRUCTION OBJETS TEMPORAIRES        
        CALL JEDETR('&&FETINL.E.R')
        CALL JEDETR('&&FETINL.GITGI.R')                 
      ENDIF
                
      CALL JEDEMA()
      END
