      SUBROUTINE TGVERI(OPTION,CARCRI,COMPOR,NNO,GEOM,NDIM,NDDL,DEPLP,
     &  SDEPL,VECTU,SVECT,NCONT,CONTP,SCONT,NVARI,VARIP,SVARI,
     &  MATUU,SMATR,MATSYM,EPSILO,VARIA,IRET)     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/01/2007   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C TOLE CRP_21      
      IMPLICIT NONE
      LOGICAL MATSYM
      CHARACTER*16 OPTION,COMPOR(*)
      INTEGER IRET,NNO,NDIM
      REAL*8 CARCRI(*),SDEPL(*),SCONT(*),SVECT(*),SMATR(*),VARIA(*)
      REAL*8 GEOM(*),DEPLP(*),VECTU(*),CONTP(*),MATUU(*)
      REAL*8 VARIP(*),SVARI(*)
      
C ----------------------------------------------------------------------
C VAR OPTION NOM DE L'OPTION DE CALCUL 
C             IN  : CELLE UTILISEE PAR LE TE
C             OUT : 'RAPH_MECA' SI BOUCLE, 'FULL_MECA' SI FIN DE BOUCLE
C IN  CARCRI  : CARCRI(1) = type de matrice tangente
C               0 : ANALYTIQUE, on ne passe pas ici
C               1 : PERTURBATION, on calcule Ktgte (FULL_MECA)
C               2 : VERIFICATION, on calcule Ktgte (FULL_MECA) + Kpertu
C               CARCRI(7) = valeur de la perturbation
C OUT IRET   SI IRET = 0 -> FIN, SINON -> BOUCLE
C ----------------------------------------------------------------------
C
C EXEMPLE D'INSERTION DANS UN TE DE L'OPTION FULL_MECA
C  1000 CONTINUE
C       CALL NMPL3D(OPTION,...)
C       CALL TGVERI(OPTION,....., IRET)
C       IF (IRET.NE.0) GOTO 1000
C       
C ----------------------------------------------------------------------

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C      CHARACTER*24 DDLD,VECTU,MATRA,MATRC,VARIA,CONT,VARI
      CHARACTER*24 MATRA,MATRC
      INTEGER LDDLD,EDDLD,EVECTU,EMATRA,EMATRC,EVARIA,EXI
      INTEGER LVECTU,LMATRI,ICOMPO,TAB(7),RET,NDDL,IGEOM
      INTEGER ICONTP,IVARIP,IVARIX,NVARI,NCONT,ECONTP,EVARIP
      INTEGER I,J,K,INDI,NVAR,DIMGEO,INIT,POS
      REAL*8 V,EPSILO,FP,FM,F0,PERTU,MAXDEP,MAXGEO,R8MIEM
      REAL*8 MATPER(3*27*3*27)
      SAVE INIT,POS
      DATA MATRA  /'PYTHON.TANGENT.MATA'/
      DATA MATRC  /'PYTHON.TANGENT.MATC'/
      DATA INIT,POS /1,0/
C ----------------------------------------------------------------------

C     Calcul de la matrice TGTE par PERTURBATION
     
      IRET=0
      IF (ABS(CARCRI(2)).LT.0.1D0) THEN
         GOTO 9999
      ELSE
C INCOMATIBILITE AVEC LES COMPORTEMENTS QUI UTILISENT PVARIMP      
         IF (COMPOR(5)(1:7).EQ.'DEBORST') THEN
            GOTO 9999
         ENDIF
      ENDIF
      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN     
          GOTO 9999                       
      ENDIF                               
      
C --  INITIALISATION (PREMIER APPEL)   
      
      IF (INIT .EQ. 1) THEN
      
C       PERTURBATION OU VERIFICATION => FULL_MECA   
        IF (OPTION.NE.'FULL_MECA') THEN   
            GOTO 9999                     
        ENDIF                             

C       CALCUL de la valeur de la perturbation
C       Ici on est en mecanique seule, les DDL sont 
C       seulement des deplacements

        MAXDEP=0.D0                                       
        MAXGEO=0.D0                                       
        DO 555 I=1,NDDL                                 
           MAXDEP=MAX(MAXDEP,ABS(DEPLP(I)))          
555     CONTINUE                                          
        DO 556 I=1,NNO*NDIM                                 
           MAXGEO=MAX(MAXGEO,ABS(GEOM(I)))           
556     CONTINUE  
        PERTU=CARCRI(7)                                        
        IF (MAXDEP.GT.PERTU*MAXGEO) THEN                          
           EPSILO=PERTU*MAXDEP                            
        ELSE                                              
           EPSILO=PERTU*MAXGEO                            
        ENDIF                                             
        IF (EPSILO.LT.R8MIEM()) THEN
           CALL U2MESS('F','ALGORITH11_86')
        ENDIF
        
C      ARCHIVAGE DES VALEURS DE REFERENCE

        CALL DCOPY(NDDL,DEPLP ,1,SDEPL ,1)
        CALL DCOPY(NCONT,CONTP ,1,SCONT ,1)
        CALL DCOPY(NDDL,VECTU,1,SVECT,1)
        CALL DCOPY(NVARI,VARIP,1,SVARI ,1)
        
C       ARCHIVAGE DE LA MATRICE TANGENTE COHERENTE
        IF (MATSYM) THEN
          K = 0
          DO 557 I = 1,NDDL
            DO 558 J = 1,I
              V = MATUU(K+1)
              K = K + 1
              SMATR((I-1)*NDDL+J) = V
              SMATR((J-1)*NDDL+I) = V
 558         CONTINUE
 557      CONTINUE
        ELSE
          CALL DCOPY(NDDL*NDDL,MATUU,1,SMATR,1)
        ENDIF
                    
C      PREPARATION DES ITERATIONS

        OPTION = 'RAPH_MECA'
        IRET = 1  
        INIT = 0  
        POS = 0        

      END IF
                          
C -- TRAITEMENT DES VARIATIONS


C    SAUVEGARDE DE LA FORCE INTERIEURE PERTURBEE

      NVAR = INT((POS+1)/2)
      
      IF (NVAR.GT.0) THEN
        CALL DCOPY(NDDL,VECTU,1,VARIA(1+(POS-1)*NDDL),1)
      END IF      

      POS = POS + 1
      NVAR = INT((POS+1)/2)
      INDI = 1-2*MOD(POS,2)

      IF (NVAR.LE.NDDL) THEN
        CALL DCOPY(NDDL,SDEPL,1,DEPLP,1)
        DEPLP(NVAR) = SDEPL(NVAR) + INDI*EPSILO
        
C      INITIALISATION DES CHAMPS 'E'
        CALL R8INIR(NCONT,0.D0,CONTP,1)
        CALL R8INIR(NDDL,0.D0, VECTU,1)
        IRET=1
        GOTO 9999
      END IF    
      
C    CALCUL DE LA MATRICE TANGENTE

      DO 559 I = 1,NDDL
        DO 560 J = 1,NDDL
          FM = VARIA((2*J-2)*NDDL+I)
          FP = VARIA((2*J-1)*NDDL+I)
          V  = (FP-FM)/(2*EPSILO)
          MATPER((I-1)*NDDL+J) = V
 560    CONTINUE
 559  CONTINUE
            
C    MENAGE POUR ARRET DE LA ROUTINE

      IRET = 0
      INIT = 1
      OPTION = 'FULL_MECA'

C    RETABLISSEMENT DE LA SOLUTION        
      CALL DCOPY(NDDL, SDEPL  ,1,DEPLP  ,1)
      CALL DCOPY(NDDL, SVECT  ,1,VECTU ,1)
      CALL DCOPY(NCONT,SCONT  ,1,CONTP ,1)
      CALL DCOPY(NVARI,SVARI  ,1,VARIP ,1)
      
C     PERTURBATION => SAUVEGARDE DE LA MATRICE CALCULEE PAR 
C     DIFFERENCES FINIES COMME MATRICE TANGENTE

      IF (ABS(CARCRI(2)-1.D0).LT.0.1D0) THEN
         IF (MATSYM) THEN
           CALL MAVEC(MATPER,NDDL,MATUU,NDDL*(NDDL+1)/2)
         ELSE                                               
           CALL DCOPY(NDDL*NDDL,MATPER,1,MATUU,1)
         ENDIF 
                                                      
C     VERIFICATION    
     
      ELSEIF (ABS(CARCRI(2)-2.D0).LT.0.1D0) THEN
         IF (MATSYM) THEN
           CALL MAVEC(SMATR,NDDL,MATUU,NDDL*(NDDL+1)/2)
         ELSE                                               
           CALL DCOPY(NDDL*NDDL,SMATR,1,MATUU,1)
         ENDIF                                              

C      CREATION DES OBJETS
C      CE N'EST PAS LA PREMIERE FOIS QU'ON CALCULE LA MATRICE TANGENTE
C      -> ON NE CONSERVE QUE LE DERNIER CALCUL (EN COURS)
        CALL JEEXIN(MATRA,EXI)
        IF (EXI.NE.0) THEN
          CALL JEDETR(MATRA)
          CALL JEDETR(MATRC)
        END IF
         CALL WKVECT(MATRA ,'G V R',NDDL*NDDL,EMATRA)
         CALL WKVECT(MATRC ,'G V R',NDDL*NDDL,EMATRC)
         CALL DCOPY(NDDL*NDDL,SMATR,1,ZR(EMATRA),1)
         CALL DCOPY(NDDL*NDDL,MATPER,1,ZR(EMATRC),1)
         CALL JELIBE(MATRA)
         CALL JELIBE(MATRC)
      ENDIF
      
 9999 CONTINUE
      END      
