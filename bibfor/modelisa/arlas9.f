      SUBROUTINE ARLAS9(NDIM,NN1,NN2,IJ,IDEB,IMATUU,C)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT      NONE
      INTEGER       NDIM,NN1,NN2,IDEB,IMATUU
      INTEGER       IJ(NN2,NN1)  
      REAL*8        C(*)                 
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C GESTION DES RELATIONS LINEAIRES
C ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN 
C   MAILLE SOLIDE / MAILLE SOLIDE
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  NN1   : NOMBRE DE NOEUDS MAILLE 1
C IN  NN2   : NOMBRE DE NOEUDS MAILLE 2
C IN  IJ     : POINTEURS DANS C (CF ARLAS0)
C I/O C      : MATRICE MORSE (CF ARLFAC)
C
C MATRICE PONCTUELLE DANS C : (X1.X2, X1.Y2, [X1.Z2], Y1.X2, ...) 
C 
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C

      INTEGER  IDIM1,IDIM2,INO1,INO2,I
      REAL*8   MCPLCZ(NDIM,NDIM,NN1,NN2)
C
C ----------------------------------------------------------------------
C

       I = IDEB
       DO 50 INO1 = 1,NN1         
         DO 40 IDIM1 = 1,NDIM         
           DO 30 INO2 = 1,NN2          
             DO 20 IDIM2 = 1,NDIM          
               I = I+1
               MCPLCZ(IDIM1,IDIM2,INO1,INO2) = ZR(IMATUU-1+I)
 20          CONTINUE
 30        CONTINUE
 40      CONTINUE
 50    CONTINUE


C --- ASSEMBLAGE DE LA MATRICE ELEMENTAIRE CK

      DO 90 INO1 = 1,NN1
        DO 80 INO2 = 1,NN2
          I = NDIM*NDIM*(IJ(INO2,INO1)-1)
          DO 70 IDIM1 = 1,NDIM         
            DO 60 IDIM2 = 1,NDIM
              I = I+1           
              C(I) = C(I) + MCPLCZ(IDIM1,IDIM2,INO1,INO2)
 60         CONTINUE
 70       CONTINUE
 80     CONTINUE
 90   CONTINUE

      END
