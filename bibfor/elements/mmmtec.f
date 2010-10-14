      SUBROUTINE MMMTEC(PHASE ,NDIM  ,NNL   ,NNE   ,NORM  ,
     &                  HPG   ,FFL   ,FFE   ,JACOBI,CWEAR ,
     &                  DISSIP,DLAGRC,DELUSU,TYPBAR,TYPRAC,
     &                  MATREC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*4  PHASE
      INTEGER      NDIM,NNE,NNL,TYPBAR,TYPRAC
      REAL*8       FFE(9),FFL(9)
      REAL*8       HPG,JACOBI
      REAL*8       NORM(3)
      REAL*8       DISSIP,CWEAR,DLAGRC,DELUSU(3)       
      REAL*8       MATREC(27,9)        
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE DEPL_ESCL/LAGR_C 
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'CONT' - CONTACT METHODE LAGRANGIENNE
C              'PCON' - CONTACT METHODE PENALISEE
C              'USUR' - USURE
C              'EXCL' - EXCLUSION D'UN NOEUD
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
C IN  NORM   : NORMALE AU POINT DE CONTACT
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
C IN  FFL    : FONCTIONS DE FORMES LAGR.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  TYPBAR : NUMERO DU NOEUD (MILIEU) EXCLU
C IN  CWEAR  : COEFFICIENT D'USURE (KWEAR/HWEAR)
C IN  DLAGRC : LAGR_C DEPDEL DU POINT DE CONTACT (USURE UNILATERALE)
C IN  DELUSU : SAUT TGT DE L'INCREMENT DE DEPLACEMENT [[DELTA_U]]_TAU
C IN  DISSIP : DISSIPATION USURE
C OUT MATREC : MATRICE ELEMENTAIRE DEPL_E/LAGR_C
C
C ----------------------------------------------------------------------
C
      INTEGER   INOC,INOE,IDIM,JJ,I
      INTEGER   NDEXCL(9),IEXCL
C
C ----------------------------------------------------------------------
C
      DO 10 I = 1,9
        NDEXCL(I) = 0
 10   CONTINUE
C
C --- PARTIE CONTACT METHODE LAGRANGIENNE
C       
      IF (PHASE.EQ.'CONT') THEN    
        DO 200 INOC = 1,NNL
          DO 190 INOE = 1,NNE
            DO 180 IDIM = 1,NDIM
              JJ = NDIM*(INOE-1)+IDIM            
              MATREC(JJ,INOC) = MATREC(JJ,INOC) -
     &                       HPG*FFL(INOC)*FFE(INOE)*JACOBI*NORM(IDIM) 
  180       CONTINUE
  190     CONTINUE
  200   CONTINUE
C
C --- PARTIE CONTACT METHODE PENALISEE
C 
      ELSEIF (PHASE.EQ.'PCON') THEN
C       ON NE FAIT RIEN / LA MATRICE EST NULLE
C
C --- PARTIE USURE
C       
      ELSEIF (PHASE.EQ.'USUR') THEN   
        DO 201 INOC = 1,NNL
          DO 191 INOE = 1,NNE
            DO 181 IDIM = 1,NDIM
              JJ = NDIM*(INOE-1)+IDIM
              MATREC(JJ,INOC) = MATREC(JJ,INOC) +
     &                      (HPG*FFL(INOC)*FFE(INOE)*JACOBI*
     &                       DELUSU(IDIM)*(CWEAR/DISSIP)*DLAGRC) 

      
  181       CONTINUE
  191     CONTINUE
  201   CONTINUE      
      ELSEIF (PHASE.EQ.'EXCL') THEN 
        CALL MMEXN1(TYPBAR,NDEXCL)
        DO 291 INOE = 1,NNE
          DO 281 IDIM = 1,NDIM
            JJ = NDIM*(INOE-1)+IDIM 
            DO 78 IEXCL = 1,9   
              IF (NDEXCL(IEXCL).EQ.1) THEN
                MATREC(JJ,IEXCL) = 0.D0
              ENDIF  
   78       CONTINUE           
  281     CONTINUE
  291   CONTINUE 
        IF (TYPRAC.NE.0) THEN
        CALL MMEXN2(TYPRAC,NDEXCL)
        DO 299 INOE = 1,NNE
          DO 289 IDIM = 1,NDIM
            JJ = NDIM*(INOE-1)+IDIM 
            DO 79 IEXCL = 1,9   
              IF (NDEXCL(IEXCL).EQ.1) THEN
                MATREC(JJ,IEXCL) = 0.D0
              ENDIF  
   79       CONTINUE           
  289     CONTINUE
  299   CONTINUE 
        ENDIF               
      ELSE
        CALL ASSERT(.FALSE.)     
      ENDIF
C
      END
