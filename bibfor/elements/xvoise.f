      SUBROUTINE XVOISE(NDIM,NNOTOT,NSETOT,NNOP,NNO,JCNSET,JLONCH,
     &                  CNINV,CVOISE)
      IMPLICIT NONE
      INTEGER   NVOIS
      PARAMETER (NVOIS=3)
      INTEGER   NDIM,NNOTOT,NSETOT,NNOP,NNO,JCNSET,JLONCH
      INTEGER   CNINV(NNOTOT,NSETOT+1),CVOISE(NVOIS,NSETOT) 
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2008   AUTEUR DELMAS J.DELMAS 
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
C     
C     BUT:
C         RECHERCHE DES VOISINS DES SOUS ELEMENTS DE 
C         L'ELEMENT XFEM PARENT (EN 2D), PUIS ECRITURE DANS ZI()
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   NDIM   : DIMENSION
C IN   NNOTOT : NOMBRE TOTAL DE NOEUDS (POINTS D'INTERSECTION INCLUS)
C IN   NSETOT : OMBRE TOTAL DE SOUS ELEMENT DE L'ELEMENT PARENT
C IN   NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT PARENT (POINTS) 
C                D'INTERSECTION EXCLUS
C IN   NNO    : NOMBRE DE NOEUDS DU SOUS-ELEMENT DE REFERENCE
C IN   JCNSET : ADRESSE DANS ZI DE LA CONNECTIVITE DES SOUS-ELEMENTS
C IN   JLONCH : ADRESSE DANS ZI DES DONNEES CONCERNANT LE DECOUPAGE
C                EN SOUS ELEMENTS
C IN   CNINV  : TABLEAU DE LA CONNECTIVITE INVERSE
C
C      SORTIE :
C-------------
C OUT  CVOISE : TABLEAU DES VOISINS PAR SOUS-ELEMENT
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C      
      INTEGER NSIMPL,NSE,CPT,NBMAV1,NBMAV2
      INTEGER ISIMPL,ISE,IN,INO,JNO,IMAV1,NUMAV1,INDMA1,J,K
      INTEGER INSUI,INOSUI,JNOSUI,IMAV2,NUMAV2,INDMA2
C
C ----------------------------------------------------------------------
C
C --- INITIALISATION DU TABLEAU DES VOISINS 
C
      DO 100 J=1,NSETOT
        DO 110 K=1,NVOIS
          CVOISE(K,J)=0
 110    CONTINUE
 100  CONTINUE
C      
C --- RECUPERATION DE LA SUBDIVISION DE L'ELEMENT PARENT 
C --- EN NSIMPL SIMPLEXES 
C
      NSIMPL=ZI(JLONCH-1+1)
C      
      CPT=0
C
C ----------------------------------------------------------------------
C -----------------  BOUCLE SUR LES NSIMPL SIMPLEXES  ------------------
C ----------------------------------------------------------------------
C
      DO 200 ISIMPL=1,NSIMPL
C        
C ----- RECUPERATION DU DECOUPAGE EN NSE SOUS-ELEMENTS 
C
        NSE=ZI(JLONCH-1+1+ISIMPL)
C
C ----------------------------------------------------------------------
C -----------------  BOUCLE SUR LES NSE SOUS-ELEMENTS  -----------------
C ----------------------------------------------------------------------
C
        DO 210 ISE=1,NSE
          CPT=CPT+1
C
C ----------------------------------------------------------------------
C --------------  BOUCLE SUR LES SOMMETS DU SOUS-ELEMENTS  -------------
C ----------------------------------------------------------------------
C
          DO 211 IN=1,NNO
C
C --------- RECUPERATION DE LA NUMEROTATION XFEM
C
            INO=ZI(JCNSET-1+(NDIM+1)*(CPT-1)+IN)     
C
C --------- NUMEROTATION PROPRE A LA CONNECTIVITE INVERSE          
C
            IF (INO.LT.1000) THEN
              JNO=INO
            ELSE
              JNO=INO-1000+NNOP              
            END IF
C            
            NBMAV1=CNINV(JNO,1)
C
C ----------------------------------------------------------------------
C ---------  BOUCLE SUR LES VOISINS POTENTIELS CONTENANT "JNO"  --------
C ----------------------------------------------------------------------
C
            DO 212 IMAV1=1,NBMAV1
C
              INDMA1=IMAV1+1
              NUMAV1=CNINV(JNO,INDMA1)
C
C ----------- ON S'ASSURE QUE LE VOISIN POTENTIEL N'EST PAS LE 
C ----------- SOUS-ELEMENT COURANT
C
              IF(NUMAV1.NE.CPT) THEN
C
C
C ------------- RESPECT DE LA NUMEROTATION AU SEIN DU SOUS-ELEMENT 
C
                IF(IN.EQ.NNO) THEN
                  INSUI=1
                ELSE
                  INSUI=IN+1
                END IF
C
C ------------- RECUPERATION DE LA NUMEROTATION XFEM
C
                INOSUI=ZI(JCNSET-1+(NDIM+1)*(CPT-1)+INSUI)    
C
C ------------- NUMEROTATION PROPRE A LA CONNECTIVITE INVERSE
C
                IF (INOSUI.LT.1000) THEN
                  JNOSUI=INOSUI
                ELSE
                  JNOSUI=INOSUI-1000+NNOP              
                END IF
C
                NBMAV2=CNINV(JNOSUI,1)
C
C ----------------------------------------------------------------------
C ------  BOUCLE SUR LES VOISINS POTENTIELS CONTENANT "JNOSUI"  --------
C ----------------------------------------------------------------------
C
                DO 213 IMAV2=1,NBMAV2
C                
                  INDMA2=IMAV2+1
                  NUMAV2=CNINV(JNOSUI,INDMA2)
C
C --------------- ON LOCALISE LE VOISIN SITUE EN VIS-À-VIS DE L'ARRETE
C --------------- [JNO,JNOSUI] (S'IL EXISTE), PUIS ON L'ECRIT DANS ZI()
C
                  IF(NUMAV2.EQ.NUMAV1) THEN
                    CVOISE(IN,CPT)=NUMAV1
                  END IF
C                  
 213            CONTINUE
C
              ENDIF
C
 212        CONTINUE
C
 211      CONTINUE
C
 210    CONTINUE
C
 200  CONTINUE
C      
      END
