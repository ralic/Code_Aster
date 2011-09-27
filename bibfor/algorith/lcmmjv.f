      SUBROUTINE LCMMJV(COMP,NMAT,CPMONO,NBFSYS,IROTA,ITBINT,HSR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----------------------------------------------------------------
C     MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C     ----------------------------------------------------------------
C     IN  COMP   : OBJET COMPOR
C         NMAT   :  DIMENSION  MAXIMUM DE CPMONO
C     OUT CPMONO : NOMS DES LOIS POUR CHAQUE FAMILLE DE SYSTEME
C         NBFSYS : NOMBRE DE FAMILLES DE SYS GLIS
C         IROTA  : 1 POUR ROTATION DE RESEAU
C         ITBINT : 1 SI MATRICE D'INTERACTION DONNEE PAR L'UTILISATEUR
C         HSR    : MATRICE D'INTERACTION POUR L'ECROUISSAGE ISOTROPE
C                  UTILISEE SEULEMENT POUR LE MONOCRISTAL IMPLICITE
C     COMMON
C         TBSYSG : SYSTEMES DE GLISSEMENT DONNES PAR L'UTILISATEUR
C     ----------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C     ----------------------------------------------------------------
      INTEGER NMAT,ICOMPI,IROTA,ITBINT,ICOMPO,NBFSYS,I,J
      INTEGER ICOMPR,NBSYST,NBTBSY,IFA,NBSYSI,IDECAL
      REAL*8 HSR(5,30,30),TBSYSG
      CHARACTER*16 COMP(*),CPMONO(5*NMAT+1),COMPK,COMPI,COMPR
      COMMON/TBSYSG/TBSYSG(182)
C     ----------------------------------------------------------------
C
C -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
C
      CALL JEMARQ()
      
      COMPK=COMP(7)(1:8)//'.CPRK'
      COMPI=COMP(7)(1:8)//'.CPRI'
      COMPR=COMP(7)(1:8)//'.CPRR'
      CALL JEVEUO(COMPK,'L',ICOMPO)
      CALL JEVEUO(COMPI,'L',ICOMPI)
            
      NBFSYS=ZI(ICOMPI-1+5)
      IROTA =ZI(ICOMPI-1+6)
      ITBINT=ZI(ICOMPI-1+4)
      NBSYST=ZI(ICOMPI-1+8)
      
C     5 FAMILLES DE SYSTEMES MAXI      
      DO 1 I=1,5*NBFSYS
         CPMONO(I)=ZK16(ICOMPO-1+I)
 1    CONTINUE

      CPMONO(5*NBFSYS+1)=ZK16(ICOMPO-1+5*NBFSYS+1)
      
      NBTBSY=0
      DO 3 IFA=1,NBFSYS
         NBSYSI=ZI(ICOMPI-1+8+IFA)
         NBTBSY=NBTBSY+NBSYSI
 3    CONTINUE
 
      CALL R8INIR(182, 0.D0, TBSYSG, 1)
      
      IF (NBTBSY.NE.0) THEN
C        1 FAMILLE DE SYSTEMES MAXI ICI 
         CALL ASSERT(NBFSYS.EQ.1)    
         TBSYSG(1)=1.D0
         CALL JEVEUO(COMPR,'L',ICOMPR)
         NBSYSI=ZI(ICOMPI-1+8+1)
         TBSYSG(2)=NBSYSI
         
C        table contenant les systemes
         CALL DCOPY(6*NBSYSI,ZR(ICOMPR),1,TBSYSG(3),1)
      ELSE
         TBSYSG(1)=0.D0
      ENDIF       
      
C     table contenant la matrice d'interaction        
      IF (ITBINT.EQ.1) THEN
         IDECAL=0
         IF (NBTBSY.EQ.0) THEN
            CALL JEVEUO(COMPR,'L',ICOMPR)
         ELSE
            IDECAL=6*NBSYSI
         ENDIF
         DO 2 I=1,NBSYST
         DO 2 J=1,NBSYST
            HSR(1,I,J)=ZR(ICOMPR-1+IDECAL+NBSYST*(I-1)+J)
 2       CONTINUE 
      ENDIF
      CALL JEDEMA()
      END
