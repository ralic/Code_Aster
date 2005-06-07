      SUBROUTINE TE0549(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/02/2005   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
C    - FONCTION REALISEE:  EXTRACTION DES VARIABLES INTERNES EN THM
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C =====================================================================
      INTEGER IL,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      INTEGER ICHG,ICOMPO,ICHGS,NUME,IBID,I,NCMP,INOVA
      CHARACTER*16 VARI
      LOGICAL LUMPED
      REAL*8 R8VIDE
C =====================================================================
      CALL LUMTHM(NOMTE,LUMPED)
      IF ( LUMPED ) THEN
         CALL ELREF4(' ','NOEU_S',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &               JGANO)
      ELSE
         CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &               JGANO)
      ENDIF
     
      CALL JEVECH('PNOVARI','L',INOVA)
      CALL JEVECH('PCOMPOR','L',ICOMPO)

      IF (OPTION.EQ.'EXTR_ELGA_VARI') THEN

         CALL JEVECH('PVARIGR','L',ICHG)
         CALL JEVECH('PVARIGS','E',ICHGS)

         CALL POSVAR(ZK16(ICOMPO),NDIM,ZK24(INOVA),NUME)

         READ (ZK16(ICOMPO+1),'(I16)') NCMP
         
         IF (NUME.GT.0) THEN 
            DO 30 I=1,NPG
               ZR(ICHGS-1+I)=ZR(ICHG-1+(I-1)*NCMP+NUME)
  30        CONTINUE
         ELSE
            DO 40 I=1,NPG
               ZR(ICHGS-1+I)=R8VIDE()
  40        CONTINUE
         ENDIF

      ELSE IF (OPTION.EQ.'EXTR_ELNO_VARI') THEN
      
         CALL JEVECH('PVARINR','L',ICHG)
         CALL JEVECH('PVARINS','E',ICHGS)
         
         CALL POSVAR(ZK16(ICOMPO),NDIM,ZK24(INOVA),NUME)

         READ (ZK16(ICOMPO+1),'(I16)') NCMP

         IF (NUME.GT.0) THEN 
            DO 50 I=1,NNO
               ZR(ICHGS-1+I)=ZR(ICHG-1+(I-1)*NCMP+NUME)
  50        CONTINUE
         ELSE
            DO 60 I=1,NNO
               ZR(ICHGS-1+I)=R8VIDE()
  60        CONTINUE
         ENDIF
      
      ENDIF
      
      END
