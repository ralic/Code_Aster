      SUBROUTINE TE0004 (OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/08/2011   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE DELMAS J.DELMAS
C
C     BUT:
C       !! ROUTINE GENERIQUE !!
C       ASSURER LE PASSAGE ELGA -> ELNO
C
C.......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER  ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NDIM,NNO,NNOS,NPG,NBSP
      INTEGER IPOIDS,IVF,IDFDE,JGANO
      INTEGER IRET,NBCMP,ITAB(7)
      INTEGER IINPG,IOUTNO

      CHARACTER*4 FAMI
C
C ----------------------------------------------------------------------
C
C    -------------------------------------------------------------------
C    -- OPTION "DERA_ELNO"
C    -------------------------------------------------------------------

      IF (OPTION.EQ.'DERA_ELNO') THEN
        FAMI = 'RIGI'
        CALL TECACH('OOO','PDERAPG',7,ITAB,IRET)
        CALL JEVECH ('PDERANO','E',IOUTNO)
      ENDIF
C
C    -------------------------------------------------------------------
C    -- OPTION "ENDO_ELNO"
C    -------------------------------------------------------------------

      IF (OPTION.EQ.'ENDO_ELNO') THEN
        FAMI = 'RIGI'
        CALL TECACH('OOO','PTRIAPG',7,ITAB,IRET)
        CALL JEVECH ('PTRIANO','E',IOUTNO)
      ENDIF
C
C    -------------------------------------------------------------------
C    -- COMMUN A TOUTES LES OPTIONS
C    -------------------------------------------------------------------
C
C --- IL FAUDRAIT PENSER A RECUPERER FAMI DE MANIERE AUTOMATIQUE ET SURE
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      IINPG=ITAB(1)
      NBCMP=ITAB(2)/ITAB(3)
C
      IF (ITAB(3).NE.NPG) CALL ASSERT(.FALSE.)
C
C --- ON INTERDIT LES ELEMENTS A SOUS-POINT POUR LE MOMENT
      IF (ITAB(7).NE.1) CALL ASSERT(.FALSE.)
C
      NBSP=1
C
      CALL PPGAN2(JGANO,NBSP,NBCMP,ZR(IINPG),ZR(IOUTNO))
C
      END
