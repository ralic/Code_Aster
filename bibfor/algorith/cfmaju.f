      SUBROUTINE CFMAJU(RESOCO, NEQ, NDIM, NBLIAI, NBLIAC, LLF, LLF1, 
     +                                                             LLF2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/05/2003   AUTEUR PABHHHH N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
       IMPLICIT      NONE 
       INTEGER       NEQ, NDIM, NBLIAI, NBLIAC, LLF, LLF1, LLF2 
       CHARACTER*24  RESOCO 
C ======================================================================
C ----------------------------------------------------------------------
C --- BUT : MISE A JOUR DU VECTEUR DU ----------------------------------
C ----------------------------------------------------------------------
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
C ======================================================================
      CHARACTER*32       JEXNUM , JEXNOM 
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
C ======================================================================
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      INTEGER       JDELTA, JCM1A, JMU, ILIAC, JLIAC, LLIAC, POSIT, KK 
      INTEGER       POSNBL, POSLF0, POSLF1, POSLF2
      CHARACTER*19  DELTA, MU, CM1A, LIAC 
C ======================================================================
      CALL JEMARQ () 
C ======================================================================
      MU       = RESOCO(1:14)//'.MU' 
      DELTA    = RESOCO(1:14)//'.DELT' 
      CM1A     = RESOCO(1:14)//'.CM1A' 
      LIAC     = RESOCO(1:14)//'.LIAC' 
C ======================================================================
      CALL JEVEUO (DELTA, 'E', JDELTA) 
      CALL JEVEUO (MU,    'L', JMU   ) 
      CALL JEVEUO (LIAC,  'L', JLIAC ) 
C ======================================================================
      POSNBL = 0
      POSLF0 = NBLIAC
      POSLF1 = NBLIAC + (NDIM-1)*LLF
      POSLF2 = NBLIAC + (NDIM-1)*LLF + LLF1
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU ------------------------------
C ======================================================================
      DO 10 ILIAC = 1, NBLIAC + LLF + LLF1 + LLF2
         LLIAC = ZI(JLIAC-1+ILIAC) 
         CALL CFTYLI(RESOCO, ILIAC, POSIT) 
         GOTO (1000, 2000, 3000, 4000) POSIT 
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU - LIAISON DE CONTACT ---------
C ======================================================================
 1000    CONTINUE
         POSNBL = POSNBL + 1
         CALL JEVEUO (JEXNUM(CM1A,LLIAC),'L',JCM1A)
         CALL R8AXPY(NEQ,-ZR(JMU-1+POSNBL),ZR(JCM1A),1, ZR(JDELTA),1) 
         CALL JELIBE (JEXNUM(CM1A,LLIAC)) 
         GOTO 10 
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU - LIAISON DE FROTTEMENT ------
C ======================================================================
 2000    CONTINUE
         POSLF0 = POSLF0 + 1
         CALL JEVEUO (JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A) 
         CALL R8AXPY(NEQ,-ZR(JMU-1+POSLF0),ZR(JCM1A),1, ZR(JDELTA),1) 
         CALL JELIBE (JEXNUM(CM1A,LLIAC+NBLIAI)) 
         IF (NDIM.EQ.3) THEN
            CALL JEVEUO (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'L',JCM1A) 
            CALL R8AXPY(NEQ,-ZR(JMU-1+POSLF0+LLF),ZR(JCM1A),1, 
     +                                                     ZR(JDELTA),1)
            CALL JELIBE (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI)) 
         ENDIF 
         GOTO 10 
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU - LIAISON DE FROTTEMENT ------
C --- SUIVANT LA PREMIERE DIRECTION ------------------------------------
C ======================================================================
 3000    CONTINUE
         POSLF1 = POSLF1 + 1
         CALL JEVEUO (JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A) 
         CALL R8AXPY(NEQ,-ZR(JMU-1+POSLF1),ZR(JCM1A),1, ZR(JDELTA),1) 
         CALL JELIBE (JEXNUM(CM1A,LLIAC+NBLIAI)) 
         GOTO 10 
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU - LIAISON DE FROTTEMENT ------
C --- SUIVANT LA SECONDE DIRECTION -------------------------------------
C ======================================================================
 4000    CONTINUE
         POSLF2 = POSLF2 + 1
         CALL JEVEUO (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'L',JCM1A) 
         CALL R8AXPY(NEQ,-ZR(JMU-1+POSLF2),ZR(JCM1A),1, ZR(JDELTA),1) 
         CALL JELIBE (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI)) 
 10   CONTINUE 
C ======================================================================
       CALL JEDEMA () 
C ======================================================================
       END 
