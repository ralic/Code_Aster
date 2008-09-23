      SUBROUTINE CUACA2(DEFICU,RESOCU,NBLIAC,SPLIAI,INDFAC,
     &                  LMAT  ,XJVMAX)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      CHARACTER*24  DEFICU,RESOCU      
      INTEGER       NBLIAC
      INTEGER       SPLIAI
      INTEGER       INDFAC
      INTEGER       LMAT
      REAL*8        XJVMAX
C        
C ----------------------------------------------------------------------
C
C ROUTINE LIAISON_UNILATER (RESOLUTION - A.C-1.AT)
C
C CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES)
C STOCKAGE DE LA MOITIE UNIQUEMENT (PROBLEME SYMETRIQUE)
C
C ----------------------------------------------------------------------
C
C
C IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C I/O INDFAC : INDICE DE DEBUT DE LA FACTORISATION
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C I/O XJVMAX : VALEUR DU PIVOT MAX
C IN  DEFICU : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCU : SD DE TRAITEMENT NUMERIQUE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32       JEXNUM
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER       JDECAL
      INTEGER       NBDDL,JVA,JVALE,JPOI,NEQ
      INTEGER       ILIAC,JJ,LLIAC,LLJAC,JSCIB,II,DERCOL,BLOC
      INTEGER       JSCBL,JOUV,JSCDE,NBBLOC
      REAL*8        VAL
      CHARACTER*19  LIAC,CM1A,MATR,STOC,OUVERT
      INTEGER       JLIAC,JCM1A
      CHARACTER*24  APDDL,APCOEF,POINOE
      INTEGER       JAPDDL,JAPCOE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C ======================================================================
      POINOE = DEFICU(1:16)//'.POINOE' 
      CM1A   = RESOCU(1:14)//'.CM1A'
      APDDL  = RESOCU(1:14)//'.APDDL'
      LIAC   = RESOCU(1:14)//'.LIAC'
      APCOEF = RESOCU(1:14)//'.APCOEF'
      MATR   = RESOCU(1:14)//'.MATR'
      STOC   = RESOCU(1:14)//'.SLCS'
      CALL JEVEUO (APDDL, 'L',JAPDDL)
      CALL JEVEUO (LIAC,  'L',JLIAC )
      CALL JEVEUO (APCOEF,'L',JAPCOE)
      CALL JEVEUO (POINOE,'L',JPOI) 
      CALL JEVEUO (STOC//'.SCIB','L',JSCIB)
      CALL JEVEUO (STOC//'.SCBL','L',JSCBL)
      CALL JEVEUO (STOC//'.SCDE','L',JSCDE)
C 
C --- INITIALISATIONS 
C 
      NEQ    = ZI(LMAT+2)
      NBBLOC = ZI(JSCDE-1+3)
      OUVERT = '&CFACA2.TRAV'
      CALL WKVECT (OUVERT,'V V L',NBBLOC,JOUV)
C 
C --- CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES) 
C --- (STOCKAGE DE LA MOITIE PAR SYMETRIE)
C 
      INDFAC = MIN(INDFAC, SPLIAI+1)

      DO 210 ILIAC = SPLIAI+1, NBLIAC 
         LLIAC  = ZI(JLIAC-1+ILIAC)
        
         CALL JEVEUO(JEXNUM(CM1A,LLIAC),'L',JCM1A)
         II     = ZI(JSCIB-1+ILIAC)
         DERCOL = ZI(JSCBL+II-1)
         BLOC   = DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF (II.GT.1) THEN
               CALL JELIBE(JEXNUM(MATR//'.UALF',(II-1)))
               ZL(JOUV-2+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MATR//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA = JVALE-1 + (ILIAC-1)*(ILIAC)/2-BLOC
         DO 10 JJ = 1, ILIAC
            LLJAC   = ZI(JLIAC-1+JJ)
            JDECAL  = ZI(JPOI+LLJAC-1)
            NBDDL   = ZI(JPOI+LLJAC) - ZI(JPOI+LLJAC-1)
            JVA     = JVA + 1
            ZR(JVA) = 0.0D0
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
 10      CONTINUE
         CALL JELIBE(JEXNUM(CM1A,LLIAC))
 210  CONTINUE
C ======================================================================
      SPLIAI = NBLIAC 
      CALL JEDETR(OUVERT)
      CALL JEDEMA()
C ======================================================================

      END
