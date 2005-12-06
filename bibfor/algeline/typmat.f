      FUNCTION TYPMAT(NBMAT,TLIMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 25/03/2004   AUTEUR OUGLOVA A.OUGLOVA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
      CHARACTER*(*) TLIMAT(*)
      INTEGER NBMAT
      CHARACTER*1 TYPMAT
C-----------------------------------------------------------------------
C
C- BUT : CETTE FONCTION RETOURNE LE TYPE SYMETRIQUE : 'S'
C                                     OU PLEINE     : 'N'
C        DE LA MATRICE GLOBALE RESULTANTE DE L'ASSEMBLAGE
C        DES MATR_ELEM TLIMAT
C
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C IN  I  NBMAT  : NOMBRE DE MATR_ELEM DE LA LISTE TLIMAT
C IN  K* TLIMAT : LISTE DES MATR_ELEM
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*1 BAS2
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8,NOMACR,EXIELE
      CHARACTER*14 NUM2
      CHARACTER*16 ZK16,OPTIO
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*7 SYM
      CHARACTER*8 MATEL
      INTEGER I
      INTEGER IBID, IERD
      INTEGER IRET     
C----------------------------------------------------------------------
C
C- PAR DEFAUT LE TYPE DE MATRICE EST SYMETRIQUE
C
      TYPMAT = 'S'
C
C- BOUCLE SUR LES MATR_ELEM
C
      DO 10 I =1, NBMAT
        MATEL = TLIMAT(I)
C   TEST NECESSAIRE A LA DETECTION DE MACRO-ELEMENT DANS LE MODELE
        CALL JEEXIN(MATEL//'.LISTE_RESU',IRET)
        IF (IRET.EQ.0) GOTO 10
C   FIN DU TEST 
        CALL DISMOI('F','TYPE_MATRICE',MATEL,'MATR_ELEM',IBID,
     &              SYM,IERD)
        IF (SYM.EQ.'NON_SYM') TYPMAT = 'N'
   10 CONTINUE
      END
