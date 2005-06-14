      SUBROUTINE RCEXIS ( JMAT, PHENOM, NOMRES, CODRET )
      IMPLICIT NONE
      INTEGER            IMAT,JMAT
      CHARACTER*2        CODRET
      CHARACTER*(*)      PHENOM, NOMRES
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C
C     OBTENTION DE LA VALEUR VALRES D'UN "ELEMENT" D'UNE RELATION DE
C     COMPORTEMENT D'UN MATERIAU DONNE
C
C     ARGUMENTS D'ENTREE:
C        IMAT   : ADRESSE DU MATERIAU CODE
C        PHENOM : NOM DU PHENOMENE
C        NBPAR  : NOMBRE DE PARAMETRES DANS NOMPAR ET VALPAR
C        NOMPAR : NOMS DES PARAMETRES(EX: TEMPERATURE )
C        VALPAR : VALEURS DES PARAMETRES
C        NBRES  : NOMBRE DE RESULTATS
C        NOMRES : NOM DES RESULTATS (EX: E,NU,... )
C                 TELS QU'IL FIGURENT DANS LA COMMANDE MATERIAU
C        STOP = '  '  : ON REMPLIT CODRET ET ON SORT SANS MESSAGE.
C             = 'FM'  : SI UN DES PARAMETRES N'EST PAS TROUVE, ON ARRETE
C                       EN FATAL EN INDIQUANT LE NOM DE LA MAILLE.
C             = 'F '  : IDEM QUE 'FM' MAIS ON N'INDIQUE PAS LA MAILLE.
C
C     ARGUMENTS DE SORTIE:
C     VALRES : VALEURS DES RESULTATS APRES RECUPERATION ET INTERPOLATION
C     CODRET : POUR CHAQUE RESULTAT, 'OK' SI ON A TROUVE, 'NO' SINON
C
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
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
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
C     ------------------------------------------------------------------
C --- PARAMETER ASSOCIE AU MATERIAU CODE
C
      INTEGER       ICOMP, IPI, NBR, NBC, NBF, IVALK,IR, IK,NBMAT
      CHARACTER*10  NOMPHE
C DEB ------------------------------------------------------------------
C
C
      NOMPHE = PHENOM
      CODRET = 'NO'


      NBMAT=ZI(JMAT)
      CALL ASSERT(NBMAT.EQ.1)
      IMAT = JMAT+ZI(JMAT+NBMAT+1)
      
      DO 20 ICOMP = 1 , ZI(IMAT+1)
        IF ( NOMPHE .EQ. ZK16(ZI(IMAT)+ICOMP-1)(1:10) ) THEN
          IPI = ZI(IMAT+2+ICOMP-1)
          GOTO 22
        ENDIF
 20   CONTINUE
C
      GOTO 999
C
 22   CONTINUE
C
      NBR   = ZI(IPI)
      NBC   = ZI(IPI+1)
      NBF   = ZI(IPI+2)
      IVALK = ZI(IPI+3)
      DO 30 IR = 1, NBR
            IF ( NOMRES .EQ. ZK8(IVALK+IR-1) ) THEN
               CODRET = 'OK'
            ENDIF
 30   CONTINUE
C
      DO 42 IK = 1,NBF
         IF ( NOMRES .EQ. ZK8(IVALK+NBR+NBC+IK-1) ) THEN
            CODRET = 'OK'
         ENDIF
 42   CONTINUE
C
 999  CONTINUE
C
      END
