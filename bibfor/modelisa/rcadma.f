      SUBROUTINE RCADMA ( IMATE, PHENOM, NOMRES, VALRES, CODRET, STOP )
      IMPLICIT   NONE
      CHARACTER*(*)       PHENOM, NOMRES, STOP
      CHARACTER*2         CODRET
      INTEGER             IMATE, VALRES
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/09/98   AUTEUR CIBHHLV L.VIVAN 
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
C
C     OBTENTION DES ADRESSES DES COMPOSANTES D'UN MATERIAU METALLURGIQUE
C               DE L'ADRESSE D'UNE TEXTURE            
C
C     ARGUMENTS D'ENTREE:
C        IMATE  : ADRESSE DU MATERIAU CODE
C        PHENOM : NOM DU PHENOMENE
C        NOMRES : NOM DES RESULTATS (EX: TRC, TEXTURE, ... )
C                 TELS QU'IL FIGURENT DANS LA COMMANDE MATERIAU
C
C     ARGUMENTS DE SORTIE:
C       VALRES  : ADRESSE DU .VALE du LISTR8
C       CODRET  : 'OK' SI ON A TROUVE, 'NO' SINON
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER            LMAT, ICOMP, IPI, IPIF, IPIFC, IADZI, IAZK24,
     +                   NBK, IVALK, IK, NBR, NBC, LFCT
      PARAMETER        ( LMAT = 7 , LFCT = 9)
      CHARACTER*2        STOP2
      CHARACTER*8        NOMAIL
      CHARACTER*10       NOMPHE
C DEB ------------------------------------------------------------------
C
      CODRET = 'NO'
      NOMPHE = PHENOM
      STOP2 = STOP
C
      DO 10 ICOMP = 1,ZI(IMATE+1)
        IF ( NOMPHE .EQ. ZK16(ZI(IMATE)+ICOMP-1)(1:10) ) THEN
          IPI = ZI(IMATE+2+ICOMP-1)
          GOTO 11
        ENDIF
 10   CONTINUE
C
C     -- SELON LA VALEUR DE STOP2 ON ARRETE OU NON :
      IF ( STOP2(1:1) .EQ. 'F' ) THEN
         CALL UTDEBM ('F','RCADMA_01',
     &                'COMPORTEMENT :'//NOMPHE//' NON TROUVE')
         IF ( STOP2(2:2) .EQ. 'M' ) THEN
            CALL TECAEL ( IADZI, IAZK24 )
            NOMAIL = ZK24(IAZK24-1+3)(1:8)
            CALL UTIMPK ( 'S', 'POUR LA MAILLE ', 1, NOMAIL )
         ENDIF
         CALL UTFINM ()
      END IF
      GOTO 9999
C
 11   CONTINUE
C
      NBR   = ZI(IPI)
      NBC   = ZI(IPI+1)
      NBK   = ZI(IPI+2)
      IVALK = ZI(IPI+3)
      DO 150 IK = 1, NBK
         IF ( NOMRES .EQ. ZK8(IVALK+NBR+NBC+IK-1) ) THEN
            CODRET = 'OK'
            IPIF = IPI + LMAT + (IK-1)*LFCT -1
            VALRES = ZI(IPIF  )
            GOTO 9999
         ENDIF
  150 CONTINUE
C
      CALL RCVALS( STOP, CODRET, 1, NOMRES )
C
 9999 CONTINUE
C
      END
