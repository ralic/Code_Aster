      SUBROUTINE RCVALA( IMAT, PHENOM, NBPAR, NOMPAR, VALPAR,
     &                   NBRES, NOMRES, VALRES, CODRET, STOP  )
      IMPLICIT NONE
      INTEGER            IMAT, NBPAR, NBRES
      REAL*8             VALPAR(NBPAR), VALRES(NBRES)
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*(*)      PHENOM, STOP, NOMPAR(NBPAR), NOMRES(NBRES)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
      INTEGER      LMAT, LFCT, LSUP
      PARAMETER  ( LMAT = 7 , LFCT = 9 , LSUP = 2 )
C
      INTEGER       IRES, ICOMP, IPI, IADZI, IAZK24, NBOBJ, NBR, NBC,
     +              NBF, IVALK, IVALR, IR, IPIF, IK
      CHARACTER*2   STOP2
      CHARACTER*8   NOMAIL
      CHARACTER*10  NOMPHE
C DEB ------------------------------------------------------------------
C
C
      STOP2 = STOP
      NOMPHE = PHENOM
C
      DO 10 IRES = 1 , NBRES
        CODRET(IRES) = 'NO'
 10   CONTINUE
      DO 20 ICOMP = 1 , ZI(IMAT+1)
        IF ( NOMPHE .EQ. ZK16(ZI(IMAT)+ICOMP-1)(1:10) ) THEN
          IPI = ZI(IMAT+2+ICOMP-1)
          GOTO 22
        ENDIF
 20   CONTINUE
C
C --- SELON LA VALEUR DE STOP2 ON ARRETE OU NON :
      IF (STOP2(1:1).EQ.'F') THEN
         CALL UTDEBM ('F','RCVALA_01',
     &                'COMPORTEMENT :'//NOMPHE//' NON TROUVE')
         IF ( STOP2(2:2) .EQ. 'M' ) THEN
            CALL TECAEL ( IADZI, IAZK24 )
            NOMAIL = ZK24(IAZK24-1+3)(1:8)
            CALL UTIMPK ( 'S', 'POUR LA MAILLE ', 1, NOMAIL )
         ENDIF
         CALL UTFINM ()
      END IF
      GOTO 999
C
 22   CONTINUE
C
      NBOBJ = 0
      NBR   = ZI(IPI)
      NBC   = ZI(IPI+1)
      NBF   = ZI(IPI+2)
      IVALK = ZI(IPI+3)
      IVALR = ZI(IPI+4)
      DO 30 IR = 1, NBR
         DO 32 IRES = 1, NBRES
            IF ( NOMRES(IRES) .EQ. ZK8(IVALK+IR-1) ) THEN
               VALRES(IRES) = ZR(IVALR-1+IR)
               CODRET(IRES) = 'OK'
               NBOBJ = NBOBJ + 1
            ENDIF
 32      CONTINUE
 30   CONTINUE
C
      IF (NBOBJ .NE. NBRES) THEN
         DO 40 IRES = 1,NBRES
            IPIF = IPI+LMAT-1
            DO 42 IK = 1,NBF
               IF ( NOMRES(IRES) .EQ. ZK8(IVALK+NBR+NBC+IK-1) ) THEN
                  CALL FOINTA (IPIF,NBPAR,NOMPAR,VALPAR,VALRES(IRES))
                  CODRET(IRES) = 'OK'
               ENDIF
               IPIF = IPIF + LFCT
               IF ( NOMPHE .EQ. 'DIS_CONTAC' ) THEN
                  IF ( ZK8(IVALK+NBR+NBC+IK-1) .EQ. 'RELA_MZ ' ) THEN
                     IPIF = IPIF + LSUP
                  ENDIF
               ELSEIF (  NOMPHE(1:8) .EQ. 'TRACTION' ) THEN
                  IPIF = IPIF + LSUP
               ELSEIF ( NOMPHE .EQ. 'META_TRACT' ) THEN
                  IPIF = IPIF + LSUP
               ENDIF
 42         CONTINUE
 40      CONTINUE
      ENDIF
C
 999  CONTINUE
C
      CALL RCVALS( STOP, CODRET, NBRES, NOMRES )
C
      END
