      SUBROUTINE MATRTH(NNO,YOUNG,NU,ALPHA,INDITH)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI,ITAB(8),IRET
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80 (1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      REAL*8        VALRES(26),VALPAR,VALPU(2)
      CHARACTER*2   BL2,CODRET(26)
      CHARACTER*8   NOMRES(26), NOMPAR,NOMPU(2)
      CHARACTER*10  PHENOM
      REAL*8 YOUNG, NU, ALPHA
C
      INDITH=0
      BL2='  '
C
      CALL JEVECH ('PMATERC' , 'L' , JMATE)
C
      CALL RCCOMA(ZI(JMATE),'ELAS',PHENOM,CODRET)
C
      IF ( PHENOM .EQ. 'ELAS' )  THEN
         NOMRES(1)='E'
         NOMRES(2)='NU'
         NOMRES(3)='ALPHA'
      ELSEIF ( PHENOM .EQ. 'ELAS_ORTH' )  THEN
         NOMRES(1)='ALPHA_L'
         NOMRES(2)='ALPHA_T'
         CALL RCVALA(ZI(JMATE),' ',PHENOM,0,NOMPAR,VALPAR,2 ,NOMRES,
     &                                            VALRES,CODRET, 'FM')
         IF (CODRET(1).NE.'OK') THEN
            INDITH = -1
            GOTO 9999
         ELSE
            IF ((VALRES(1).EQ.0.D0).AND.(VALRES(2).EQ.0.D0)) THEN
               INDITH = -1
               GOTO 9999
            ELSE
               CALL UTMESS('F','MATRTH','PAS DE DILATATION '//
     &                      'THERMIQUE ORTHOTROPE POUR COQUE_3D')
            ENDIF
         ENDIF
      ELSE
         CALL UTMESS('F','MATRTH','COMPORTEMENT MATERIAU NON ADMIS')
      ENDIF
C
C===============================================================
C     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:

C     -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
      CALL TECACH ('ONN','PTEMPER',8,ITAB,IRET)
      ITEMP=ITAB(1)
      IF ( ITEMP .NE. 0 ) THEN
         NBPAR  = 1
         NOMPAR = 'TEMP'
         TPG1 = 0.D0
         DO 10 I = 1,NNO
            CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
            T    = ZR(ITEMP  +3*(I-1))
            TINF = ZR(ITEMP+1+3*(I-1))
            TSUP = ZR(ITEMP+2+3*(I-1))
            TPG1 = TPG1 + T + ( TSUP + TINF - 2*T ) / 6.D0
 10      CONTINUE
         VALPAR = TPG1 / NNO
      ELSE

C     -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS':
      CALL TECACH('ONN','PTEMPEF',1,ITEMP,IRET)
         IF (ITEMP.GT.0) THEN
            NBPAR  = 1
            NOMPAR = 'TEMP'
            NOMPU(1)='INST'
            NOMPU(2)='EPAIS'
            CALL JEVECH ( 'PTEMPSR' ,'L', IBID )
            VALPU(1)= ZR(IBID)
            VALPU(2)= 0.D0
            CALL  FOINTE ('FM', ZK8(ITEMP), 2, NOMPU, VALPU, VALPAR,IER)

C     -- SI LA TEMPERATURE N'EST PAS DONNEE:
         ELSE
            NBPAR  = 0
            NOMPAR = ' '
            VALPAR = 0.D0
         ENDIF
      ENDIF
C===============================================================
C
      IF ( PHENOM .EQ. 'ELAS' )  THEN
C
      CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,2  ,NOMRES,
     &                                            VALRES,CODRET, 'FM')
C
      CALL RCVALA(ZI(JMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,1  ,
     &                          NOMRES(3), VALRES(3),CODRET(3), BL2 )
C
      IF (CODRET(3) .NE. 'OK') THEN
         INDITH = -1
         GOTO 9999
      ENDIF
C
C     MATERIAU ISOTROPE
C
         YOUNG = VALRES(1)
         NU    = VALRES(2)
         ALPHA = VALRES(3)
C
C     CONSTRUCTION DU COEFFICIENT EALPNU
C
C        ELAPNU=YOUNG*ALPHA/(1.D0-NU)
C
      ENDIF
C
 9999 CONTINUE
      END
