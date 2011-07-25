      SUBROUTINE NMDECP(SDDISC,ITERAT,IEVDAC,NBRPAS,RATIO ,
     &                  LDECO )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/07/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*19 SDDISC
      INTEGER      IEVDAC,ITERAT
      INTEGER      NBRPAS
      LOGICAL      LDECO
      REAL*8       RATIO
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
C
C CAS AUTOMATIQUE - PARAMETRES DE DECOUPE
C      
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  ITERAT : NUMERO D'ITERATION DE NEWTON
C IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
C OUT RATIO  : RATIO DU PREMIER PAS DE TEMPS
C OUT NBRPAS : NOMBRE DE PAS DE TEMPS
C OUT LDECO  : .FALSE. SI LA DECOUPE A ECHOUE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C 
      INTEGER      IBID
      REAL*8       UN
      LOGICAL      LEXTRA
      REAL*8       VALEXT(4)
      REAL*8       XXBB,XA0,XA1,XDET,CRESI,CIBLEN
      REAL*8       R8BID,R8PREM
      CHARACTER*16 SUBAUT
      INTEGER      MNITER,MXITER
C 
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      UN     = 1.D0
      LDECO  = .FALSE.
      RATIO  = 0.D0
      NBRPAS = 0
C
C --- LECTURE DES INFOS SUR LES CONVERGENCES
C
      CALL NMLERR(SDDISC,'L','MNITER',R8BID ,MNITER)
      CALL NMLERR(SDDISC,'L','MXITER',R8BID ,MXITER)
C
C --- TYPE DE DECOUPAGE AUTO
C   
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IEVDAC,'SUBD_METHODE_AUTO',
     &            R8BID ,IBID  ,SUBAUT)     
C
C --- CALCUL DU NOMBRE DE PAS SELON EVENEMENT
C
      IF (SUBAUT.EQ.'COLLISION') THEN
      
      
      ELSEIF (SUBAUT.EQ.'EXTRAPOLE') THEN
        NBRPAS = 4
      ELSE
        CALL ASSERT(.FALSE.) 
      ENDIF
C
C --- CALCUL DU RATIO SELON EVENEMENT
C

      IF (SUBAUT.EQ.'COLLISION') THEN
      
      
      ELSEIF (SUBAUT.EQ.'EXTRAPOLE') THEN
C      
C ----- EXTRAPOLATION LINEAIRE DES RESIDUS
C
        CALL NMACEX(SDDISC,ITERAT,LEXTRA,VALEXT)
        IF (.NOT.LEXTRA) THEN
          CALL U2MESS('I','EXTRAPOLATION_10')
          LDECO = .FALSE.
        ELSE
          CALL U2MESS('I','EXTRAPOLATION_11')
          XA0    = VALEXT(1)
          XA1    = VALEXT(2)
          XDET   = VALEXT(3)
          CRESI  = VALEXT(4)      
          CIBLEN = (XA0 + XA1*LOG(CRESI) )/XDET
          IF (XDET.LE.R8PREM()) THEN
            RATIO    = 24.0D0/((3.0D0*NBRPAS+UN)**2 - UN)
          ELSE
            IF ( (CIBLEN*1.20D0) .LT. MNITER ) THEN
              RATIO    = 24.0D0/((3.0D0*NBRPAS+UN)**2 - UN)
            ELSE
              IF (XA1.LE.R8PREM()) THEN
                RATIO    = 24.0D0/((3.0D0*NBRPAS+UN)**2 - UN)
              ELSE
                IF ( (CIBLEN-MXITER) .LE. (-10.0D0*XA1/XDET) ) THEN
                  RATIO = EXP( (CIBLEN-MXITER)*XDET/XA1 )
                ELSE
                  RATIO = EXP( -10.0D0 )
                ENDIF
                RATIO  = 0.48485D0*RATIO
                XXBB   = ( -UN + (UN+24.0D0/RATIO)**0.5D0 )/3.0D0
                IF ( XXBB .LT. 2.0D0 ) THEN
                  NBRPAS = 2
                  RATIO  = 0.50D0
                ELSE
                  NBRPAS = NINT( XXBB )
                ENDIF          
              ENDIF
            ENDIF
          ENDIF
          LDECO = .TRUE.
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.) 
      ENDIF 
C
      CALL JEDEMA()
      END
