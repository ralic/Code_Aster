      SUBROUTINE TE0396 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  OPTION : 'CHAR_MECA_TEMP_R'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      INTEGER ICARAC,NNO,NPG,IFF,IPOIDS,IVF,IDFDE,KP,NE,IMATE
      INTEGER LSECT,LORIEN,JEFINT,KC,ITEMPR,ITEREF,IGEOM,I,K0,IFINT,IC
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(4),ELREFE
      CHARACTER*2        BL2, CODRET(4)
      REAL*8 EN(3,2),ENPRIM(3,2),VALRES(4),GRANC(6),
     &FINT(6,3),TEMPN(3),Y0(3),X00(3,3),X0PG(3),
     &ROT0(3,3),NU,
     &GN(3),GM(3),PN(3),PM(3),
     &QIGK(3),ROTK(3,3),ROTABS(3,3),
     &ALPHA,TREF
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CALL ELREF1(ELREFE)
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG-1
      IDFDE =IVF + NPG*NNO
C
      DO 2 KP=1,NPG
        DO 1 NE=1,NNO
          IVF = IVF + 1
          IDFDE = IDFDE + 1
          EN(NE,KP) = ZR(IVF)
          ENPRIM(NE,KP) = ZR(IDFDE)
1       CONTINUE
2     CONTINUE
C PARAMETRES EN ENTREE
      CALL JEVECH('PMATERC','L',IMATE)
      BL2 = '  '
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'RHO'
      NOMRES(4) = 'ALPHA'
      CALL RCVALA ( ZI(IMATE),'ELAS',0,'  ',R8BID,2,NOMRES,
     +              VALRES, CODRET, 'FM' )
      CALL RCVALA ( ZI(IMATE),'ELAS',0,'  ',R8BID,1,NOMRES(3),
     +              VALRES(3), CODRET(3), BL2 )
      CALL RCVALA ( ZI(IMATE),'ELAS',0,'  ',R8BID,1,NOMRES(4),
     +              VALRES(4), CODRET(4), BL2 )
      IF ( CODRET(4).NE.'OK' ) THEN
         ALPHA   = 0.D0
      ELSE
         ALPHA   = VALRES(4)
      ENDIF
      E     = VALRES(1)
      NU    = VALRES(2)
      G     = E / (2.D0*(1.0D0+NU))
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
C
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
C
C     --- LA SECTION EST SUPPOSEE CONSTANTE ---
C
      A     =  ZR(LSECT+1)
      XIY   =  ZR(LSECT+2)
      XIZ   =  ZR(LSECT+3)
      AY    =  ZR(LSECT+4)
      AZ    =  ZR(LSECT+5)
      XJX   =  ZR(LSECT+8)
      GRANC(1) = E * A
      GRANC(2) = G * A / AY
      GRANC(3) = G * A / AZ
      GRANC(4) = G * XJX
      GRANC(5) = E * XIY
      GRANC(6) = E * XIZ
C
C     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
C
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
      Y0(1) =  ZR(LORIEN    )
      Y0(2) =  ZR(LORIEN + 1)
      Y0(3) =  ZR(LORIEN + 2)
C
C PARAMETRES EN SORTIE
C
        CALL JEVECH('PVECTUR','E',JEFINT)
        DO 14 NE=1,NNO
          DO 13 KC=1,6
            FINT(KC,NE) = 0.D0
13        CONTINUE
14      CONTINUE
C
      CALL JEVECH('PTEMPER','L',ITEMPR)
      CALL JEVECH('PTEREF','L',ITEREF)
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      DO 21 I=1,NNO
        TEMPN(I) = ZR(ITEMPR-1+I)
21    CONTINUE
      TREF = ZR(ITEREF)
C
      K0 = IGEOM - 1
C
      DO 33 NE=1,NNO
        DO 31 KC=1,3
          K0 = K0 + 1
          X00(KC,NE) = ZR(K0)
31      CONTINUE
33    CONTINUE
C
C     MATRICE DE ROTATION ASSOCIE AU VECTEUR ROTATION NUL
      DO 41 I=1,3
         QIGK(I)=0.D0
41    CONTINUE
      CALL MAROTA (QIGK,ROTK)
C
C     BOUCLE SUR LES POINTS DE GAUSS
C
      DO 51 KP=1,NPG
         CALL GDJRG0 (KP,NNO,ENPRIM,X00,Y0,AJACOB,ROT0)
         PJACOB = ZR(IPOIDS-1+KP) * AJACOB
         CALL PROMAT (ROTK,3,3,3,ROT0,3,3,3,ROTABS)
C
         DO  11 IC=1,3
            X0PG(IC) = 0.D0
11       CONTINUE
         UNSURJ = 1.D0 / AJACOB
         DO  53 IC=1,3
         DO  52 NE=1,NNO
          X0PG(IC)=X0PG(IC)+UNSURJ*ENPRIM(NE,KP)*X00(IC,NE)
52       CONTINUE
53       CONTINUE
         TEMPG = 0.D0
         DO  5 NE=1,NNO
           TEMPG = TEMPG + EN(NE,KP)*TEMPN(NE)
    5    CONTINUE
C
         DO 15 I=1,3
            GN(I) = 0.D0
            GM(I) = 0.D0
   15    CONTINUE
C
C        DILATATION THERMIQUE : -E*A*ALPHA*(T-TREF)
C
         GN(1) = GN(1) + GRANC(1)*ALPHA*(TEMPG-TREF)
C
         CALL PROMAT (ROTABS,3,3,3,GN   ,3,3,1,   PN    )
         CALL PROMAT (ROTABS,3,3,3,GM   ,3,3,1,   PM    )
         CALL GDFINT (KP,NNO,AJACOB,PJACOB,EN,ENPRIM,X0PG,PN,PM,FINT)
C
51    CONTINUE
C
C     FIN DE BOUCLE SUR LES POINTS DE GAUSS
C
          IFINT = JEFINT - 1
          DO 72 NE=1,NNO
            DO 71 KC=1,6
              IFINT = IFINT + 1
              ZR(IFINT) = FINT(KC,NE)
71          CONTINUE
72        CONTINUE
 9999 CONTINUE
      END
