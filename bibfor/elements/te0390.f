      SUBROUTINE TE0390 ( OPTION , NOMTE )
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
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - ELEMENT:  MECA_POU_D_T_GD
C      OPTION : 'FULL_MECA'   'RAPH_MECA'   'RIGI_MECA_TANG'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      REAL*8 NU,INSTAM,INSTAP
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(4),ELREFE
      CHARACTER*2        BL2, CODRET(4)
      REAL*8 EN(3,2),ENPRIM(3,2),VALRES(4),GRANC(6),GRANI(4),
     &RIGI(18,18),FINT(6,3),TEMPN(3),Y0(3),X00(3,3),X0K(3,3),X0PG(3),
     &TETAK(3,3),TETAG(3),TETAPG(3),QIM(3,3),QIKM1(3,3),QIK(3,3),
     &ROT0(3,3),ROTM(3,3),ROTKM1(3,3),ROTK(3,3),PETIK(3),PETIKM(3),
     &GN(3),GM(3),PN(3),PM(3),X0SK(3,3),RMKM1(3,3),RMK(3,3),OMKM1(3,3),
     &OMPKM1(3,3),OMK(3,3),OMPK(3,3),X0SEC(3),RGMKM(3),RGMK(3),OMGKM(3),
     &OMPGKM(3),OMGK(3),OMPGK(3),ALPHA,TREF
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      IF (OPTION(1:9) .EQ. 'FORC_NODA') GOTO 9999
C
      ZERO = 0.D0
      DEMI = 5.D-1
      UN   = 1.D0
      DEUX = 2.D0
C
C* STOUDY VAUT: 1., SI L'ON EST EN DYNAMIQUE
C*              0., SI L'ON EST EN STATIQUE
C
      CALL TECACH(.FALSE.,.FALSE.,'PSTADYN',1,ISTADY)
      IF (ISTADY.EQ.0) THEN
        STOUDY = 0.D0
      ELSE
        STOUDY = ZR(ISTADY)
      END IF
C
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
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      IF ( ZK16(ICOMPO+3)(1:9).EQ. 'COMP_INCR' ) THEN
         CALL UTMESS('F','TE0390','COMP_INCR NON VALIDE')
      ENDIF
      IF (ZK16(ICOMPO)(1:4).NE.'ELAS') THEN
        CALL UTMESS('F','TE0390_1',' RELATION : '//ZK16(ICOMPO)//
     &              ' NON IMPLANTEE SUR LES ELEMENTS "POU_D_T_GD"')
      ENDIF
      IF (ZK16(ICOMPO+2)(1:8).NE.'GREEN_GR') THEN
        CALL UTMESS('F','TE0390_2',' DEFORMATION : '//ZK16(ICOMPO+2)//
     &              ' NON IMPLANTEE SUR LES ELEMENTS "POU_D_T_GD"')
      ENDIF
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
      IF ( CODRET(3).NE.'OK' ) THEN
          IF ( STOUDY.GT.DEMI ) THEN
             CALL UTMESS('F','TE0390','RCVALA NE TROUVE PAS RHO, '//
     &                   'QUI EST NECESSAIRE EN DYNAMIQUE' )
          ELSE
             VALRES(3) = ZERO
          ENDIF
      ENDIF
      CALL RCVALA ( ZI(IMATE),'ELAS',0,'  ',R8BID,1,NOMRES(4),
     +              VALRES(4), CODRET(4), BL2 )
      IF ( CODRET(4).NE.'OK' ) THEN
             ALPHA   = 0.D0
          ELSE
            ALPHA   = VALRES(4)
      ENDIF
      E     = VALRES(1)
      NU    = VALRES(2)
      RHO   = VALRES(3)
      G     = E / (DEUX*(UN+NU))
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
C
C     --- LA SECTION EST SUPPOSEE CONSTANTE ---
      A     =  ZR(LSECT+1)
      XIY   =  ZR(LSECT+2)
      XIZ   =  ZR(LSECT+3)
      AY    =  ZR(LSECT+4)
      AZ    =  ZR(LSECT+5)
      XJX   =  ZR(LSECT+8)
      GRANC(1) = E * A
C     GRANC(1) = 1.D6
      GRANC(2) = G * A / AY
      GRANC(3) = G * A / AZ
      GRANC(4) = G * XJX
      GRANC(5) = E * XIY
      GRANC(6) = E * XIZ
C
C     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
      Y0(1) =  ZR(LORIEN    )
      Y0(2) =  ZR(LORIEN + 1)
      Y0(3) =  ZR(LORIEN + 2)
C PARAMETRES EN SORTIE
      IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &   OPTION(1:14) .EQ. 'RIGI_MECA_TANG'     ) THEN
        CALL JEVECH('PMATUNS','E',IMATUU)
C
        NORD = 6 * NNO
        DO 12 J=1,NORD
          DO 11 I=1,NORD
            RIGI(I,J) = ZERO
11        CONTINUE
12      CONTINUE
      ENDIF
      IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &   OPTION(1:9) .EQ. 'RAPH_MECA'     ) THEN
        CALL JEVECH('PVECTUR','E',JEFINT)
        CALL JEVECH('PCONTPR','E',LSIGMA)
        DO 14 NE=1,NNO
          DO 13 KC=1,6
            FINT(KC,NE) = ZERO
13        CONTINUE
14      CONTINUE
      ENDIF
C
      CALL JEVECH('PTEMPPR','L',ITEMPR)
      CALL JEVECH('PTEREF','L',ITEREF)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLMR','L',IDEPM)
      CALL JEVECH('PDEPLPR','L',IDEPDE)
      CALL JEVECH('PDDEPLA','L',IDDEPL)
C
      DO 21 I=1,NNO
        TEMPN(I) = ZR(ITEMPR-1+I)
21    CONTINUE
      TREF = ZR(ITEREF)
C
      K0 = IGEOM - 1
      K1 = IDEPM - 1
      K2 = IDEPDE - 1
      K3 = IDDEPL - 1
C
      DO 33 NE=1,NNO
        DO 31 KC=1,3
          K0 = K0 + 1
          K1 = K1 + 1
          K2 = K2 + 1
          K3 = K3 + 1
          X00(KC,NE) = ZR(K0)
          X0K(KC,NE) = ZR(K0) + ZR(K1) + ZR(K2)

31      CONTINUE
        DO 32 KC=1,3
          K1 = K1 + 1
          K2 = K2 + 1
          K3 = K3 + 1
          QIM(KC,NE) = ZR(K1)
          QIK(KC,NE) = ZR(K2)
          TETAK(KC,NE) = ZR(K3)
32      CONTINUE
33    CONTINUE
C
      CALL JEVECH('PVARIMP','E',IVARIM)
      CALL JEVECH('PVARIPR','E',IVARIP)
      IF (STOUDY.GT.DEMI) THEN
C* ON TRAITE UN PROBLEME DYNAMIQUE
         CALL JEVECH('PINSTMR','L',INSTMR)
         CALL JEVECH('PINSTPR','L',INSTPR)
         INSTAM = ZR(INSTMR)
         INSTAP = ZR(INSTPR)
         PAS = INSTAP - INSTAM
         GRANI(1) = RHO * XJX
         GRANI(2) = RHO * XIY
         GRANI(3) = RHO * XIZ
         GRANI(4) = RHO * A
C* PARAMETRES ALPHA ET DELTA DE NEWMARK
         ALFNMK = ZR(ISTADY + 1)
         DELNMK = ZR(ISTADY + 2)
C
         CALL JEVECH('PDEPKM1','L',IDEPKM)
         CALL JEVECH('PVITKM1','L',IVITKM)
         CALL JEVECH('PACCKM1','L',IACCKM)
         CALL JEVECH('PVITPLU','L',IVITP )
         CALL JEVECH('PACCPLU','L',IACCP )
         CALL JEVECH('PROMKM1','L',IROMKM)
         CALL JEVECH('PROMK'  ,'L',IROMK )
         K1 = IDEPKM - 1
         K2 = IVITKM - 1
         K3 = IACCKM - 1
         K4 = IVITP  - 1
         K5 = IACCP  - 1
         K6 = IROMKM - 1
         K7 = IROMK  - 1
         DO 38 NE=1,NNO
            DO 36 KC=1,3
               K1 = K1 + 1
               K2 = K2 + 1
               K3 = K3 + 1
               K4 = K4 + 1
               K5 = K5 + 1
               K6 = K6 + 1
               K7 = K7 + 1
               X0SK(KC,NE) = ZR(K5)
36          CONTINUE
            DO 37 KC=1,3
               K1 = K1 + 1
               K2 = K2 + 1
               K3 = K3 + 1
               K4 = K4 + 1
               K5 = K5 + 1
               K6 = K6 + 1
               K7 = K7 + 1
               QIKM1 (KC,NE) = ZR(K1)
               OMKM1 (KC,NE) = ZR(K2)
               OMPKM1(KC,NE) = ZR(K3)
               OMK   (KC,NE) = ZR(K4)
               OMPK  (KC,NE) = ZR(K5)
               RMKM1(KC,NE) = ZR(K6)
               RMK   (KC,NE) = ZR(K7)
37          CONTINUE
38       CONTINUE
      ENDIF
C
C* BOUCLE SUR LES POINTS DE GAUSS
C
      DO 51 KP=1,NPG
        CALL GDJRG0 (KP,NNO,ENPRIM,X00,Y0,   AJACOB,ROT0)
        PJACOB = ZR(IPOIDS-1+KP) * AJACOB
C*** LECTURE, DANS 'PVARIMR', DU VECTEUR-COURBURE A L'ITER. PRECEDENTE
        CALL GDLIVA (KP,ZR(IVARIM),   PETIKM)
C
        CALL GDSTAG (STOUDY,KP,NNO,AJACOB,EN,ENPRIM,X0K,TETAK,QIM,QIKM1,
     &         QIK,TEMPN,     X0PG,TETAG,TETAPG,ROTM,ROTKM1,ROTK,TEMPG)
        CALL GDPETK (TETAG,TETAPG,PETIKM,   PETIK)
C
C*** ECRITURE, DANS 'PVARIPR', DU VECTEUR-COURBURE ACTUALISE, CAR
C*** MAJSVT, UTILE POUR D'AUTRES ELEMENTS, COPIE VARIP DANS VARIM
        CALL GDECVA (KP,PETIK,   ZR(IVARIP))
C
        CALL GDSIG (X0PG,PETIK,ROT0,ROTK,GRANC,ALPHA,TEMPG,TREF,
     &              GN,GM,PN,PM)
      IF (STOUDY.GT.DEMI) THEN
C* ON TRAITE UN PROBLEME DYNAMIQUE
      CALL GDDYNG (KP,NNO,EN,X0SK,RMKM1,RMK,OMKM1,OMPKM1,OMK,OMPK,
     &             X0SEC,RGMKM,RGMK,OMGKM,OMPGKM,OMGK,OMPGK)
      ENDIF
C
        IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:14) .EQ. 'RIGI_MECA_TANG'   ) THEN
          CALL GDMRIG (KP,NNO,AJACOB,PJACOB,EN,ENPRIM,X0PG,ROT0,ROTK,
     &                 GRANC,PN,PM,   RIGI)
      IF (STOUDY.GT.DEMI) THEN
C* ON TRAITE UN PROBLEME DYNAMIQUE
      CALL GDMINE (KP,NNO,PJACOB,EN,GRANI,ALFNMK,DELNMK,PAS,ROT0,ROTM,
     &             ROTKM1,ROTK,RMKM1,RMK,OMGKM,OMPGKM,OMGK,OMPGK, RIGI)
      ENDIF
        ENDIF
C
        IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:9) .EQ. 'RAPH_MECA'   ) THEN
          CALL GDFINT (KP,NNO,AJACOB,PJACOB,EN,ENPRIM,X0PG,PN,PM,  FINT)
          LSIG = LSIGMA - 1 + (KP-1)*6
          DO 41 KS=1,3
            LSIG = LSIG + 1
C*** ATTENTION : LE TORSEUR EST EXPRIME EN COORDONNEES LOCALES
            ZR(LSIG) = GN(KS)
41        CONTINUE
          DO 42 KS=1,3
            LSIG = LSIG + 1
            ZR(LSIG) = GM(KS)
42        CONTINUE
      IF (STOUDY.GT.DEMI) THEN
      CALL GDFINE (KP,NNO,PJACOB,EN,GRANI,X0SEC,ROT0,ROTK,OMGK,OMPGK,
     &             FINT)
      ENDIF
        ENDIF
C
C* FIN DE BOUCLE SUR LES POINTS DE GAUSS
C
51    CONTINUE
C
        IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:14) .EQ. 'RIGI_MECA_TANG'   ) THEN
          IMAT = IMATUU - 1
          DO 62 I=1,NORD
            DO 61 J=1,NORD
              IMAT = IMAT +1
              ZR(IMAT) = RIGI(I,J)
61          CONTINUE
62        CONTINUE
        ENDIF
C
        IF(OPTION(1:9) .EQ. 'FULL_MECA' .OR.
     &     OPTION(1:9) .EQ. 'RAPH_MECA'   ) THEN
          IFINT = JEFINT - 1
          DO 72 NE=1,NNO
            DO 71 KC=1,6
              IFINT = IFINT + 1
              ZR(IFINT) = FINT(KC,NE)
71          CONTINUE
72        CONTINUE
C
          CALL JEVECH ( 'PCODRET', 'E', JCRET )
          ZI(JCRET) = 0
        ENDIF
 9999 CONTINUE
      END
