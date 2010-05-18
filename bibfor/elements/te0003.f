      SUBROUTINE TE0003(OPTION,NOMTE)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/05/2010   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE BOITEAU O.BOITEAU
C TOLE CRP_20
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DE L'ESTIMATEUR D'ERREUR EN RESIDU
C      SUR UN ELEMENT ISOPARAMETRIQUE 2D/3D, LUMPE OU NON, VIA L'OPTION
C     'ERRE_ELEM_THER_F/R'

C IN OPTION : NOM DE L'OPTION
C IN NOMTE  : NOM DU TYPE D'ELEMENT
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MESSAGE:INFNIV,U2MESS,U2MESG.
C       JEVEUX:JEMARQ,JEDEMA,JEVETE,JEVEUO,JELIRA,JENUNO.
C       CHAMP LOCAUX:JEVECH,TECACH.
C       ENVIMA:R8PREM,R8MIEM.
C       ELEMENTS FINIS:DFDM2D,DFDM3D.
C       MATERIAUX/CHARGES:RCVALA,RCCOMA,FOINTE.
C       NON LINEAIRE: NTFCMA,RCFODE.
C       DEDIEES A TE0003:UTHK,UTJAC,UTINTC,UTIN3D,UTNORM,UTNO3D,
C                        UTVOIS,UTERFL,UTEREC,UTERSA,UTNBNV.
C     FONCTIONS INTRINSEQUES:
C       ABS,SQRT,SIGN.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       22/06/01 (OB): CREATION EN S'INSPIRANT DE TE0377.F (2D) ET DE
C                      TE0375.F (3D).
C       05/02/02 (OB): EXTENSION AUX EFS LUMPES.
C       10/06/02 (OB): CORRECTION BUG DES ELREFE.
C       22/08/02 (OB): RAJOUT TABNIV POUR NIVEAU DIFFERENCIE SI INFO=2
C               RAJOUT DE L'OBJET '&&RESTHE.JEVEUO' POUR AMELIORER LES
C               PERFORMANCES DE RESTHE/CALCUL/TE0003.
C       12/09/02 (OB): ELIMINATION ALARME DU A LA DIVISION PAR ZERO
C                POUR CONSTRUIRE ERTREL + AFFICHAGES SUPL. SI INFO=2.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16 OPTION,NOMTE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
      INTEGER IFM,NIV
      INTEGER TABNIV(20)
      INTEGER IADZI,IAZK24
      INTEGER IBID,ITAB(7),IRET
      INTEGER NOE(9,6,4)
      INTEGER IGEOM
      INTEGER IERR,IVOIS
      INTEGER IMATE
      INTEGER ISOUR,ICHARG
      INTEGER IFLUM,IFLUP,ITEMM,ITEMP
      INTEGER NDIM
      INTEGER NNO , NNOS , NPG , IPOIDS, IVF , IDFDE , JGANO
      INTEGER NDIMF
      INTEGER NNOF, NNOSF, NPGF, IPOIDF, IVFF, IDFDXF, IDFDYF, JGANOF
      INTEGER NNO2, NNOS2, NPG2, IPOID2, IVF2, IDFDX2, IDFDY2, JGANO2
      INTEGER I,J,K,ICODE,I1,IJ,I2,MCELD,MCELV,PCELD,PCELV,IAUX1,
     >        IAVAF,NCMPF,IAVAH,NCMPH,JNO,MNO,
     >        IMAV,IAPTMA,IENTF,IENTH,IENTT,IAVAT,NCMPT,NBSV,
     >        NBNV,JAD,JADV,IGREL,IEL,IAVALP,IAVALM,IAREPE,
     >        NIVEAU,IFON(3),NBPAR,
     >        IJEVEO
      INTEGER IPG
      INTEGER NBF
      INTEGER TYMVOL,NDEGRE,IFA,TYV
C
      REAL*8 R8BID,R8BID3(4)
      REAL*8 R8PREM
      REAL*8 INSOLD,INST,VALTHE,AUX,RHOCP,DFDX(27),DFDY(27),
     >       POIDS,R,VALFP(9),VALFM(9),R8CART,VALSP,VALSM,
     >       VALUNT,TERBUF,TEMPM,TEMPP,FLUXM(3),FLUXP(3),FFORME,DELTAT,
     >       R8MIEM,PREC,OVFL,DER(6),FLURP,FLURM,UNSURR,X3,Y3,XN(9),
     >       YN(9),ZN(9),TERM22,JAC(9),HK,HF,ZRJNO1,ZRJNO2,ZRINO1,
     >       ZRINO2,POINC1,POINC2,VALHP(9),VALHM(9),VALTP(9),VALTM(9),
     >       R8CAR1,ERTABS,ERTREL,TERMNO,TERMVO,TERMSA,TERMFL,TERMEC,
     >       TERMV1,TERMV2,TERMS1,TERMS2,TERMF1,TERMF2,TERME1,TERME2,
     >       JACOB,UNSURD,RHOCPM,RHOCPP,DFDZ(27),X,Y,Z
C
      CHARACTER*2 CODRET(2),FORMV
      CHARACTER*4 EVOL
      CHARACTER*8 TYPMAV, ELREFE
      CHARACTER*8 ELREFF, ELREFB
      CHARACTER*8 MA,NOMPAR(4),TYPMAC,K8CART,K8CAR1
      CHARACTER*16 PHENOM
      CHARACTER*19 CARTEF,CARTEH,CARTET,CARTES,NOMGDF,NOMGDH,NOMGDT,
     >             NOMGDS,LIGREL,CHFLUM,CHFLUP
      CHARACTER*24 VALK(2)
      LOGICAL LEVOL,LTHETA,LAXI,LMAJ,LNONLI,L2D,LLUMPE,LTEATT
C
C --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE ----
C     REMARQUE : NE PAS SUPPRIMER CE COMMENTAIRE DETAILLE CAR IL EST
C                COMMUN A TOUS LES PROGRAMMES UTILISANT NOE
C
C     NOE (IN,IFA,TYMVOL) : IN     : NUMERO DU NOEUD DANS LA FACE
C                           IFA    : NUMERO DE LA FACE
C                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C                                    1 : HEXAEDRE A 8,20 ET 27 NOEUDS
C                                    2 : PENTAEDRE A 6 ET 15 NOEUDS
C                                    3 : TETRAEDRE A 4 ET 10 NOEUDS
C                                    4 : PYRAMIDE A 5 ET 13 NOEUDS
C  ==> POUR LE IN-EME NOEUD DE LA IFA-EME FACE D'UNE MAILLE DE TYPE
C      TYMVOL, NOE (IN,IFA,TYMVOL) EST SON NUMERO LOCAL DANS LA
C      DESCRIPTION DE LA MAILLE VOLUMIQUE.
C   . ON CHOISIT UNE ORIENTATION SORTANTE POUR DECRIRE UNE FACE.
C   . L'ORDRE DES FACES N'A PAS D'IMPORTANCE EN SOI MAIS IL FAUT
C     ETRE COHERENT :
C      - MEME DESCRIPTION PAR LES SOMMETS POUR LES VOISINAGES (RESVOI)
C      - CONVENTION POUR LE PENTAEDRE ET DE LA PYRAMIDE POUR LESQUELS
C        ON COMMENCE PAR LES TRIANGLES.
C
C      ON RAPPELLE QU'EN FORTRAN L'ORDRE DE RANGEMENT EST LE SUIVANT :
C   (1,1,1) (2,1,1) (3,1,1) ... (9,1,1) (1,2,1) (2,2,1) ... (9,2,1)
C   (1,3,1)  ...    (8,6,4) (9,6,4)
C    ON COMMENCE AINSI PAR LES 9 NOEUDS DE LA 1ERE FACE DE L'HEXAEDRE,
C    PUIS LES 9 NOEUDS DE LA 2EME FACE DE L'HEXAEDRE,
C    ETC JUSQU'AUX 9 NOEUDS DE LA 6EME FACE DE L'HEXAEDRE.
C    ENSUITE ON A LES 6 NOEUDS DE LA 1ERE FACE DU PENTAEDRE, ETC
C
C CONVENTIONS ASTER POUR UN HEXAEDRE :
C  (N1,N2,N3,N4) EST ENTRANT
C  (N5,N6,N7,N8) EST SORTANT (DONC DANS LE MEME SENS QUE (N1,N2,N3,N4))
C  N9 EST AU MILIEU DE (N1,N2)
C N10 EST AU MILIEU DE (N2,N3)
C N11 EST AU MILIEU DE (N3,N4)
C N12 EST AU MILIEU DE (N4,N1)
C N13 EST AU MILIEU DE (N1,N5)
C N14 EST AU MILIEU DE (N2,N6)
C N15 EST AU MILIEU DE (N3,N7)
C N16 EST AU MILIEU DE (N4,N8)
C N17 EST AU MILIEU DE (N5,N6)
C N18 EST AU MILIEU DE (N6,N7)
C N19 EST AU MILIEU DE (N7,N8)
C N20 EST AU MILIEU DE (N8,N5)
C
C             N1                  N2
C             ---------N9----------
C            /                   /.
C        N12/                  N10.
C          /                   /  .
C         /   N13             /   .
C      N4 ---------N11--------N3  .N14
C         .                  .    .
C         .                  .    .
C         .    .N5     N17   .    .N6
C      N16.                  N15 /
C         . N20              .  /N18
C         .                  . /
C         .                  ./
C         ---------N19--------
C       N8                    N7
C
C
C CONVENTIONS ASTER POUR UN PENTAEDRE :
C  (N1,N2,N3) EST ENTRANT
C  (N4,N5,N6) EST SORTANT (DONC DANS LE MEME SENS QUE (N1,N2,N3))
C  N7 EST AU MILIEU DE (N1,N2)
C  N8 EST AU MILIEU DE (N2,N3)
C  N9 EST AU MILIEU DE (N3,N1)
C N10 EST AU MILIEU DE (N1,N4)
C N11 EST AU MILIEU DE (N2,N5)
C N12 EST AU MILIEU DE (N3,N6)
C N13 EST AU MILIEU DE (N4,N5)
C N14 EST AU MILIEU DE (N5,N6)
C N15 EST AU MILIEU DE (N6,N4)
C
C          N2                     N11                  N5
C           X------------------------------------------X
C          .                                          .
C         .  .                                       .  .
C        .                                          .
C     N8.     .                                 N14.     .
C      .                                          .
C     .        .                                 .        .
C    .          N7                              .           N13
C N3.           .         N12                 N6.           .
C  X------------------------------------------X
C     .          .                               .          .
C          .                                      N15 .
C              .  .                                       .  .
C        N9        X------------------------------------------X
C                 N1                     N10                  N4
C
C CONVENTIONS ASTER POUR UN TETRAEDRE :
C  (N1,N2,N3) EST ENTRANT
C  N5 EST AU MILIEU DE (N1,N2)
C  N6 EST AU MILIEU DE (N2,N3)
C  N7 EST AU MILIEU DE (N3,N1)
C  N8 EST AU MILIEU DE (N1,N4)
C  N9 EST AU MILIEU DE (N2,N4)
C N10 EST AU MILIEU DE (N3,N4)
C
C                     N1
C                     *
C                    .  ..
C                   .     . .
C                  .        .  *N8
C                 .           .   .
C                .              *    .  N4
C             N7*               N5.    *
C              .                  . .   .
C             .              .        .  .
C            .          *N10             . *N9
C           .      .                      ..
C          .  .                             .
C         *................*.................*
C       N3                 N6                N2
C
C
C CONVENTIONS ASTER POUR UNE PYRAMIDE :
C  (N1,N2,N3,N4) EST ENTRANT
C  N5 EST LE SOMMET OPPOSE AU QUADRANGLE
C  N6 EST AU MILIEU DE (N1,N2)
C  N7 EST AU MILIEU DE (N2,N3)
C  N8 EST AU MILIEU DE (N3,N4)
C  N9 EST AU MILIEU DE (N4,N1)
C N10 EST AU MILIEU DE (N1,N5)
C N11 EST AU MILIEU DE (N2,N5)
C N12 EST AU MILIEU DE (N3,N5)
C N13 EST AU MILIEU DE (N4,N5)
C                            N5
C                            X
C                         . . . .
C                       .  .   .   .
C                     .   .   N13     .
C                   .    .       .       .
C                 .     .        X .         .
C             N10.      .     .   N4    .        .N12
C             .       .  .                 .       .
C           .        .                         .      .
C         .      .  .N11                           .     .
C       .    .     .                            N8     .    .
C     .  .  N9    .                                        .   .
C N1 .           .                                             .  .
C  X .         .                                                  .  .
C         .    .                                                      .
C       N6    X--------------------------------------------------------X
C           N2                           N7                          N3
C
      DATA NOE/1,4,3,2,12,11,10, 9,21, 1,2,6,5, 9,14,17,13,22,
     >         2,3,7,6,10,15,18,14,23, 3,4,8,7,11,16,19,15,24,
     >         4,1,5,8,12,13,20,16,25, 5,6,7,8,17,18,19,20,26,
     >         1,3,2,9,8,7, 3*0,       4,5,6,13,14,15, 3*0,
     >         1,2,5,4, 7,11,13,10, 0, 2,3,6,5,8,12,14,11,0,
     >         1,4,6,3,10,15,12, 9, 0,  9*0,
     >         1,3,2,7,6, 5, 3*0,      2,3,4,6,10,9, 3*0,
     >         3,1,4,7,8,10, 3*0,      1,2,4,5, 9,8, 3*0,
     >         9*0,                    9*0,
     >         1,2,5,6,11,10, 3*0,     2,3,5,7,12,11, 3*0,
     >         3,4,5,8,13,12, 3*0,     4,1,5,9,10,13, 3*0,
     >         1,4,3,2,9,8,7,6, 0,     9*0 /
C--------------------------------------------------------------------
C INITIALISATIONS/RECUPERATION DE LA GEOMETRIE ET DES CHAMPS LOCAUX
C--------------------------------------------------------------------
 1000 FORMAT(A,' :',(6(1X,1PE17.10)))
 1001 FORMAT(A,' :',9I10)
C 2000 FORMAT(A,10I8)
C ----------------------------------------------------------------------
C 1 -------------- GESTION DES DONNEES ---------------------------------
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)
C
      CALL TECAEL(IADZI,IAZK24)
      VALK(1)=ZK24(IAZK24-1+3)
      VALK(2)=OPTION
C
      CALL ELREF1(ELREFE)

C NIVEAU D'AFFICHAGE DIFFERENCIE PAR ROUTINE (0: RIEN, 2 AFFICHAGE)
      DO 10 IBID = 1,20
        TABNIV(IBID) = 0
   10 CONTINUE
      IF (NIV.EQ.2) THEN
C INFO GENERALES SUR L'ELEMENT K
        TABNIV(1) = 2
        TABNIV(2) = 0
C POUR UTHK: INFO SUR LE DIAMETRE DE L'ELEMENT K
        TABNIV(3) = 2
C AFFICHAGE DU PHENOMENE DE LA MAILLE ET DU RHOCP
        TABNIV(4) = 0
C AFFICHAGE DU JACOBIEN
        TABNIV(5) = 2
C AFFICHAGE DES DETAILS DE CONSTRUCTION DU TERME SOURCE
        TABNIV(6) = 0
C AFFICHAGE DU TOTAL DU TERME SOURCE
        TABNIV(7) = 2
C POUR UTNORM: INFO SUR HF, NORMAL, CONNECTIQUE DES FACES
        TABNIV(8) = 0
C POUR UTINTC/3D: INTERPOLATION FLUX + INFO CARTE
        TABNIV(9) = 0
C POUR UTERFL: CALCUL TERME ERREUR FLUX
        TABNIV(10) = 0
C AFFICHAGE DU TOTAL DU TERME DE FLUX
        TABNIV(11) = 2
C POUR UTINC/3D: INTERPOLATION ECHANGE + INFO CARTE
        TABNIV(12) = 0
C POUR UTERFL: CALCUL TERME ERREUR ECHANGE
        TABNIV(13) = 0
C AFFICHAGE DU TOTAL DU TERME D'ECHANGE
        TABNIV(14) = 2
C POUR UTERSA: CALCUL TERME ERREUR SAUT
        TABNIV(15) = 0
C AFFICHAGE DU TOTAL DU TERME DE SAUT
        TABNIV(16) = 2
      END IF
C
      IF (TABNIV(1).EQ.2) THEN
      WRITE(IFM,*) ' '
      WRITE(IFM,*) '================================================='
      WRITE(IFM,*) ' '
      WRITE(IFM,*) 'MAILLE NUMERO', ZI(IADZI),', DE TYPE ', ELREFE
      END IF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
C
      CALL JEVECH('PERREUR','E',IERR)
      DO 1 IBID=1,14
        ZR(IERR-1+IBID)=0.D0
 1    CONTINUE
C
C INITIALISATION DIMENSION DE L'ESPACE DE TRAVAIL/LLUMPE OU PAS
      LLUMPE = LTEATT(' ','LUMPE','OUI')
      L2D = (NDIM.EQ.2)
      IF ( LLUMPE ) THEN
        IF ( ( ELREFE(1:3).EQ.'H20' ) .OR.
     >       ( ELREFE(1:3).EQ.'H27' ) .OR.
     >       ( ELREFE(1:3).EQ.'P15' ) .OR.
     >       ( ELREFE(1:3).EQ.'T10' ) .OR.
     >       ( ELREFE(1:3).EQ.'P13' ) ) THEN
          CALL U2MESS('F','CALCULEL5_27')
        ENDIF
      ENDIF

C INIT. GENERALES (PARAM NUMERIQUE, INTERPOLATION, FLAG SI AXI...)
      PREC = R8PREM()
      OVFL = R8MIEM()
      NBPAR = NDIM + 1
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(NBPAR) = 'INST'
      LAXI = LTEATT(' ','AXIS','OUI')
      IF (.NOT.L2D) THEN
        NOMPAR(3) = 'Z'
      END IF

C RECUPERATION DES ADRESSES DES CHAMPS LOCAUX ASSOCIES AUX PARAMETRES:
C FLUX TEMP - ET +(IFLUM ET IFLUP),SOURCE (ISOUR),
C MATERIAU (IMATE),CHARGE (ICHARG),
C FLAG EVOL='EVOL' OU '',THETA (VALTHE),INSTANT - ET + (INSOLD/INST)
      CALL JEVECH('PCHARG','L',ICHARG)
      EVOL = ZK24(ICHARG+15)(1:4)
      IF (EVOL.EQ.'EVOL') THEN
        CALL JEVECH('PTEMP_M','L',ITEMM)
        CALL JEVECH('PFLUX_M','L',IFLUM)
        READ (ZK24(ICHARG+13),'(F19.8)') INSOLD
        READ (ZK24(ICHARG+12),'(F19.8)') VALTHE
        VALUNT = 1.D0 - VALTHE
        LEVOL = .TRUE.
      ELSE
        VALTHE = 1.D0
        VALUNT = 0.D0
        LEVOL = .FALSE.
      END IF

      READ (ZK24(ICHARG+14),'(F19.8)') INST
C FIN DE LECTURE DE LA CARTE ETENDUE: LECTURE VECT '&&RESTHE.JEVEUO'
      READ (ZK24(ICHARG+16),'(I24)') IJEVEO
      NIVEAU = ZI(IJEVEO)
      IFM = ZI(IJEVEO+1)
      NIV = ZI(IJEVEO+2)
      IAREPE = ZI(IJEVEO+3)
      MCELD = ZI(IJEVEO+4)
      MCELV = ZI(IJEVEO+5)
      PCELD = ZI(IJEVEO+6)
      PCELV = ZI(IJEVEO+7)
      IAVAF = ZI(IJEVEO+8)
      NCMPF = ZI(IJEVEO+9)
      IAVAH = ZI(IJEVEO+10)
      NCMPH = ZI(IJEVEO+11)
      IAVAT = ZI(IJEVEO+12)
      NCMPT = ZI(IJEVEO+13)

      CALL JEVECH('PTEMP_P','L',ITEMP)
      CALL JEVECH('PFLUX_P','L',IFLUP)
      CALL TECACH('ONN','PSOURCR',1,ISOUR,IRET)
      CALL JEVECH('PMATERC','L',IMATE)


C RECUPERATION DES CARTES FLUX, CHARGEMENT ET DE LEUR TYPE
      MA = ZK24(ICHARG)(1:8)
      CHFLUM = ZK24(ICHARG+2)(1:19)
      CHFLUP = ZK24(ICHARG+3)(1:19)
      CARTEF = ZK24(ICHARG+4)(1:19)
      NOMGDF = ZK24(ICHARG+5)(1:19)
      CARTEH = ZK24(ICHARG+6)(1:19)
      NOMGDH = ZK24(ICHARG+7)(1:19)
      CARTET = ZK24(ICHARG+8)(1:19)
      NOMGDT = ZK24(ICHARG+9)(1:19)
      CARTES = ZK24(ICHARG+10)(1:19)
      NOMGDS = ZK24(ICHARG+11)(1:19)

C FLAG POUR EFFECTUER LES CALCULS IMPLIQUANT (1-THETA)
      LTHETA = (VALUNT.GT.PREC)

C IMPRESSIONS NIVEAU 2 POUR DIAGNOSTIC
      IF (TABNIV(2).EQ.2) THEN
        WRITE (IFM,*) 'NOMTE : ',NOMTE,', L2D : ',L2D
        IF (L2D) WRITE (IFM,*) 'LAXI =',LAXI
        WRITE (IFM,*) 'VALTHE =',VALTHE,', VALUNT =',VALUNT
        WRITE (IFM,*) 'LEVOL =',LEVOL,', LTHETA =',LTHETA
        LIGREL = ZK24(ICHARG+1)(1:19)
        WRITE (IFM,*) 'MA = ',MA,', LIGREL = ',LIGREL
        WRITE (IFM,*) 'CHFLUM = ',CHFLUM,', CHFLUP = ',CHFLUP
        WRITE (IFM,*) 'CARTEF = ',CARTEF,', NOMGDF = ',NOMGDF
        WRITE (IFM,*) 'CARTEH = ',CARTEH,', NOMGDH = ',NOMGDH
        WRITE (IFM,*) 'CARTET = ',CARTET,', NOMGDT = ',NOMGDT
        WRITE (IFM,*) 'CARTES = ',CARTES,', NOMGDS = ',NOMGDS
      END IF

C ----------------------------------------------------------------------
C ---------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
C ----- CALCUL DU DIAMETRE HK DE LA MAILLE ----
      CALL UTHK(NOMTE,IGEOM,HK,NDIM,ITAB,IBID,IBID,IBID,TABNIV(3),IFM)

C------------------------------------------------------------------
C CALCUL DU TERME VOLUMIQUE
C------------------------------------------------------------------

C RECHERCHE DE LA VALEUR DE RHO*CP EN LINEAIRE ET EN NON-LINEAIRE
      CALL RCCOMA(ZI(IMATE),'THER',PHENOM,CODRET)
      CALL ASSERT(CODRET(1).EQ.'OK')
      IF (PHENOM.EQ.'THER') THEN
        LNONLI = .FALSE.
        CALL RCVALA(ZI(IMATE),' ',PHENOM,1,'INST',INST,1,'RHO_CP',
     &             RHOCP, CODRET,'FM')
        IF (CODRET(1).NE.'OK') CALL U2MESS('F','ELEMENTS2_62')
      ELSE IF (PHENOM.EQ.'THER_NL') THEN
        LNONLI = .TRUE.
        CALL NTFCMA(ZI(IMATE),IFON)
        CALL U2MESS('A', 'ELEMENTS4_91')
      ELSE
        CALL U2MESS('F','ELEMENTS2_63')
      END IF
      IF (TABNIV(4).EQ.2) THEN
        WRITE (IFM,*) 'PHENOM =',PHENOM
        IF (.NOT.LNONLI) WRITE (IFM,1000) 'RHOCP',RHOCP
      END IF

C---------------------------------
C ----- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -------------
C---------------------------------
      TERMVO = 0.D0
      TERMV1 = 0.D0
      AUX = 0.D0
      LMAJ = .FALSE.
      DO 12 , IPG = 1,NPG

        TERBUF = 0.D0
        K = (IPG-1)*NNO
C FONCTIONS DE FORME ET LEURS DERIVEES
        IF (L2D) THEN
          CALL DFDM2D(NNO,IPG,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        ELSE
          CALL DFDM3D ( NNO, IPG, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
        END IF

C CALCUL L'ORIENTATION DE LA MAILLE
        CALL UTJAC ( L2D, IGEOM, IPG, IDFDE,
     &                                    TABNIV(5), IFM, NNO, JACOB )

C---------------------------------
C CALCUL DE LA PARTIE SOURCE (THETA * S+ + (1-THETA) * S-)
C---------------------------------
C---------------------------------
C CAS SOURCE VARIABLE
C---------------------------------
        IF (NOMGDS(1:6).EQ.'SOUR_F') THEN

C SOURCE FONCTION (R,Z,TEMPS) (VALSP/M S+/- AU POINT DE GAUSS)
          X = 0.D0
          Y = 0.D0
          Z = 0.D0
          VALSP = 0.D0
          VALSM = 0.D0
          DO 20 I = 1,NNO
            I1 = I - 1
            I2 = IGEOM + NDIM*I1
            FFORME = ZR(IVF+K+I1)
            X = X + ZR(I2)*FFORME
            Y = Y + ZR(I2+1)*FFORME
            IF (.NOT.L2D) Z = Z + ZR(I2+2)*FFORME
   20     CONTINUE
          R8BID3(1) = X
          R8BID3(2) = Y
          IF (.NOT.L2D) R8BID3(3) = Z
          R8BID3(NBPAR) = INST
          CALL FOINTE('FM',ZK8(ISOUR),NBPAR,NOMPAR,R8BID3,VALSP,ICODE)
          IF (LTHETA) THEN
            R8BID3(NBPAR) = INSOLD
            CALL FOINTE('FM',ZK8(ISOUR),NBPAR,NOMPAR,R8BID3,VALSM,ICODE)
            TERBUF = VALTHE*VALSP + VALUNT*VALSM
          ELSE
            TERBUF = VALTHE*VALSP
          END IF
          IF (TABNIV(6).EQ.2) THEN
            WRITE (IFM,1000) 'X / Y / Z / INST ',X,Y,Z,INST
            WRITE (IFM,1000) 'TERMVO SOUR_F',VALSP,VALSM
          END IF

C---------------------------------
C CAS SOURCE CONSTANTE
C---------------------------------
        ELSE IF (NOMGDS(1:6).EQ.'SOUR_R') THEN
          TERBUF = ZR(ISOUR+IPG-1)
          IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO SOUR_R',TERBUF
        END IF

        IF (TERBUF.GT.PREC) LMAJ = .TRUE.
C CALCUL PRELIMINAIRE POUR TERMNO (TERME SOURCE DE NORMALISATION)
        AUX = TERBUF

C---------------------------------
C CALCUL DE LA PARTIE DIFFERENCE FINIE (RHOCP*DELTA TEMP/DELTA T)
C---------------------------------

        IF (LEVOL) THEN

C TEMPM/P T-/+ AU POINT DE GAUSS
          TEMPM = 0.D0
          TEMPP = 0.D0
          DO 30 I = 1,NNO
            I1 = I - 1
            FFORME = ZR(IVF+K+I1)
            TEMPM = TEMPM + ZR(ITEMM+I1)*FFORME
            TEMPP = TEMPP + ZR(ITEMP+I1)*FFORME
   30     CONTINUE
          DELTAT = INST - INSOLD
          CALL ASSERT(DELTAT.GT.OVFL)
          UNSURD = 1.D0/DELTAT
          IF (LNONLI) THEN
C CAS NON LINEAIRE
C INTERPOLATION DERIVEE CHAMP D'ENTHALPIE IFON(1) A T- ET T+
            CALL RCFODE(IFON(1),TEMPM,R8BID,RHOCPM)
            CALL RCFODE(IFON(1),TEMPP,R8BID,RHOCPP)
            TERBUF = TERBUF - (RHOCPP-RHOCPM)*UNSURD
          IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'RHOCPP/M',RHOCPP,RHOCPM
          ELSE
C CAS LINEAIRE
            TERBUF = TERBUF - (TEMPP-TEMPM)*RHOCP*UNSURD
          END IF
C IMPRESSIONS NIVEAU 2 POUR DIAGNOSTIC...
          IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO TEMPP/M/DELTAT',
     &        TEMPP,TEMPM,DELTAT
        END IF

C---------------------------------
C CALCUL DE LA PARTIE DIVERGENCE (DIV(THETA*Q+ + (1-THETA)*Q-))
C---------------------------------

        DO 40 I = 1,NDIM
          FLUXM(I) = 0.D0
          FLUXP(I) = 0.D0
   40   CONTINUE

C TRAITEMENT PARTICULIER DU A L'AXI (PART I)
        IF (LAXI) THEN
          R = 0.D0
          FLURM = 0.D0
          FLURP = 0.D0
        END IF

        DO 60 I = 1,NNO
          I1 = I - 1
          I2 = I1*NDIM
          DER(1) = DFDX(I)
          DER(2) = DFDY(I)
C TRAITEMENT PARTICULIER DU A L'AXI (PART II)
          IF (LAXI) THEN
            DER(4) = ZR(IVF+K+I1)
            R = R + ZR(IGEOM+I2)*DER(4)
            FLURP = FLURP + ZR(IFLUP+I2)*DER(4)
            IF (LTHETA) FLURM = FLURM + ZR(IFLUM+I2)*DER(4)
          END IF
          IF (.NOT.L2D) DER(3) = DFDZ(I)
          DO 50 J = 1,NDIM
            IJ = I2 + J - 1
            FLUXP(J) = FLUXP(J) + ZR(IFLUP+IJ)*DER(J)
            IF (LTHETA) FLUXM(J) = FLUXM(J) + ZR(IFLUM+IJ)*DER(J)
   50     CONTINUE
   60   CONTINUE

C TRAITEMENT PARTICULIER DU A L'AXI (PART III)
        IF (LAXI) THEN
          CALL ASSERT(ABS(R).GT.OVFL)
          UNSURR = 1.D0/R
          POIDS = POIDS*R
        END IF
        IF (L2D) THEN
          TERBUF = TERBUF - VALTHE* (FLUXP(1)+FLUXP(2))
          IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO FLUXP',
     &        FLUXP(1),FLUXP(2)
          IF (LAXI) THEN
            TERBUF = TERBUF - VALTHE*UNSURR*FLURP
            IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO FLURP/R',
     &          FLURP*UNSURR,R
          END IF
          IF (LTHETA) THEN
            TERBUF = TERBUF - VALUNT* (FLUXM(1)+FLUXM(2))
            IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO FLUXM',
     &          FLUXM(1),FLUXM(2)
            IF (LAXI) THEN
              TERBUF = TERBUF - VALUNT*UNSURR*FLURM
              IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO FLURM',
     &            FLURM*UNSURR
            END IF
          END IF
        ELSE
          TERBUF = TERBUF - VALTHE* (FLUXP(1)+FLUXP(2)+FLUXP(3))
          IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO FLUXP',
     &        FLUXP(1),FLUXP(2),FLUXP(3)
          IF (LTHETA) THEN
            TERBUF = TERBUF - VALUNT* (FLUXM(1)+FLUXM(2)+FLUXM(3))
            IF (TABNIV(6).EQ.2) WRITE (IFM,1000) 'TERMVO FLUXM',
     &          FLUXM(1),FLUXM(2),FLUXM(3)
          END IF
        END IF
        TERMVO = TERMVO + TERBUF*TERBUF*POIDS
        TERMV1 = TERMV1 + AUX*AUX*POIDS

C---------------------------------
C FIN BOUCLE SUR LES POINTS DE GAUSS --------------------------------
C---------------------------------
   12 CONTINUE

C CALCUL FINAL DU TERME VOLUMIQUE ET DU TERME SOURCE DE NORMALISATION
      TERMVO = HK*SQRT(TERMVO)
      TERMV1 = HK*SQRT(TERMV1)
      IF (TABNIV(7).EQ.2) WRITE (IFM,1000) '---> TERMVO/TERMV1',
     &    TERMVO,TERMV1

C
C ----------------------------------------------------------------------
C ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C --------- CALCUL DES DEUXIEME ET TROISIEME TERMES DE L'ERREUR --------
C ----------------------------------------------------------------------
C
      CALL JEVECH('PVOISIN','L',IVOIS)
C
      IF ( L2D ) THEN
C TYPE DE LA MAILLE COURANTE
      IBID = ZI(IVOIS+7)
      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',IBID),TYPMAC)
C CALCUL DES CARACTERISTIQUES DE SES ARETES (OU DE SES FACES) ET
C RECUPERATION DE SES DERIVEES DE FONCTIONS DE FORME (EN 3D ONLY).
      CALL UTVOIS(TYPMAC,LMAJ,NBF,NNOSF,POINC1,POINC2,
     &            ELREFF,NDEGRE)
      ELSE
C
C --------- INFORMATIONS SUR LA MAILLE COURANTE : --------------------
C       TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C       NDEGRE : DEGRE DE L'ELEMENT
C       NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
C       ELREFF : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 1
C       ELREFB : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 2
C      --- REMARQUE : ON IMPOSE UNE FAMILLE DE POINTS DE GAUSS
C
      CALL ELREF7 ( ELREFE,
     >              TYMVOL, NDEGRE, NBF, ELREFF, ELREFB )
C
CGN      WRITE(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',TYMVOL
C --- CARACTERISTIQUES DES FACES DE BORD DE LA FAMILLE 1 ---------------
CGN      WRITE(IFM,*) 'ELREFF : ',ELREFF
      CALL ELREF4 ( ELREFF, 'MASS',
     &              NDIMF, NNOF, NNOSF, NPGF, IPOIDF, IVFF,
     &              IDFDXF, JGANOF )
      IDFDYF = IDFDXF + 1
CGN      WRITE(IFM,2000) 'NDIMF,NNOF',NDIMF,NNOF
CGN      WRITE(IFM,2000) 'NNOSF,NPGF',NNOSF,NPGF
CGN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
C
C --- COMPLEMENT EVENTUEL POUR LES MAILLES QUI ONT 2 TYPES DE ---
C --- MAILLES DE BORD (PENTAEDRE, PYRAMIDE) ---
C
      IF ( ELREFB(1:1).NE.' ' ) THEN
         CALL ELREF4 ( ELREFB, 'NOEU',
     &                 NDIMF, NNO2, NNOS2, NPG2, IPOID2, IVF2,
     &                 IDFDX2, JGANO2 )
        IDFDY2 = IDFDX2 + 1
CGN       WRITE(IFM,2000) 'NDIMF,NNO2',NDIMF,NNO2
CGN       WRITE(IFM,2000) 'NNOS2,NPG2',NNOS2,NPG2
CGN       WRITE(IFM,1000) 'IPOID2', (ZR(IPOID2+IFA),IFA=0,NPG2-1)
      ENDIF

      ENDIF

      IF (TABNIV(1).EQ.2) THEN
        WRITE (IFM,*) 'DIAMETRE ',HK
        IF (L2D) THEN
          WRITE (IFM,*) ' ARETES DE TYPE ',ELREFF
        ELSE
          WRITE (IFM,*) ' FACES DE TYPE   ',ELREFF,ELREFB
        END IF
        WRITE (IFM,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
      END IF
C---------------------------------
C BOUCLE SUR LES ARETES OU LES FACES DE LA MAILLE VOLUMIQUE -------
C---------------------------------
      TERMSA = 0.D0
      TERMS1 = 0.D0
      TERMFL = 0.D0
      TERMF1 = 0.D0
      TERMEC = 0.D0
      TERME1 = 0.D0
      DO 300 , IFA=1,NBF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        TYV=ZI(IVOIS+7+IFA)
C
        IF ( TYV.NE.0 ) THEN
C
C ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
C
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',TYV),TYPMAV)
      IF (TABNIV(1).EQ.2) THEN
      WRITE(IFM,*) 'IFA =', IFA,' ==> TYPMAV = ', TYPMAV
      ENDIF
C---------------------------------
C CALCULS PRELIMINAIRES
C---------------------------------

C CALCUL HF + POINTS INTEGRATION + NORMALE/JACOBIEN
        IF (L2D) THEN
C CAS 2D
          CALL UTNORM(IGEOM,NNOSF,NBF,IFA,POINC1,POINC2,JNO,MNO,
     &                ZRINO2,ZRINO1,ZRJNO2,ZRJNO1,X3,Y3,HF,XN,YN,JAC,
     &                LAXI,JACOB,IFM,TABNIV(8))
        ELSE

C CAS 3D
C
C --- QUAND ON ARRIVE AUX FACES QUAD DES PENTAEDRES OU DES PYRAMIDES ---
C --- IL FAUT REMPLACER LES CARACTERISTIQUES DE LA FAMILLE 1         ---
C --- PAR CELLES DE LA FAMILLE 2                                     ---
C
          IF ( ( TYMVOL.EQ.2 .AND. IFA.GE.3 ) .OR.
     >         ( TYMVOL.EQ.4 .AND. IFA.GE.5 ) ) THEN
C
            NNOF   = NNO2
            NPGF   = NPG2
            NNOSF  = NNOS2
            IPOIDF = IPOID2
            IDFDXF = IDFDX2
            IDFDYF = IDFDY2
C
          ENDIF
C
          IBID = IFA
          CALL UTNO3D(IFM,TABNIV(8),NNOSF,IBID,TYMVOL,IGEOM,
     &                XN,YN,ZN,JAC,
     &                IDFDXF,IDFDYF,HF,ZR(IPOIDF),NPGF,NOE)
C
        END IF

C TEST DU TYPE DE LA MAILLE VOISINE PARTAGEANT L'ARETE/LA FACE IFA
        FORMV = TYPMAV(1:2)

C NUMERO DE CETTE MAILLE VOISINE
        IMAV = ZI(IVOIS+IFA)

        IF (TABNIV(1).EQ.2) THEN
        WRITE(IFM,1003)  IFA, IMAV, TYPMAV
 1003 FORMAT (I2,'-EME FACE DE NUMERO',I10,' ==> TYPMAV = ', A)
          CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMAV),'L',IAUX1)
          WRITE (IFM,1001) '<<< DE PREMIERS SOMMETS ',
     &      (ZI(IAUX1+IBID),IBID=0,NNOSF-1)
        END IF
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
C --------------- LE BORD VOISIN EST UNE MAILLE DE PEAU ----------------
C ----------------------------------------------------------------------
C
        IF ((L2D.AND. ((FORMV.EQ.'SE').OR. (FORMV.EQ.'SL'))) .OR.
     &      (.NOT.L2D.AND. ((FORMV.EQ.'QU').OR. (FORMV.EQ.'TR').OR.
     &      (FORMV.EQ.'QL').OR. (FORMV.EQ.'TL')))) THEN

C---------------------------------
C CALCUL DE LA PARTIE FLUX
C           (THETA*(F+ + NU.Q+)+(1-THETA)*(F- + NU.Q-))
C---------------------------------

C POINTEUR CARTE FLUX IMPOSE
          IF (CARTEF.NE.' ') THEN
            CALL JEEXIN(CARTEF//'.PTMA',IRET)
            IF (IRET.EQ.0) THEN
C CARTE CONSTANTE
              IENTF = 1
            ELSE
C LA CARTE A ETE ETENDUE
              CALL JEVEUO(CARTEF//'.PTMA','L',IAPTMA)
              IENTF = ZI(IAPTMA-1+IMAV)
            END IF
          END IF

C---------------------------------
C CAS FLUX VARIABLE
C---------------------------------

          LMAJ = .FALSE.
          IF (NOMGDF(1:6).EQ.'FLUN_F') THEN

C FLUX IMPOSE FONCTION (VALFP/M(I)=Q+/- AU POINT IFA/JNO/MNO)
            K8CART = ZK8(IAVAF+ (IENTF-1)*NCMPF)
            IF (TABNIV(9).EQ.2) WRITE (IFM,*) 'TERMFL FLUN_F ',K8CART
            IF (K8CART.NE.'&FOZERO') THEN

              LMAJ = .TRUE.
C INTERPOLATION F AUX INSTANTS +/-
              IF (L2D) THEN
                CALL UTINTC(ZRINO2,ZRINO1,ZRJNO2,ZRJNO1,X3,Y3,INST,
     &                      INSOLD,K8CART,LTHETA,NNOSF,VALFP,VALFM,
     &                      TABNIV(9),IFM,1)
              ELSE
                CALL UTIN3D(IGEOM,NNOSF,IFA,TYMVOL,INST,INSOLD,K8CART,
     &                      LTHETA,TABNIV(9),IFM,1,VALFP,VALFM,NOE)
              END IF

C CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
              CALL UTERFL(NDIM,IFLUP,IFLUM,IFA,MNO,JNO,NNOSF,JAC,TERM22,
     &                    AUX,LTHETA,VALTHE,VALUNT,TABNIV(10),IFM,XN,YN,
     &                    ZN,VALFP,VALFM,TYMVOL,NOE)
            END IF

C---------------------------------
C CAS FLUX CONSTANT
C---------------------------------
          ELSE IF (NOMGDF(1:6).EQ.'FLUN_R') THEN

C FLUX IMPOSE CONSTANT (VALFP(I)=Q AU POINT IFA/JNO/MNO VALFM(I)=0)
            R8CART = ZR(IAVAF+ (IENTF-1)*NCMPF)
            IF (TABNIV(9).EQ.2) WRITE (IFM,*) 'TERMFL FLUN_R',R8CART
            IF (ABS(R8CART).GT.PREC) THEN

              LMAJ = .TRUE.
C CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
              DO 100 I = 1,NNOSF
                VALFP(I) = R8CART
  100         CONTINUE
              IF (LTHETA) THEN
                DO 110 I = 1,NNOSF
                  VALFM(I) = R8CART
  110           CONTINUE
              END IF
              CALL UTERFL(NDIM,IFLUP,IFLUM,IFA,MNO,JNO,NNOSF,JAC,TERM22,
     &                    AUX,LTHETA,VALTHE,VALUNT,TABNIV(10),IFM,XN,YN,
     &                    ZN,VALFP,VALFM,TYMVOL,NOE)
            END IF
          END IF

C MISE A JOUR DU TERME DE FLUX
          IF (LMAJ) THEN
            TERM22 = SQRT(HF*TERM22)
            AUX = SQRT(HF*AUX)
            TERMFL = TERMFL + TERM22
            TERMF1 = TERMF1 + AUX
            IF (TABNIV(11).EQ.2) THEN
              WRITE (IFM,1000) '---> TERMFL/TERMF1 ',TERM22,AUX
              WRITE (IFM,*)
            END IF
            LMAJ = .FALSE.
          END IF

C---------------------------------
C CALCUL DE LA PARTIE ECHANGE AVEC L'EXTERIEUR
C (THETA*(H+*(TEXT+ - T+)+NU.Q+)+(1-THETA)*(H-*(TEXT- - T-)+NU.Q-)
C---------------------------------

          IF (CARTEH.NE.' ') THEN
            CALL JEEXIN(CARTEH//'.PTMA',IRET)
            IF (IRET.EQ.0) THEN
              IENTH = 1
            ELSE
              CALL JEVEUO(CARTEH//'.PTMA','L',IAPTMA)
              IENTH = ZI(IAPTMA-1+IMAV)
            END IF
            CALL JEEXIN(CARTET//'.PTMA',IRET)
            IF (IRET.EQ.0) THEN
              IENTT = 1
            ELSE
              CALL JEVEUO(CARTET//'.PTMA','L',IAPTMA)
              IENTT = ZI(IAPTMA-1+IMAV)
            END IF
          END IF

C COMPTE-TENU DU PERIMETRE DE AFFE_CHAR_THER ON A SIMULTANEMENT
C (COEH_F,TEMP_F) OU  (COEH_R,TEMP_R)
C---------------------------------
C CAS ECHANGE VARIABLE
C---------------------------------

          LMAJ = .FALSE.
          IF (NOMGDH(1:6).EQ.'COEH_F') THEN

C COEF_H IMPOSE FONCTION (VALHP/M(I)=Q+/- AU POINT IFA/JNO/MNO)
            K8CART = ZK8(IAVAH+ (IENTH-1)*NCMPH)
C TEMPERATURE IMPOSEE FONCTION (VALTP/M(I)=TEXT+/-  IFA/JNO/MNO)
            K8CAR1 = ZK8(IAVAT+ (IENTT-1)*NCMPT)
            IF (TABNIV(12).EQ.2) WRITE (IFM,*) 'TERMEC COEH_F ',K8CART,
     &          ' / ',K8CAR1

            IF ((K8CART.NE.'&FOZERO') .AND. (K8CAR1.NE.'&FOZERO')) THEN

              LMAJ = .TRUE.
              IF (L2D) THEN
C INTERPOLATION H AUX INSTANTS +/-
                CALL UTINTC(ZRINO2,ZRINO1,ZRJNO2,ZRJNO1,X3,Y3,INST,
     &                      INSOLD,K8CART,LTHETA,NNOSF,VALHP,VALHM,
     &                      TABNIV(12),IFM,2)
C INTERPOLATION TEXT AUX INSTANTS +/-
                CALL UTINTC(ZRINO2,ZRINO1,ZRJNO2,ZRJNO1,X3,Y3,INST,
     &                      INSOLD,K8CAR1,LTHETA,NNOSF,VALTP,VALTM,
     &                      TABNIV(12),IFM,3)
              ELSE
                CALL UTIN3D(IGEOM,NNOSF,IFA,TYMVOL,INST,INSOLD,K8CART,
     &                      LTHETA,TABNIV(12),IFM,2,VALHP,VALHM,NOE)
                CALL UTIN3D(IGEOM,NNOSF,IFA,TYMVOL,INST,INSOLD,K8CAR1,
     &                      LTHETA,TABNIV(12),IFM,3,VALTP,VALTM,NOE)
              END IF

C CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
              CALL UTEREC(NDIM,IFLUP,IFLUM,IFA,MNO,JNO,NNOSF,JAC,TERM22,
     &                    AUX,LTHETA,VALTHE,VALUNT,TABNIV(13),IFM,XN,YN,
     &                    ZN,VALHP,VALHM,VALTP,VALTM,TYMVOL,ITEMP,ITEMM,
     &                    NOE)

            END IF

C---------------------------------
C CAS ECHANGE CONSTANT
C---------------------------------
          ELSE IF (NOMGDH(1:6).EQ.'COEH_R') THEN

C COEF_H IMPOSE CONSTANT (VALHP(I)=Q AU POINT IFA/JNO/MNO VALHM(I)=0)
            R8CART = ZR(IAVAH+ (IENTH-1)*NCMPH)
C TEMPERATURE IMPOSEE CONSTANT (VALTP(I)=TEXT+  IFA/JNO/MNO VALTM(I)=0)
            R8CAR1 = ZR(IAVAT+ (IENTT-1)*NCMPT)
            IF (TABNIV(12).EQ.2) WRITE (IFM,1000) 'TERMEC COEH_R',
     &          R8CART,R8CAR1

            IF ((ABS(R8CART).GT.PREC) .OR. (ABS(R8CAR1).GT.PREC)) THEN

              LMAJ = .TRUE.
C CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
              DO 120 I = 1,NNOSF
                VALHP(I) = R8CART
                VALTP(I) = R8CAR1
  120         CONTINUE
              IF (LTHETA) THEN
                DO 130 I = 1,NNOSF
                  VALHM(I) = R8CART
                  VALTM(I) = R8CAR1
  130           CONTINUE
              END IF
              CALL UTEREC(NDIM,IFLUP,IFLUM,IFA,MNO,JNO,NNOSF,JAC,TERM22,
     &                    AUX,LTHETA,VALTHE,VALUNT,TABNIV(13),IFM,XN,YN,
     &                    ZN,VALHP,VALHM,VALTP,VALTM,TYMVOL,ITEMP,ITEMM,
     &                    NOE)
            END IF
          END IF

C CALCUL FINAL DU TERME D'ECHANGE
          IF (LMAJ) THEN
            TERM22 = SQRT(HF*TERM22)
            AUX = SQRT(HF*AUX)
            TERMEC = TERMEC + TERM22
            TERME1 = TERME1 + AUX
            IF (TABNIV(14).EQ.2) THEN
              WRITE (IFM,1000) '---> TERMEC/TERME1',TERM22,AUX
              WRITE (IFM,*)
            END IF
            LMAJ = .FALSE.
          END IF

C ----------------------------------------------------------------------
C --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
C --------------- LE BORD VOISIN EST UN VOLUME -------------------------
C ----------------------------------------------------------------------
C
C---------------------------------
C CALCUL DE LA PARTIE SAUT DU FLUX CALCULE
C---------------------------------

        ELSE IF ((L2D.AND. ((FORMV.EQ.'TR').OR. (FORMV.EQ.'QU').OR.
     &           (FORMV.EQ.'TL').OR. (FORMV.EQ.'QL'))) .OR.
     &           (.NOT.L2D.AND. ((FORMV.EQ.'HE').OR. (FORMV.EQ.'PE').OR.
     &            (FORMV.EQ.'TE')))) THEN

C CALCUL DU TYPE DE MAILLE VOISINE ET DE SON NBRE DE SOMMETS
          CALL UTNBNV(TYPMAV,NBSV,NBNV)

C NUMERO DU LIGREL DE LA MAILLE VOISINE DE NUMERO GLOBAL IMAV
          IGREL = ZI(IAREPE+2* (IMAV-1))
C INDICE DE LA MAILLE VOISINE DANS LE IGREL
          IEL = ZI(IAREPE+2* (IMAV-1)+1)
          IF (TABNIV(15).EQ.2) WRITE (IFM,*) 'IGREL/IEL',IGREL,IEL

C INDICE DE DEPART DANS LA SD .CELV DU FLUX + ET -. ADDRESSE
C DANS ZR DE LA CMP 1 DU PT 1 DE LA MAILLE 1 DU GREL IEL
          IAVALP = PCELV - 1 + ZI(PCELD-1+ZI(PCELD-1+4+IGREL)+8)
          IF (LTHETA) IAVALM = MCELV - 1 +
     &                         ZI(MCELD-1+ZI(MCELD-1+4+IGREL)+8)

C CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
          CALL UTERSA(NDIM,IFLUP,IFLUM,IFA,MNO,JNO,IVOIS,MA,IEL,NBNV,
     &                NBSV,IAVALP,IAVALM,NNOSF,JAC,LTHETA,VALTHE,VALUNT,
     &                TABNIV(15),IFM,TYMVOL,XN,YN,ZN,TERM22,AUX,
     &                JAD,JADV,NOE)

C CALCUL FINAL DU TERME DE SAUT
          TERM22 = 0.5D0*SQRT(HF*TERM22)
          AUX = 0.5D0*SQRT(HF*AUX)
          TERMSA = TERMSA + TERM22
          TERMS1 = TERMS1 + AUX
          IF (TABNIV(16).EQ.2) THEN
            WRITE (IFM,1000) '---> TERMSA/TERMS1',TERM22,AUX
            WRITE (IFM,*)
          END IF
C
C ----------------------------------------------------------------------
C --------------- CURIEUX ----------------------------------------------
C ----------------------------------------------------------------------
C
        ELSE
C
          VALK(1)=TYPMAV(1:4)
          CALL U2MESK('F','INDICATEUR_10',1,VALK)
C---------------------------------
C FIN IF FORMV                    --------------------------------
C---------------------------------
        END IF
C---------------------------------
C FIN IF (TYV.NE.0) THEN          --------------------------------
C---------------------------------
        END IF
C---------------------------------
C FIN DE BOUCLE SUR LES ARETES    --------------------------------
C---------------------------------
  300 CONTINUE

C---------------------------------
C MISE EN FORME DES DIFFERENTS TERMES DE ERRETEMP
C---------------------------------

C ERREUR TOTALE
      ERTABS = TERMVO + TERMSA + TERMFL + TERMEC
      TERMNO = TERMV1 + TERMS1 + TERMF1 + TERME1
      ERTREL = 0.D0
      IF (TERMNO.GT.OVFL) ERTREL = (100.D0*ERTABS)/TERMNO

C ERREURS PARTIELLES
      IF (NIVEAU.EQ.2) THEN
        TERMV2 = 0.D0
        IF (TERMV1.GT.OVFL) TERMV2 = 100.D0* (TERMVO/TERMV1)
        TERMS2 = 0.D0
        IF (TERMS1.GT.OVFL) TERMS2 = 100.D0* (TERMSA/TERMS1)
        TERMF2 = 0.D0
        IF (TERMF1.GT.OVFL) TERMF2 = 100.D0* (TERMFL/TERMF1)
        TERME2 = 0.D0
        IF (TERME1.GT.OVFL) TERME2 = 100.D0* (TERMEC/TERME1)
      END IF

C STOCKAGE
      ZR(IERR) = ERTABS
      ZR(IERR+1) = ERTREL
      ZR(IERR+2) = TERMNO
      IF (NIVEAU.EQ.2) THEN
        ZR(IERR+3) = TERMVO
        ZR(IERR+4) = TERMV2
        ZR(IERR+5) = TERMV1
        ZR(IERR+6) = TERMSA
        ZR(IERR+7) = TERMS2
        ZR(IERR+8) = TERMS1
        ZR(IERR+9) = TERMFL
        ZR(IERR+10) = TERMF2
        ZR(IERR+11) = TERMF1
        ZR(IERR+12) = TERMEC
        ZR(IERR+13) = TERME2
        ZR(IERR+14) = TERME1
      END IF

      IF (TABNIV(1).EQ.2) THEN
        WRITE (IFM,*)
        WRITE (IFM,*) '*********************************************'
        WRITE (IFM,*) '     TOTAL SUR LA MAILLE ',ZI(IVOIS)
        WRITE (IFM,*)
        WRITE (IFM,*) 'ERREUR             ABSOLUE   /  RELATIVE '//
     &    '/ NORMALISATION'
        WRITE (IFM,400) ' TOTAL           ',ERTABS,ERTREL,'%',TERMNO
        WRITE (IFM,400) ' TERME VOLUMIQUE ',TERMVO,TERMV2,'%',TERMV1
        WRITE (IFM,400) ' TERME SAUT      ',TERMSA,TERMS2,'%',TERMS1
        WRITE (IFM,400) ' TERME FLUX      ',TERMFL,TERMF2,'%',TERMF1
        WRITE (IFM,400) ' TERME ECHANGE   ',TERMEC,TERME2,'%',TERME1
        WRITE (IFM,*) '*********************************************'
        WRITE (IFM,*)
      END IF

      CALL JEDEMA()
  400 FORMAT (A17,D12.4,1X,D12.4,A2,1X,D12.4)
      END
