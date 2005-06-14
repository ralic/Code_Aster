      SUBROUTINE ARLCLR(DIME,IOC,NNO,NOMC,NOMT,EQ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C FILTRE EQUATIONS COUPLAGE ARLEQUIN REDONDANTES AVEC CONDITIONS LIMITES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER       DIME    : DIMENSION DE L'ESPACE
C INTEGER       IOC     : OCCURENCE DU MOT CLEF ARLEQUIN
C INTEGER       NNO     : NOMBRE DE NOEUDS DU MAILLAGE
C CHARACTER*10  NOMC    : NOM DE LA ZONE DE COLLAGE
C CHARACTER*10  NOMT    : NOM DE L'OBJET TANGENTES LISSEES
C
C VARIABLE DE SORTIE
C LOGICAL       EQ(5,*) : .FALSE. SI L'EQUATION CORRESPONDANTE N'EST PAS
C                         A CONSIDERER
C ----------------------------------------------------------------------

      IMPLICIT NONE

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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C --- VARIABLES
      CHARACTER*10 NOMC,NOMT
      CHARACTER*8  CLIM
      INTEGER      DIME,IOC,NNO,CMP,NCL,NL,N,NO,INO,NU(6)
      INTEGER      I,J,K,P0,P1,P2,P3,P4,P5,P6,P7,Q0
      LOGICAL      EQ(5,*)
      REAL*8       R1,R2

C --- TABLE
      CHARACTER*16 TECMP(6)
      DATA TECMP / 'D_DEPL_R_DX ','D_DEPL_R_DY ','D_DEPL_R_DZ ',
     &             'D_DEPL_R_DRX','D_DEPL_R_DRY','D_DEPL_R_DRZ' /

C --- INITIALISATION

      CALL JEMARQ()

      DO 10 I = 1, 6
        CALL JENONU(JEXNOM('&CATA.TE.NOMTE',TECMP(I)),NU(I))
 10   CONTINUE

      CALL WKVECT('&&ARLCLR.DICO','V V I',NNO,Q0)
      DO 20 I = 1, NNO
        ZI(Q0-1+I) = 0
 20   CONTINUE

      CALL JELIRA(NOMC//'.INO','LONMAX',NL,ZK8)
      CALL JEVEUO(NOMC//'.INO','L',P0)
      DO 30 I = 1, NL
        ZI(Q0-1+ZI(P0)) = I
        P0 = P0 + 1
 30   CONTINUE
        
      CALL JEEXIN(NOMT,I)
      IF (I.NE.0) CALL JEVEUO(NOMT,'L',P7)

      CALL GETVID('ARLEQUIN','COND_LIM',IOC,1,0,ZK8,NCL)

      DO 40 I = 1, -NCL

C ----- LECTURE DONNEES

        CALL GETVID('ARLEQUIN','COND_LIM',IOC,1,I,CLIM,ZI)
        CALL JELIRA(CLIM//'.CHME.LIGRE.LIEL','NMAXOC',NL,ZK8)
        CALL JEVEUO(CLIM//'.CHME.LIGRE.LIEL','L',P0)
        CALL JEVEUO(JEXATR(CLIM//'.CHME.LIGRE.LIEL','LONCUM'),'L',P1)
        CALL JEVEUO(CLIM//'.CHME.LIGRE.NEMA','L',P2)
        CALL JEVEUO(JEXATR(CLIM//'.CHME.LIGRE.NEMA','LONCUM'),'L',P3)

C ----- PARCOURS DES LIELS ASSOCIEES AUX CONDITIONS LIMITES

        P5 = ZI(P1)

        DO 40 J = 1, NL

          P4 = P5
          P5 = ZI(P1+J)
          P6 = P0-1+P4

C ------- COMPOSANTE

          K = ZI(P0-2+P5)

          CMP = 0
 50       CONTINUE
          IF (CMP.EQ.6) GOTO 40
          CMP = CMP + 1
          IF (NU(CMP).NE.K) GOTO 50

C ------- PARCOURS DES NOEUDS

          N = P5 - P4 - 1

          DO 60 K = 1, N

            INO = ZI(P2-1+ZI(P3-1-ZI(P6)))
            NO = ZI(Q0-1+INO)
            P6 = P6 + 1
            
            IF (NO.EQ.0) GOTO 60
 
C --------- CAS DES TRANSLATIONS

            IF (CMP.LE.DIME) THEN

              EQ(CMP,NO) = .FALSE.

C --------- CAS DES ROTATIONS 2D

            ELSEIF (DIME.EQ.2) THEN

              EQ(3,NO) = .FALSE.

C --------- CAS DES ROTATIONS 3D

            ELSE

              R1 = ABS(ZR(P7+6*INO+CMP-7))
              R2 = ABS(ZR(P7+6*INO+CMP-10))

              IF (R1.GT.R2) THEN
                EQ(4,NO) = .FALSE.
              ELSE
                EQ(5,NO) = .FALSE.
              ENDIF

            ENDIF

 60       CONTINUE
 
 40   CONTINUE

C --- DESALLOCATION

      CALL JEDETR('&&ARLCLR.DICO')
      CALL JEDEMA()

      END
