      SUBROUTINE ARLCLR(MOTCLE,IOCC  ,MAIL  ,NOMARL,DIME  ,
     &                  NOMC  ,TANG  )
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*16 MOTCLE
      INTEGER      IOCC
      CHARACTER*8  NOMARL,MAIL  
      INTEGER      DIME
      INTEGER      NNO    
      CHARACTER*10 NOMC,TANG
C
C ATTENTION !!! ROUTINE DECONNECTEE, IL FAUT CORRIGER LES 'EQ' EN
C COMMENTAIRE
C

C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C FILTRE EQUATIONS COUPLAGE ARLEQUIN REDONDANTES AVEC CONDITIONS LIMITES
C
C
C ----------------------------------------------------------------------
C
C
C IN  MAIL   : NOM UTISATEUR DU MAILLAGE
C IN  MOTCLE : MOT-CLEF FACTEUR POUR ARLEQUIN
C IN  IOCC   : OCCURRENCE DU MOT CLEF-FACTEUR ARLEQUIN
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NOMC   : NOM DE LA ZONE DE COLLAGE
C IN  TANG   : NOM DE L'OBJET TANGENTES LISSEES
C I/O NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C
C SD PRODUITE: NOMARL(1:8)//'.EXCLU'
C  VECTEUR DE LOGICAL INDIQUANT SI UN NOEUD COLLE EST A EXCLURE
C  DES RELATIONS POUR ARLEQUIN
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNOM,JEXATR
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
C      
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*8  CLIM,K8BID
      INTEGER      CMP,NCL,NL,N,NO,INO,IRET,JARLEQ,IBID,JDIM
      INTEGER      I,J,K,P0,P1,P2,P3,P4,P5,P6,P7,Q0,NC
      REAL*8       R1,R2
      INTEGER      IFM,NIV   
      INTEGER      NTECMP
      PARAMETER    (NTECMP=6)    
      CHARACTER*16 TECMP(NTECMP)
      INTEGER       NU(NTECMP)
C            
      DATA TECMP / 'D_DEPL_R_DX ','D_DEPL_R_DY ','D_DEPL_R_DZ ',
     &             'D_DEPL_R_DRX','D_DEPL_R_DRY','D_DEPL_R_DRZ' /    
C
C ----------------------------------------------------------------------
C 
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)      
C
C --- CREATION DE L'OBJET NOMARL(1:8)//'.EXCLU'
C
      CALL JELIRA(NOMC(1:10)//'.INO','LONMAX',NC,K8BID)
      CALL JEDETR(NOMARL(1:8)//'.EXCLU')
      CALL WKVECT(NOMARL(1:8)//'.EXCLU', 'V V L', 5*NC, JARLEQ )
C
      DO 90 I = 1, 5*NC
        ZL(JARLEQ-1+I) = .TRUE.
 90   CONTINUE
C
      CALL JEVEUO(MAIL(1:8)//'.DIME','L',JDIM)
      NNO  = ZI(JDIM)
C
C --- NUMERO DES DDLS
C
      DO 10 I = 1, NTECMP
        CALL JENONU(JEXNOM('&CATA.TE.NOMTE',TECMP(I)),NU(I))
 10   CONTINUE
C
C --- CREATION OBJET TEMPORAIRE
C
      CALL WKVECT('&&ARLCLR.DICO','V V I',NNO,Q0)
C
C --- LECTURE DONNEES COLLAGE
C  
      CALL JELIRA(NOMC(1:10)//'.INO','LONMAX',NL,K8BID)
      CALL JEVEUO(NOMC(1:10)//'.INO','L',P0)
      DO 30 I = 1, NL
        ZI(Q0-1+ZI(P0)) = I
        P0 = P0 + 1
 30   CONTINUE
C
C --- LECTURE DONNEES NORMALES
C        
      CALL JEEXIN(TANG,IRET)
      IF (IRET.NE.0) THEN
        CALL JEVEUO(TANG,'L',P7)
      ENDIF  
C
      CALL GETVID(MOTCLE,'COND_LIM',IOCC,1,0,K8BID,NCL)
      
      IF (NCL.NE.0) THEN
        CALL U2MESS('F','ARLEQUIN_14')
      ENDIF

      DO 40 I = 1, -NCL

C ----- LECTURE DONNEES

        CALL GETVID(MOTCLE,'COND_LIM',IOCC,1,I,CLIM,IBID)
        CALL JELIRA(CLIM//'.CHME.LIGRE.LIEL','NMAXOC',NL,K8BID)
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

C              EQ(CMP,NO) = .FALSE.

C --------- CAS DES ROTATIONS 2D

            ELSEIF (DIME.EQ.2) THEN

C              EQ(3,NO) = .FALSE.

C --------- CAS DES ROTATIONS 3D

            ELSE

              R1 = ABS(ZR(P7+6*INO+CMP-7))
              R2 = ABS(ZR(P7+6*INO+CMP-10))

              IF (R1.GT.R2) THEN
C                EQ(4,NO) = .FALSE.
              ELSE
C                EQ(5,NO) = .FALSE.
              ENDIF

            ENDIF

 60       CONTINUE
 
 40   CONTINUE
C
C --- DESALLOCATIONS
C
      CALL JEDETR('&&ARLCLR.DICO')
      CALL JEDEMA()

      END
