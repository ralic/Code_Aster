      SUBROUTINE   XPRFON(NOMA,FISS,NUMFON,NVIT,NBETA)

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8    FISS,NOMA

      CHARACTER*24   NVIT,NBETA
      INTEGER        NUMFON

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR JAUBERT A.JAUBERT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE COLOMBO D.COLOMBO

C       XPRFON   : X-FEM PROPAGATION :RENUMEROTATION DU FRONT DE FISSURE
C       ------     -     --                                  ---
C    RENUMEROTATION DU FRONT DE FISSURE DANS LE CAS DE L'UTILISATION DE
C    LA METHODE UPWIND AVEC PLUSIEURS FOND DE FISSURE     
C
C    ENTREE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISS    : NOM DU CONCEPT FISSURE X-FEM
C                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
C        NVIT    : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C        NBETA   : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C        NUMFON  : NOMBRE DE FONDS DE FISSURE
C
C     ------------------------------------------------------------------
      INTEGER        I,J,K,IRET,IFM,NIV,NFON,NBNOL
      INTEGER        JBASEF,JFMULT,JFONF,JBETA,JVIT,JMEMO
      INTEGER        JBASO,JFMULO,JFONO,JBETAO,JVITO,JFONG
      INTEGER        LONG,NPTFG,NBPTFF,IVALUE,NVAL
      INTEGER        NPOIN,NPOINP,NPOINO,NPONOP
      REAL*8         A1(4),B1(4),M1(3),A2(4),B2(4),M2(3)
      REAL*8         PROSCA,NORMAB,COEFFK,MEM(5),MEMO(5)
      REAL*8         VECT1,VECT2,R8PREM
      CHARACTER*8    K8B,K8BID
C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      CALL VECINI(4,0.D0,MEM)

C     RECUPERATION DES CARACTERISTIQUES DU FOND SUR LA GRILLE
C     NPTFG : NBRE DE POINTS DU FOND SUR LA GRILLE
C     (UTILE POUR REORIENTER LES FONDS SUR LE MAILLAGE REEL, cf DOC???)
      CALL JEVEUO(FISS//'.FONDFISG','L',JFONG)
      CALL JELIRA(FISS//'.FONDFISG','LONMAX',LONG,K8BID)
      NPTFG=LONG/4

C     RECUPERATION DU FOND DE FISSURE
      CALL JEVEUO(FISS//'.FONDFISS','E',JFONF)
      CALL DISMOI('F','NB_POINT_FOND',FISS,'FISS_XFEM',NBPTFF,K8B,IRET)

C     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
      CALL JEVEUO(FISS//'.FONDMULT','E',JFMULT)
      CALL DISMOI('F','NB_FOND',FISS,'FISS_XFEM',NUMFON,K8B,IRET)

C     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
      CALL JEVEUO(FISS//'.BASEFOND','E',JBASEF)

C     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE ON
C     THE FRONT
      CALL JEVEUO(NVIT,'E',JVIT)
      CALL JEVEUO(NBETA,'E',JBETA)

C     CREATION DE VECTEUR DE PASSAGE
      CALL WKVECT('&&XPRFON.MEMO','V V R8',
     &       5*NUMFON,JMEMO)
      CALL WKVECT('&&XPRFON.JFONO','V V R8',
     &       4*NBPTFF,JFONO)
      CALL WKVECT('&&XPRFON.JFMULO','V V I',
     &       2*NUMFON,JFMULO)
      CALL WKVECT('&&XPRFON.JBASO','V V R8',
     &       6*NBPTFF,JBASO)
      CALL WKVECT('&&XPRFON.JVITO','V V R8',
     &       NBPTFF,JVITO)
      CALL WKVECT('&&XPRFON.JBETAO','V V R8',
     &       NBPTFF,JBETAO)
      DO 333  J=1,NBPTFF
        DO 334 I=1,4
          ZR(JFONO-1+4*(J-1)+I)=ZR(JFONF-1+4*(J-1)+I)
 334    CONTINUE
        DO 335 I=1,6
          ZR(JBASO-1+6*(J-1)+I)=ZR(JBASEF-1+6*(J-1)+I)
 335    CONTINUE
        ZR(JVITO-1+(J-1)+1)=ZR(JVIT-1+(J-1)+1)
        ZR(JBETAO-1+(J-1)+1)=ZR(JBETA-1+(J-1)+1)
 333  CONTINUE
      DO 336  I=1,NUMFON
        ZI(JFMULO-1+2*(I-1)+1)=ZI(JFMULT-1+2*(I-1)+1)
        ZI(JFMULO-1+2*(I-1)+2)=ZI(JFMULT-1+2*(I-1)+2)
 336  CONTINUE
      MEM(1)=1.D0/R8PREM()
C
C VERIFICATION DU BON ORDONNANCEMENT DES DIFFERENTS FONDS	   
      DO 700  I=1,NUMFON
        DO 505 J=1,NPTFG-1
          DO 222 K=1,4
            A1(K)=ZR(JFONG-1+4*(J-1)+K)
            B1(K)=ZR(JFONG-1+4*(J+1-1)+K)
 222      CONTINUE
          NORMAB=(B1(1)-A1(1))**2+(B1(2)-A1(2))**2+
     &           (B1(3)-A1(3))**2
C   ON EXTRAIT LES COORDONNEES DU PREMIER POINT DU FOND DE FISSURE
          IF (I.EQ.1) THEN
            NPOIN=ZI(JFMULO-1)
            M1(1)=ZR(JFONF-1+4*(NPOIN)+1)
            M1(2)=ZR(JFONF-1+4*(NPOIN)+2)
            M1(3)=ZR(JFONF-1+4*(NPOIN)+3)
          ELSE
            NPOIN=ZI(JFMULO-1+2*(I-1))
            M1(1)=ZR(JFONF-1+4*(NPOIN)+1)
            M1(2)=ZR(JFONF-1+4*(NPOIN)+2)
            M1(3)=ZR(JFONF-1+4*(NPOIN)+3)
          ENDIF
            COEFFK=((B1(1)-A1(1))*(M1(1)-A1(1))+(B1(2)-A1(2))*
     &         (M1(2)-A1(2))+(B1(3)-A1(3))*(M1(3)-A1(3)))/NORMAB

          IF (ABS(COEFFK).GT.1.D0) THEN
            IF (ABS(COEFFK).LT.MEM(1)) THEN
              MEM(1)=ABS(COEFFK)
              DO 328 K=1,4
                MEM(K+1)=A1(K)
 328          CONTINUE
            ENDIF
            GOTO 505
          ELSE
            CALL VECINI(4,0.D0,MEM)
            MEM(1)=1.D0/R8PREM()
            GOTO 66
          ENDIF
 505    CONTINUE
        DO 329 K=1,4
          A1(K)=MEM(K+1)
 329    CONTINUE
        CALL VECINI(4,0.D0,MEM)
        MEM(1)=1.D0/R8PREM()
 66     CONTINUE
        DO 506 J=1,NPTFG-1
          DO 223 K=1,4
            A2(K)=ZR(JFONG-1+4*(J-1)+K)
            B2(K)=ZR(JFONG-1+4*(J+1-1)+K)
 223      CONTINUE
          NORMAB=(B1(1)-A1(1))**2+(B1(2)-A1(2))**2+
     &           (B1(3)-A1(3))**2
C  ON EXTRAIT LES COORDONNEES DU DERNIER POINT DU FOND DE FISSURE
          IF (I.EQ.1) THEN
            NPOIN=ZI(JFMULO+1)
            M2(1)=ZR(JFONF-1+4*(NPOIN-1)+1)
            M2(2)=ZR(JFONF-1+4*(NPOIN-1)+2)
            M2(3)=ZR(JFONF-1+4*(NPOIN-1)+3)
          ELSEIF (I.EQ.NUMFON) THEN
                M2(1)=ZR(JFONF-1+4*(NBPTFF-1)+1)
                M2(2)=ZR(JFONF-1+4*(NBPTFF-1)+2)
                M2(3)=ZR(JFONF-1+4*(NBPTFF-1)+3)
          ELSE
            NPOIN=ZI(JFMULO-1+2*I)
            M2(1)=ZR(JFONF-1+4*(NPOIN-1)+1)
            M2(2)=ZR(JFONF-1+4*(NPOIN-1)+2)
            M2(3)=ZR(JFONF-1+4*(NPOIN-1)+3)
          ENDIF
          COEFFK=((B2(1)-A2(1))*(M2(1)-A2(1))+(B2(2)-A2(2))*
     &         (M2(2)-A2(2))+(B2(3)-A2(3))*(M2(3)-A2(3)))/NORMAB

          IF (ABS(COEFFK).GT.1.D0) THEN
            IF (ABS(COEFFK).LT.MEM(1)) THEN
              MEM(1)=ABS(COEFFK)
              DO 326 K=1,4
                MEM(K+1)=B2(K)
 326          CONTINUE
            ENDIF
            GOTO 506
          ELSE
            GOTO 67
          ENDIF
 506    CONTINUE

        DO 327 K=1,4
          B2(K)=MEM(K+1)
 327    CONTINUE
 67     CONTINUE
        CALL VECINI(4,0.D0,MEM)
        MEM(1)=1.D0/R8PREM()
        ZR(JMEMO-1+5*(I-1)+1)= I
        ZR(JMEMO-1+5*(I-1)+2)= A1(4)
        ZR(JMEMO-1+5*(I-1)+3)= B2(4)
        ZR(JMEMO-1+5*(I-1)+4)= DBLE(ZI(JFMULO-1+(2*I-1)))
        ZR(JMEMO-1+5*(I-1)+5)= DBLE(ZI(JFMULO-1+(2*I)))
 700  CONTINUE

C ON TRIE LE VECTEUR JMEMO 
C (DE L'ABSCISSE CURV. LA PLUS PETITE A LA PLUS GRANDE)
      DO 655 I=1,NUMFON-1
        DO 755 J=I+1,NUMFON
          IF (ZR(JMEMO-1+5*(J-1)+2).LT.ZR(JMEMO-1+5*(I-1)+2)) THEN
            DO 855 K=1,5
              MEMO(K) = ZR(JMEMO-1+5*(I-1)+K)
 855        CONTINUE
            DO 955 K=1,5
              ZR(JMEMO-1+5*(I-1)+K) = ZR(JMEMO-1+5*(J-1)+K)
 955        CONTINUE
            DO 956 K=1,5
              ZR(JMEMO-1+5*(J-1)+K) = MEMO(K)
 956        CONTINUE
          ENDIF
 755    CONTINUE
 655  CONTINUE

      DO 708  I=1,NUMFON
        ZI(JFMULO-1+2*(I-1)+1)=NINT(ZR(JMEMO-1+5*(I-1)+4))
        ZI(JFMULO-1+2*(I-1)+2)=NINT(ZR(JMEMO-1+5*(I-1)+5))
 708  CONTINUE

      IVALUE=0

      DO 709  I=1,NUMFON
        NPOINO=ZI(JFMULO-1+2*I)
        NPONOP=ZI(JFMULO-1+2*I-1)

        IF (ZI(JFMULT-1+2*(I-1)+2).NE.
     &         ZI(JFMULO-1+2*(I-1)+2)) THEN
          ZI(JFMULO-1+2*(I-1)+1)=IVALUE+1
          ZI(JFMULO-1+2*(I-1)+2)=IVALUE+1+(NPOINO-NPONOP)
          NPOINP=ZI(JFMULT-1+2*(NINT(ZR(JMEMO-1+5*(I-1)+1)))-1)
          NVAL=ZI(JFMULO-1+2*(I-1)+1)
          DO 710 J=1,(NPOINO-NPONOP)+1
            DO 711 K=1,4
              ZR(JFONO-1+4*(NVAL+J-2)+K)=
     &        ZR(JFONF-1+4*(NPOINP+J-2)+K)
 711        CONTINUE
            DO 712 K=1,6
              ZR(JBASO-1+6*(NVAL+J-2)+K)=
     &        ZR(JBASEF-1+6*(NPOINP+J-2)+K)
 712        CONTINUE
            ZR(JVITO-1+(NVAL+J-2)+1)=ZR(JVIT-1+(NPOINP+J-2)+1)
            ZR(JBETAO-1+(NVAL+J-2)+1)=ZR(JBETA-1+(NPOINP+J-2)+1)
 710      CONTINUE
        ENDIF
        NPOINO=ZI(JFMULO-1+2*I)
        IVALUE=NPOINO
 709  CONTINUE

      DO 136  I=1,NUMFON
        ZI(JFMULT-1+2*(I-1)+1)=ZI(JFMULO-1+2*(I-1)+1)
        ZI(JFMULT-1+2*(I-1)+2)=ZI(JFMULO-1+2*(I-1)+2)
 136  CONTINUE
      DO 133  J=1,NBPTFF
        DO 134 I=1,4
          ZR(JFONF-1+4*(J-1)+I)=ZR(JFONO-1+4*(J-1)+I)
 134    CONTINUE
        DO 135 I=1,6
          ZR(JBASEF-1+6*(J-1)+I)=ZR(JBASO-1+6*(J-1)+I)
 135    CONTINUE
        ZR(JVIT-1+(J-1)+1)=ZR(JVITO-1+(J-1)+1)
        ZR(JBETA-1+(J-1)+1)=ZR(JBETAO-1+(J-1)+1)
 133  CONTINUE
C 
C     ON VERIFIE QUE LA LECTURE DES POINTS DES FRONTS SE FAIT DANS
C     LE MEME SENS. POUR CELA ON VERIFIE LA COHERENCE AVEC LE SENS
C     DE PARCOURS DU FRONT DE FISSURE SUR LA GRILLE
C
      VECT1=ZR(JMEMO-1+3)-ZR(JMEMO-1+2)

      DO 177 I=2,NUMFON
        VECT2=ZR(JMEMO-1+5*(I-1)+3)-ZR(JMEMO-1+5*(I-1)+2)
        PROSCA=VECT1*VECT2

        IF (PROSCA.LT.0.D0) THEN
C     ON DOIT CHANGER LE SENS DE LECTURE
          NBNOL = ZI(JFMULT+2*I-1)-ZI(JFMULT+2*I-2)
          NPOIN=ZI(JFMULT-1+2*(I-1))
          DO 802 J=1,NBNOL+1
            DO 804 K=1,4
              ZR(JFONO-1+4*(J-1)+K) =
     &        ZR(JFONF-1+4*(NPOIN+J-1)+K)
804         CONTINUE
            DO 806 K=1,6
              ZR(JBASO-1+6*(J-1)+K) =
     &        ZR(JBASEF-1+6*(NPOIN+J-1)+K)
806         CONTINUE
            ZR(JVITO-1+(J-1)+1)=ZR(JVIT-1+(NPOIN+J-1)+1)
            ZR(JBETAO-1+(J-1)+1)=ZR(JBETA-1+(NPOIN+J-1)+1)
802       CONTINUE

          DO 803 J=1,NBNOL+1
            DO 805 K=1,4
              ZR(JFONF-1+4*(NPOIN+J-1)+K)=
     &        ZR(JFONO-1+4*(NBNOL-J+1)+K)
805         CONTINUE
            DO 807 K=1,6
              ZR(JBASEF-1+6*(NPOIN+J-1)+K)=
     &        ZR(JBASO-1+6*(NBNOL-J+1)+K)
807         CONTINUE
            ZR(JVIT-1+(NPOIN+J-1)+1)=ZR(JVITO-1+(NBNOL-J+1)+1)
            ZR(JBETA-1+(NPOIN-J-1)+1)=ZR(JBETAO-1+(NBNOL-J+1)+1)
803       CONTINUE
        ENDIF
177   CONTINUE

      CALL JEDETR('&&XPRFON.MEMO')
      CALL JEDETR('&&XPRFON.JFONO')
      CALL JEDETR('&&XPRFON.JFMULO')
      CALL JEDETR('&&XPRFON.JBASO')
      CALL JEDETR('&&XPRFON.JVITO')
      CALL JEDETR('&&XPRFON.JBETAO')
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
