      SUBROUTINE LISNOR(CARA,DIME,NNORMZ,NTANGZ,NEPAIZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C         LISSAGE DES NORMALES D'UN GROUPE D'ELEMENTS COQUES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8   CARA       :  SD CARA_ELEM
C INTEGER       DIME       :  DIMENSION DE L'ESPACE
C CHARACTER*8   NTM(*)     :  VECTEUR NOMS DES TYPES DE MAILLE
C CHARACTER*(*) NNORMZ     :  NOM DE L'OBJET NORMALES LISSEES
C CHARACTER*(*) NTANGZ     :  NOM DE L'OBJET TANGENTES LISSEES
C CHARACTER*(*) NEPAIZ     :  NOM DE L'OBJET EPAISSEUR
C
C SD DE SORTIE
C NNORM : NOEUD DU MAILLAGE -> NORMALE NORMEE * EPAISSEUR MOYENNE
C          ( NX1, NY1, [NZ1], NX2, ...)
C NTANG : NOEUD DU MAILLAGE -> TANGENTE[S] NORMEE[S]
C          ( TX1.1, TY1.1, [TZ1.1, TX1.2, TY1.2, TZ1.2], TX2.1, ...)
C NEPAI : MAILLAGE DU MAILLAGE -> EPAISSEUR DE LA MAILLE
C          ( EP1, EP2, ... )
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

C --- FONCTIONS
      REAL*8        R8NRM2

C --- VARIABLES
      CHARACTER*8   CARA,MAIL,K
      CHARACTER*24  NNORM,NTANG,NEPAI
      CHARACTER*(*) NNORMZ,NTANGZ,NEPAIZ
      REAL*8        R,EP,NO(18),TANG(54),W(3)
      INTEGER       NNO,NMA,NZONE,NCMP,DIME,IZONE,IMA,INO,NN,DD
      INTEGER       I,J,P0,P1,P2,P3,P4,P5,P6,P7,Q0,Q1,Q2,Q3,Q4,Q5
      LOGICAL       LTOUT

C --- LECTURE DES DONNEES
  
      CALL JEEXIN(CARA//'.CARCOQUE  .DESC',I)
      IF (I.EQ.0) CALL UTMESS('F','LISNOR','MANQUE .CARCOQUE')

      NNORM = NNORMZ
      NTANG = NTANGZ
      NEPAI = NEPAIZ

      CALL JEMARQ()

      CALL JEVEUO(CARA//'.CARCOQUE  .NOMA','L',P0)
      MAIL = ZK8(P0)
      CALL JEVEUO(MAIL//'.DIME','L',P0)
      NNO = ZI(P0)
      NMA = ZI(P0+2)
      CALL JEVEUO(CARA//'.CARCOQUE  .DESC','L',P0)
      CALL JEVEUO(CARA//'.CARCOQUE  .VALE','L',P1)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',ZI(P0)),'LONMAX',NCMP,K)
      CALL JEVEUO(MAIL//'.CONNEX','L',P2)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',P3)
      CALL JEVEUO(MAIL//'.COORDO    .VALE','L',P4)

C --- ALLOCATIONS

      DD = (DIME-1)*DIME

      CALL WKVECT(NNORM,'V V R',DIME*NNO,Q0)
      CALL WKVECT(NTANG,'V V R',DD*NNO,Q1)
      CALL WKVECT(NEPAI,'V V R',NMA,Q2)
      CALL WKVECT('&&LISNOR.COMPTEUR','V V I',NNO,Q3)

      DO 10 I = 1, DIME*NNO
        ZR(Q0-1+I) = 0.D0
 10   CONTINUE

      DO 20 I = 1, (DIME-1)*DIME*NNO
        ZR(Q1-1+I) = 0.D0
 20   CONTINUE

      DO 30 I = 1, NNO
        ZI(Q3-1+I) = 0
 30   CONTINUE

C --- CALCUL DES NORMALES LISSEES

      NZONE = ZI(P0+2)
      LTOUT = .FALSE.

      DO 40 IZONE = 1, NZONE

        EP = 0.5D0*ZR(P1)
        P1 = P1 + NCMP

        P5 = P0+1+2*IZONE
        NN = ZI(P5)
        
        IF (NN.EQ.1) THEN

          LTOUT = .TRUE.
          GOTO 50

        ENDIF

        J = ZI(P5+1)

        IF (NN.EQ.2) THEN

          CALL JEVEUO(JEXNUM(MAIL//'.GROUPEMA',J),'L',P5)
          CALL JELIRA(JEXNUM(MAIL//'.GROUPEMA',J),'LONMAX',NMA,K)

        ELSEIF (NN.EQ.3) THEN

          CALL JEVEUO(JEXNUM(CARA//'.CARCOQUE  .LIEL',J),'L',P5)
          CALL JELIRA(JEXNUM(CARA//'.CARCOQUE  .LIEL',J),'LONMAX',NMA,K)

        ELSE
           
          GOTO 40

        ENDIF

 50     CONTINUE

        DO 60 I = 1, NMA

          IF (LTOUT) THEN 
            IMA = I
          ELSE
            IMA = ZI(P5-1+I)
          ENDIF

          ZR(Q2-1+IMA) = EP

          CALL CONOEU(IMA,ZI(P2),ZI(P3),ZR(P4),R,DIME,0,NO,NN)
          CALL TANGNT(NO,NN,DIME,0,0,TANG)
          P6 = P2-1+ZI(P3-1+IMA)
          P7 = 1
          
          DO 60 J = 1, NN
         
            INO = ZI(P6)
            P6 = P6 + 1
            ZI(Q3-1+INO) = ZI(Q3-1+INO) + 1
            
            IF (DIME.EQ.2) THEN

              Q4 = Q0 + 2*(INO-1)

              R = EP/R8NRM2(2,TANG(P7),1)
              
              ZR(Q4  ) = ZR(Q4  ) - TANG(P7+1)*R
              ZR(Q4+1) = ZR(Q4+1) + TANG(P7  )*R

              P7 = P7 + 2

            ELSE
 
              Q4 = Q0 + 3*(INO-1)
              Q5 = Q1 + 6*(INO-1)

              W(1) = TANG(P7+1)*TANG(P7+5)-TANG(P7+2)*TANG(P7+4)
              W(2) = TANG(P7+2)*TANG(P7+3)-TANG(P7  )*TANG(P7+5)
              W(3) = TANG(P7  )*TANG(P7+4)-TANG(P7+1)*TANG(P7+3)

              R = EP/R8NRM2(3,W,1)
              CALL R8AXPY(3,R,W,1,ZR(Q4),1)
              CALL R8COPY(3,TANG(P7),1,ZR(Q5),1)

              P7 = P7 + 6

            ENDIF 

 60     CONTINUE
                
 40   CONTINUE

C --- MOYENNE

      DO 70 I = 1, NNO

        NN = ZI(Q3)

        IF (NN.NE.0) THEN
          
          IF (DIME.EQ.2) THEN

            R = R8NRM2(2,ZR(Q0),1)
            ZR(Q1  ) = ZR(Q0+1)/R
            ZR(Q1+1) =-ZR(Q0  )/R

          ELSE

            ZR(Q1+3) = ZR(Q0+1)*ZR(Q1+2)-ZR(Q0+2)*ZR(Q1+1)
            ZR(Q1+4) = ZR(Q0+2)*ZR(Q1  )-ZR(Q0  )*ZR(Q1+2)
            ZR(Q1+5) = ZR(Q0  )*ZR(Q1+1)-ZR(Q0+1)*ZR(Q1  )

            R = R8NRM2(3,ZR(Q1+3),1)
            IF (R.EQ.0.D0) CALL UTMESS('F','LISNOR','NORMALE MOYENNE '//
     &                 'NULLE : ATTENTION A L''ORIENTATION DES MAILLES')

            R = 1.D0/R
            CALL R8SCAL(3,R,ZR(Q1+3),1)

            ZR(Q1  ) = ZR(Q1+4)*ZR(Q0+2)-ZR(Q1+5)*ZR(Q0+1)
            ZR(Q1+1) = ZR(Q1+5)*ZR(Q0  )-ZR(Q1+3)*ZR(Q0+2)
            ZR(Q1+2) = ZR(Q1+3)*ZR(Q0+1)-ZR(Q1+4)*ZR(Q0  )
 
            R = 1.D0/R8NRM2(3,ZR(Q1),1)
            CALL R8SCAL(3,R,ZR(Q1),1)

          ENDIF

          R = 1.D0/NN
          CALL R8SCAL(DIME,R,ZR(Q0),1)

        ENDIF

        Q0 = Q0 + DIME
        Q1 = Q1 + DD
        Q3 = Q3 + 1

 70   CONTINUE

C --- DESALLOCATIONS

      CALL JEDETR('&&LISNOR.COMPTEUR') 
      CALL JEDEMA()

      END
