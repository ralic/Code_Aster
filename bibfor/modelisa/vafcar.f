      SUBROUTINE VAFCAR(TPGZ,MCLFZ,NMOBJZ,NPO,NDI,NCO,NCA,NBA,NMA,NGR,
     +                  NGB,NUTYEL,NTYELE,CAR,NCAR,IVR,KIOC,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                  NTYELE(*),      IVR(*)
      CHARACTER*6                                  KIOC
      CHARACTER*8       TPG,     NOMOBJ,CARZ
      CHARACTER*(*)     TPGZ,    NMOBJZ,CAR(*), MCLFZ
      CHARACTER*16          MCLF
      CHARACTER*8       NOMU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/01/98   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C       VERIFICATION DE LA BONNE AFFECTATION DES DONNEES :
C         CARAC POUTRE      >  ELEMENT POUTRE
C         CARAC DISCRET     >  ELEMENT DISCRET DE TYPE L OU N
C         CARAC COQUE       >  ELEMENT COQUE
C         CARAC ORIENTATION >  ELEMENT DISCRET OU POUTRE
C         CARAC DEFI_ARC    >  ELEMENT POUTRE COURBE
C         CARAC CABLE       >  ELEMENT CABLE
C         CARAC BARRE       >  ELEMENT BARRE
C         CARAC MASSIF      >  ELEMENT THERMIQUE
C ----------------------------------------------------------------------
      CHARACTER*16  CONCEP, CMD
      CHARACTER*17  TPGE
C     ------------------------------------------------------------------
      TPG    = TPGZ
      NOMOBJ = NMOBJZ
      MCLF   = MCLFZ
C
      CALL GETRES(NOMU,CONCEP,CMD)
C
C --- VERIFICATION DE L AFFECTATION DE LA MAILLE PAR UN ELEMENT
      TPGE = TPG//' '//NOMOBJ
      IF (NUTYEL.EQ.0) THEN
         IF ( IVR(1).EQ.1 .OR. IVR(2).EQ.1 ) THEN
            CALL UTMESS('A',CMD,'OCCURENCE '//KIOC//' DE '//MCLF//':'//
     +                 ' IMPOSSIBLE D AFFECTER LES VALEURS DEMANDEES'//
     +            ' SUR LE(LA)'//TPGE//' QUI N A PAS ETE AFFECTE(E) '//
     +                                                'PAR UN ELEMENT')
            IER = IER + 1
         ENDIF
         GOTO 9999
      ENDIF
C
C --- VERIFICATION DU BON TYPE DE L ELEMENT
      IF (MCLF(1:6).EQ.'POUTRE') THEN
         NPD = 1
         NPF = NPO
      ELSEIF (MCLF(1:7).EQ.'DISCRET') THEN
         NPD = NPO + 1
         NPF = NPO + NDI
      ELSEIF (MCLF(1:11).EQ.'ORIENTATION') THEN
         NPD = 1
         NPF = NPO + NDI
      ELSEIF (MCLF(1:5).EQ.'COQUE') THEN
         NPD = NPO + NDI + 1
         NPF = NPO + NDI + NCO
      ELSEIF (MCLF(1:5) .EQ.'CABLE') THEN
         NPD = NPO + NDI + NCO + 1
         NPF = NPO + NDI + NCO + NCA
      ELSEIF (MCLF(1:5) .EQ.'BARRE') THEN
         NPD = NPO + NDI + NCO + NCA + 1
         NPF = NPO + NDI + NCO + NCA + NBA
      ELSEIF(MCLF(1:8).EQ.'DEFI_ARC') THEN
         NPD = 4
         NPF = 4
      ELSEIF(MCLF(1:6).EQ.'MASSIF') THEN
         NPD = NPO + NDI + NCO + NCA + NBA + 1
         NPF = NPO + NDI + NCO + NCA + NBA + NMA
      ELSEIF(MCLF(1:9).EQ.'ASSE_GRIL') THEN
         NPD = NPO + NDI + NCO + NCA + NBA + NMA + 1
         NPF = NPO + NDI + NCO + NCA + NBA + NMA + NGR
      ELSEIF(MCLF(1:6).EQ.'GRILLE') THEN
         NPD = NPO + NDI + NCO + NCA + NBA + NMA + NGR + 1
         NPF = NPO + NDI + NCO + NCA + NBA + NMA + NGR + NGB
      ELSE   
         CALL UTDEBM('A','VAFCAR','MOCLE FACTEUR NON TRAITE :')
         CALL UTIMPK('S',MCLF,0,' ')
         CALL UTFINM()
      ENDIF
C
      DO 10 I = NPD , NPF
         IF (NUTYEL.EQ.NTYELE(I)) GOTO 20
 10   CONTINUE
      IF ( IVR(1).EQ.1 .OR. IVR(2).EQ.1 ) THEN
         CALL UTMESS('A',CMD,'OCCURENCE '//KIOC//' DE '//MCLF//':'//
     +             ' IMPOSSIBLE D AFFECTER LES VALEURS DEMANDEES SUR'//
     +            ' LE(LA) '//TPGE//' QUI NE SUPPORTE PAS UN ELEMENT'//
     +                                                  ' DU BON TYPE')
         IER = IER + 1
      ENDIF
      GOTO 9999
 20   CONTINUE
C
C --- CAS PARTICULIER DES ELEMENTS DISCRETS
      IF (MCLF(1:7).EQ.'DISCRET') THEN
         DO 100 I = 1 , NCAR
            CARZ = CAR(I)
            IF (CARZ(3:4).EQ.'T_') THEN 
              L  = 1
              L1 = 5
            ELSEIF (CARZ(3:4).EQ.'TR') THEN 
              L  = 3
              L1 = 7
            ENDIF 
            IF (((CARZ(5:5).EQ.'N' .OR. CARZ(6:6).EQ.'N' .OR.
     +            CARZ(7:7).EQ.'N' .OR. CARZ(8:8).EQ.'N'     )
     +            .AND.        (NUTYEL.NE.NTYELE(NPO+L)              
     +                   .AND.  NUTYEL.NE.NTYELE(NPO+L1))    )
     &                         .OR.
     +          ((CARZ(5:5).EQ.'L' .OR. CARZ(6:6).EQ.'L' .OR.
     +            CARZ(7:7).EQ.'L' .OR. CARZ(8:8).EQ.'L'     )
     +            .AND.        (NUTYEL.NE.NTYELE(NPO+1+L)  
     +                   .AND.  NUTYEL.NE.NTYELE(NPO+1+L1))  )
     &                         .OR.
     +          ((CARZ(5:5).EQ.'D' .OR. CARZ(6:6).EQ.'D'     )
     +            .AND.        (NUTYEL.NE.NTYELE(NPO+L)
     +                   .AND.  NUTYEL.NE.NTYELE(NPO+L1)
     +                   .AND.  NUTYEL.NE.NTYELE(NPO+1+L) 
     +                   .AND.  NUTYEL.NE.NTYELE(NPO+1+L1))  )
     &                         .OR.
     +          ((CARZ(1:5).EQ.'M_T_D' .OR. CARZ(1:6).EQ.'M_TR_D' )
     +            .AND.        (NUTYEL.EQ.NTYELE(NPO+1+L1)
     +                   .AND.  NUTYEL.NE.NTYELE(NPO+L1)))    ) THEN
               IF ( IVR(1).EQ.1 .OR. IVR(2).EQ.1 ) THEN
               CALL UTMESS('A',CMD,'OCCURENCE '//KIOC//' DE '//MCLF//
     +               ' : LE(LA)'//TPGE//' NE SUPPORTE PAS UN ELEMENT'//
     +                  ' COMPATIBLE AVEC LA CARACTERISTIQUE '//CARZ)
                 IER = IER + 1
               ENDIF
            ENDIF
 100     CONTINUE
      ENDIF
C
 9999 CONTINUE
      END
