      SUBROUTINE I3FMVN(NIL,DESC,SUCC,PREC,DESCTM,PTMDEP,
     +                  CONNEC,VLC,
     +                  LND,NBND,NBCHER,NBTROU,MATROU)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER NIL,DESC(*),SUCC(*),PREC(*),DESCTM(*),PTMDEP
      INTEGER LND(*),NBND,NBCHER,NBTROU,MATROU(*),CONNEC(*),VLC(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     RECHERCHE DES MAILLES D' UN OBJ L_MAILLE QUI ADMETTENT POUR NOEUD
C     LES NOEUDS DE LA LISTE LND SACHANT QUE LA MAILLE POINTEE PAR
C     PTMDEP EST UNE DE CES MAILLES
C     ------------------------------------------------------------------
C IN  NIL    : I :--
C IN  DESC   : I :  !
C IN  SUCC   : I :  !
C IN  PREC   : I :--
C IN  DESCTM : I : POINTEUR SUR LES DESCRIPTEUR DE TYPE DES MAILLES
C            :   : OBJ DE TYPE POINTEUR SUR DESC_TYPE_MAIL
C IN  PTMDEP : I : POINTEUR SUR LA MAILLE CONNUE
C IN  CONNEC : I : CONNECTIVITE TOTALE DU MAILLAGE
C IN  VLC    : I : POINTEUR SUR LES MAILLES DANS CONNEC
C IN  LND    : I : LISTE DES NOEUDS DE SELECTION
C IN  NBND   : I : TAILLE DE LA LISTE DES NOEUDS DE SELECTION
C IN  NBCHER : I : NOMBRE DE MAILLES CHERCHEES (-1 SI TOUTES)
C IN  NBTROU : I : NOMBRE DE MAILLES TROUVEES
C OUT MATROU : I : MAILLES TROUVEES (Y COMPRIS CELLE DEJA CONNUE)
C     ------------------------------------------------------------------
C     LA ROUTINE DE CALCUL D' INTERSECTION VIDE L' OBJ L_MAILLE PAR LA
C     TETE
C     ON COMMENCE LA RECHERCHE DANS LES SUCCESSEURS DE PTMDEP
C     LA RECHERCHE DANS LE SENS DES PREDECESSEURS EST NEANMOINS CODEE
C     ------------------------------------------------------------------
C
C
      INTEGER PTM,NUM,PTT,TM,ADR,NBS,IRET
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      MATROU(1) = PTMDEP
      PTT       = 2
      NBTROU    = 1
      PTM       = SUCC(PTMDEP)
100   CONTINUE
      IF ( (PTM .NE. NIL) .AND. (NBTROU .NE. NBCHER) ) THEN
         NUM = PTM
         TM  = DESC(PTM)
         NBS = ZI(DESCTM(TM) + 2-1)
         ADR = VLC(NUM)
         CALL I3INEI(LND,CONNEC(ADR),NBND,NBS,IRET)
         NBTROU = NBTROU + IRET
         PTM    = SUCC(PTM)
         IF ( IRET .EQ. 1 ) THEN
            MATROU(PTT) = NUM
            PTT         = PTT + 1
         ENDIF
         GOTO 100
      ENDIF
      PTM = PREC(PTMDEP)
200   CONTINUE
      IF ( (PTM .NE. NIL) .AND. (NBTROU .NE. NBCHER) ) THEN
         NUM = PTM
         TM  = DESC(PTM)
         NBS = ZI(DESCTM(TM) + 2-1)
         ADR = VLC(NUM)
         CALL I3INEI(LND,CONNEC(ADR),NBND,NBS,IRET)
         NBTROU = NBTROU + IRET
         PTM    = PREC(PTM)
         IF ( IRET .EQ. 1 ) THEN
            MATROU(PTT) = NUM
            PTT         = PTT + 1
         ENDIF
         GOTO 200
      ENDIF
      END
