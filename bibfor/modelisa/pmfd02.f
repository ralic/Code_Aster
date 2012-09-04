      SUBROUTINE PMFD02(NOMA,CESDEC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     COMMANDE AFFE_CARA_ELEM
C     TRAITEMENT DES MOTS CLES :
C           COQUE  /COQUE_NCOU
C           GRILLE /GRILLE_NCOU
C           POUTRE /TUYAU_NCOU
C           POUTRE /TUYAU_NSEC
C       CONSTRUCTION DU CHAM_ELEM_S DE NBSP_I (CESDEC)
C          IMA ->  COQ_NCOU   GRI_NCOU   TUY_NCOU   TUY_NSEC

C ----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8 NOMA
      CHARACTER*19 CESDEC

      INTEGER NBOCC,IOCC,IRET
      INTEGER NBMA,NBCOU,NBV,NBSEC
      INTEGER NBAP,K,I,JNCMP,JVALV,JMA
      CHARACTER*19 CARTE
      CHARACTER*16 MOCLES(2),TYPMCL(2),MOCLEF(4)
      CHARACTER*8 K8B
      CHARACTER*24 MESMAI
      INTEGER      IARG


      DATA MOCLES/'MAILLE','GROUP_MA'/
      DATA TYPMCL/'MAILLE','GROUP_MA'/
      DATA MOCLEF/'COQUE','GRILLE','POUTRE','MEMBRANE'/
C     ------------------------------------------------------------------
      CALL JEMARQ()


      MESMAI = '&&PMFD02.MES_MAILLES'
      NBAP = 0
      DO 70 I = 1 , 4
         CALL GETFAC ( MOCLEF(I), NBOCC )
         NBAP = NBAP + NBOCC
         DO 72 K = 1 , NBOCC
            CALL RELIEM(' ', NOMA, 'NU_MAILLE', MOCLEF(I), K, 2,
     +                                   MOCLES, TYPMCL, MESMAI, NBMA )
            IF ( NBMA .NE. 0 )  CALL JEDETR ( MESMAI )
 72      CONTINUE
 70   CONTINUE

      IF (NBAP.EQ.0) THEN
        CALL CESCRE('V',CESDEC,'ELEM',NOMA,'NBSP_I',1,'COQ_NCOU',-1,
     &                  -1,-1)
        GO TO 9999
      END IF


      CARTE='&&PMFD02.NBSP_I'
      CALL ALCART ( 'V', CARTE, NOMA, 'NBSP_I' )

      CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )




C     1- MOT CLE "COQUE" :
C     -----------------------------------------------------------
      CALL GETFAC ( 'COQUE', NBOCC )
      DO 10 IOCC = 1,NBOCC
        CALL RELIEM(' ',NOMA,'NU_MAILLE','COQUE',IOCC,2,MOCLES,
     &              TYPMCL,MESMAI ,NBMA)

        CALL GETVIS('COQUE','COQUE_NCOU',IOCC,IARG,1,NBCOU,NBV)
        ZK8(JNCMP-1+1) = 'COQ_NCOU'
        ZI(JVALV-1+1) =  NBCOU

        CALL JEVEUO (MESMAI , 'L', JMA )
        CALL NOCART ( CARTE, 3, K8B, 'NUM', NBMA, K8B,
     +                ZI(JMA), ' ', 1 )
        CALL JEDETR(MESMAI)
   10 CONTINUE


C     2- MOT CLE "GRILLE" :
C     -----------------------------------------------------------
      CALL GETFAC ( 'GRILLE', NBOCC )
      DO 20 IOCC = 1,NBOCC
        CALL RELIEM(' ',NOMA,'NU_MAILLE','GRILLE',IOCC,2,MOCLES,
     &              TYPMCL,MESMAI ,NBMA)

        CALL GETVIS('GRILLE','GRILLE_NCOU',IOCC,IARG,1,NBCOU,NBV)
        ZK8(JNCMP-1+1) = 'GRI_NCOU'
        ZI(JVALV-1+1) =  NBCOU

        CALL JEVEUO (MESMAI , 'L', JMA )
        CALL NOCART ( CARTE, 3, K8B, 'NUM', NBMA, K8B,
     +                ZI(JMA), ' ', 1 )
        CALL JEDETR(MESMAI)
   20 CONTINUE


C     3- MOT CLE "POUTRE" :
C     -----------------------------------------------------------
      CALL GETFAC ( 'POUTRE', NBOCC )
      DO 30 IOCC = 1,NBOCC
        CALL RELIEM(' ',NOMA,'NU_MAILLE','POUTRE',IOCC,2,MOCLES,
     &              TYPMCL,MESMAI ,NBMA)

        CALL GETVIS('POUTRE','TUYAU_NCOU',IOCC,IARG,1,NBCOU,NBV)
        CALL GETVIS('POUTRE','TUYAU_NSEC',IOCC,IARG,1,NBSEC,NBV)
        ZK8(JNCMP-1+1) = 'TUY_NCOU'
        ZK8(JNCMP-1+2) = 'TUY_NSEC'
        ZI(JVALV-1+1) =  NBCOU
        ZI(JVALV-1+2) =  NBSEC

        CALL JEVEUO (MESMAI , 'L', JMA )
        CALL NOCART ( CARTE, 3, K8B, 'NUM', NBMA, K8B,
     +                ZI(JMA), ' ', 2 )
        CALL JEDETR(MESMAI)
   30 CONTINUE


C     4- MOT CLE "MEMBRANE" :
C     -----------------------------------------------------------
      CALL GETFAC ( 'MEMBRANE', NBOCC )
      DO 40 IOCC = 1,NBOCC
        CALL RELIEM(' ',NOMA,'NU_MAILLE','MEMBRANE',IOCC,2,MOCLES,
     &              TYPMCL,MESMAI ,NBMA)

        CALL GETVIS('MEMBRANE','MEMBRANE_NCOU',IOCC,IARG,1,NBCOU,NBV)
        ZK8(JNCMP-1+1) = 'GRI_NCOU'
        ZI(JVALV-1+1) =  NBCOU

        CALL JEVEUO (MESMAI , 'L', JMA )
        CALL NOCART ( CARTE, 3, K8B, 'NUM', NBMA, K8B,
     +                ZI(JMA), ' ', 1 )
        CALL JEDETR(MESMAI)
   40 CONTINUE


C     5- ON TRANSFORME LA CARTE EN CHAM_ELEM_S
C     -----------------------------------------------------------
      CALL CARCES(CARTE,'ELEM',' ','V',CESDEC,'A',IRET)
      CALL DETRSD('CARTE',CARTE)



9999  CONTINUE
      CALL JEDEMA()
      END
